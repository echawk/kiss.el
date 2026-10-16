;;; test-kiss-core.el --- hermetic invariants for KISS core -*- lexical-binding: t; -*-

;; These tests deliberately exercise parsed package state rather than an
;; Emacs-specific UI.  The properties translate directly to a CL/Scheme
;; implementation: canonical records, graph ordering, and filesystem layout.

(require 'ert)
(require 'cl-lib)
(require 'kiss)
(require 'kiss-test-fixture)

(defun kiss-core-test-with-repository (fn)
  "Call FN with a fresh repository directory and remove it afterwards."
  (let ((repo (file-name-as-directory (make-temp-file "kiss-repo-" t))))
    (unwind-protect
        (funcall fn repo)
      (ignore-errors (delete-directory repo t)))))

(defun kiss-core-test-write (path contents &optional executable)
  "Write CONTENTS to PATH, optionally making it executable."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert contents))
  (when executable (set-file-modes path #o755)))

(ert-deftest kiss-manifest/records-are-root-relative-and-safe ()
  "Generated manifest records are absolute, unique, and confined to root."
  (let ((dir (file-name-as-directory (make-temp-file "kiss-manifest-" t))))
    (unwind-protect
        (progn
          (kiss-core-test-write (concat dir "usr/bin/tool") "tool")
          (let ((manifest (kiss--get-manifest-for-dir dir)))
            (should (member "/usr/bin/tool" manifest))
            (should (seq-every-p (lambda (path)
                                   (and (string-prefix-p "/" path)
                                        (not (string-match-p "/\\.\\.?/" path))))
                                 manifest))
            (should (= (length manifest)
                       (length (seq-uniq (copy-sequence manifest)))))
            ;; The serialized representation is line-oriented and lossless.
            (should (equal manifest
                           (string-split (kiss--manifest-to-string manifest)
                                         "\n" t)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest kiss-manifest/records-resolve-only-under-kiss-root ()
  "Manifest removal paths are always rooted in KISS_ROOT, never the host root."
  (kiss-test-with-fixture
    (should (string= (kiss--manifest-record-to-root-path "/usr/bin/demo")
                     (concat kiss-root "usr/bin/demo")))
    (should-error (kiss--manifest-record-to-root-path "usr/bin/demo"))
    (should-error (kiss--manifest-record-to-root-path "/usr/../etc/passwd"))))

(ert-deftest kiss-source/parser-classifies-and-canonicalizes-records ()
  "Source parsing preserves each source form's semantic fields."
  (let ((git (kiss--string-to-source-obj
              "git+https://example.invalid/project.git#v1.2 build"))
        (remote (kiss--string-to-source-obj
                 "https://example.invalid/archive.tar.xz dist"))
        (local (kiss--string-to-source-obj "files/config.h")))
    (should (eq (plist-get git :type) 'git))
    (should (string= (plist-get git :uri)
                     "https://example.invalid/project.git"))
    (should (string= (plist-get git :commit-or-branch) "v1.2"))
    (should (string= (plist-get git :extracted-path) "build"))
    ;; Canonical output uses @ for either supported git ref delimiter.
    (should (string= (kiss--source-to-string git)
                     "git+https://example.invalid/project.git@v1.2 build"))
    (should (eq (plist-get remote :type) 'remote))
    (should (string= (plist-get remote :extracted-path) "dist"))
    (should (eq (plist-get local :type) 'local))))

(ert-deftest kiss-records/constructors-provide-total-portable-records ()
  "Portable records have stable defaults and reject malformed field input."
  (let ((source (kiss-source :uri "source.tar.xz"))
        (package (kiss-package :name "demo"))
        (build-env (kiss-build-env :proc-dir "/tmp/kiss/")))
    (should (equal (plist-get source :package) ""))
    (should (null (plist-get source :type)))
    (should (equal (plist-get source :extracted-path) ""))
    (should (equal (plist-get package :depends) '()))
    (should (equal (plist-get package :make-depends) '()))
    (should (equal (plist-get build-env :log-file) "")))
  (should-error (kiss-source :uri))
  (should-error (kiss-package :name))
  (should-error (kiss-build-env :proc-dir)))

(ert-deftest kiss-core/binary-file-classifier-is-pure-and-consistent ()
  "The compatibility classifier preserves the authoritative pure result."
  (let ((paths '("/usr/bin/demo" "/usr/sbin/admin" "/usr/lib/libdemo.so"
                 "/etc/demo.conf" "/usr/share/doc/readme")))
    (should (equal (kiss--get-potential-binary-files paths)
                   '("/usr/bin/demo" "/usr/sbin/admin" "/usr/lib/libdemo.so")))
    (should (equal (kiss--get-potential-binary-files-fast paths)
                   (kiss--get-potential-binary-files paths)))))

(ert-deftest kiss-source/file-reader-ignores-comment-and-blank-records ()
  "Only source payload records become source objects."
  (let ((file (make-temp-file "kiss-sources-")))
    (unwind-protect
        (progn
          (kiss-core-test-write file
                                "# explanatory comment\n\nhttps://example.invalid/a.tar.gz out\n")
          (let ((sources (kiss--sources-file-to-sources-objs file)))
            (should (= (length sources) 1))
            (should (eq (plist-get (car sources) :type) 'remote))))
      (ignore-errors (delete-file file)))))

(ert-deftest kiss-format/package-version-and-dependency-records-follow-port-grammar ()
  "Portable package metadata accepts only the documented field forms."
  (dolist (name '("a" "A9" "libfoo++" "foo.bar_baz-1"))
    (should (kiss--package-name-p name)))
  (dolist (name '("" "-leading" "with space" "slash/name" "name@version"
                  "name>choice" "name\nnext"))
    (should-not (kiss--package-name-p name)))
  (should (equal (kiss--package-parse-version-record "1.2.3 04")
                 '("1.2.3" "04")))
  (dolist (record '("" "1" "1 2 extra" "release@1 2" "release/name 2"
                    "1 one" "1 -1"))
    (should-error (kiss--package-parse-version-record record)))
  (should (equal (kiss--package-parse-dependency-record "builder make")
                 '("builder" . t)))
  (should (equal (kiss--package-parse-dependency-record "runtime")
                 '("runtime" . nil)))
  (dolist (record '("bad/name" "runtime optional" "runtime make extra"))
    (should-error (kiss--package-parse-dependency-record record))))

(ert-deftest kiss-format/source-records-preserve-destinations-and-reject-unsafe-paths ()
  "Source destination is the line remainder but must remain safely relative."
  (let ((source (kiss--string-to-source-obj
                 "git+https://example.invalid/project.git@v1.2 output/")))
    (should (equal (plist-get source :uri) "https://example.invalid/project.git"))
    (should (equal (plist-get source :commit-or-branch) "v1.2"))
    (should (equal (plist-get source :extracted-path) "output")))
  (should (equal (plist-get (kiss--string-to-source-obj
                             "https://example.invalid/a.tar.gz output directory")
                            :extracted-path)
                 "output directory"))
  (dolist (record '("" "https://example.invalid/a.tar.gz /outside"
                    "https://example.invalid/a.tar.gz ../outside"
                    "git+https://example.invalid/project.git@"))
    (should-error (kiss--string-to-source-obj record))))

(ert-deftest kiss-format/recipe-reader-fails-before-returning-invalid-metadata ()
  "Malformed recipe fields are rejected at the repository metadata boundary."
  (let ((root (file-name-as-directory (make-temp-file "kiss-invalid-recipe-" t))))
    (unwind-protect
        (let ((recipe (concat root "demo/")))
          (kiss-core-test-write (concat recipe "build") "#!/bin/sh -e\n" t)
          (kiss-core-test-write (concat recipe "version") "1 not-a-release\n")
          (should-error (kiss--dir-to-kiss-package recipe))
          (kiss-core-test-write (concat recipe "version") "1 1\n")
          (kiss-core-test-write (concat recipe "depends") "runtime optional\n")
          (should-error (kiss--dir-to-kiss-package recipe)))
      (ignore-errors (delete-directory root t)))))

(ert-deftest kiss-package/dependency-order-precedes-dependents ()
  "Every dependency occurs before its dependent in the computed build order."
  (kiss-test-with-fixture
    (kiss-test-make-pkg "leaf" "1" "1")
    (kiss-test-make-pkg "middle" "1" "1" :depends '("leaf"))
    (kiss-test-make-pkg "root" "1" "1" :depends '("middle"))
    (let ((order (kiss--package-get-dependency-order '("root") t)))
      (should (< (seq-position order "leaf") (seq-position order "middle")))
      (should (< (seq-position order "middle") (seq-position order "root"))))))

(ert-deftest kiss-package/dependency-reader-removes-comments-and-make-marker ()
  "Dependency resolution uses the first field of noncomment records."
  (let ((file (make-temp-file "kiss-depends-")))
    (unwind-protect
        (progn
          (kiss-core-test-write file "# note\nruntime\nbuilder make\n\n")
          (should (equal (kiss--get-dependencies-from-file file)
                         '("runtime" "builder"))))
      (ignore-errors (delete-file file)))))

(ert-deftest kiss-env/random-number-is-bounded-and-silent ()
  "Temporary-name entropy is an integer in the requested half-open interval."
  (let ((inhibit-message nil)
        (message-log-max t)
        (message-log-max-before message-log-max))
    (dotimes (_ 20)
      (let ((value (kiss--get-random-number 7)))
        (should (integerp value))
        (should (<= 0 value))
        (should (< value 7))))
    (should (equal message-log-max message-log-max-before))))

(ert-deftest kiss-install/package-name-is-read-from-installed-manifest-path ()
  "The embedded DB manifest path determines a package archive's identity."
  (kiss-test-with-fixture
    (should (string= (kiss--package-get-from-manifest
                      (list "/usr/bin/tool"
                            kiss-installed-db-dir
                            (concat kiss-installed-db-dir "demo/manifest")))
                     "demo"))
    ;; Archive records are root-relative, even when KISS_ROOT is not '/'.
    (should (string= (kiss--package-get-from-manifest
                      '("/usr/bin/tool" "/var/db/kiss/installed/demo/manifest"))
                     "demo"))))

(ert-deftest kiss-download/utility-arguments-follow-selected-downloader ()
  "Downloader option lookup preserves the explicit environment mapping."
  (let ((kiss-get "curl")
        (kiss-get-alist '(("curl" . " -fL -o ") ("wget" . " -O "))))
    (should (string= (kiss--get-download-utility-arguments) " -fL -o "))))

(ert-deftest kiss-download/package-object-assigns-source-package-before-download ()
  "Every source download sees the owning package name, including local sources."
  (let* ((source (kiss-source :type 'local :uri "file"))
         (package (kiss-package :name "demo" :sources (list source))))
    (cl-letf (((symbol-function 'kiss--source-download)
               (lambda (object)
                 (string= (plist-get object :package) "demo"))))
      (should (kiss--download-pkg-obj package)))))

(ert-deftest kiss-download/list-result-requires-every-package-to-succeed ()
  "A list download succeeds exactly when each individual download succeeds."
  (let (seen)
    (cl-letf (((symbol-function 'kiss--search-pkg-obj)
               (lambda (name) (kiss-package :name name)))
              ((symbol-function 'kiss--download-pkg-obj)
               (lambda (package)
                 (push (plist-get package :name) seen)
                 (not (string= (plist-get package :name) "bad")))))
      (should (kiss-download '("one" "two")))
      (should (equal (nreverse seen) '("one" "two")))
      (setq seen nil)
      (should-not (kiss-download '("one" "bad"))))))

(ert-deftest kiss-build/generated-script-keeps-build-contract ()
  "The generated script calls BUILD with DESTDIR and VERSION in that order."
  (let ((dir (file-name-as-directory (make-temp-file "kiss-build-" t))))
    (unwind-protect
        (let ((script (concat dir "runner"))
              (build (concat dir "build"))
              (work (concat dir "work/"))
              (dest (concat dir "dest/"))
              (log (concat dir "build.log")))
          (kiss--build-make-script script build work dest "2.3.4" log)
          (should (file-executable-p script))
          (let ((contents (kiss--read-text script)))
            (should (string-match-p (regexp-quote (concat build " " dest " 2.3.4"))
                                    contents))
            (should (string-match-p "export DESTDIR=" contents))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest kiss-hook/runs-each-executable-hook-with-contract-arguments ()
  "Global hooks receive hook name followed by the three documented arguments."
  (let* ((dir (file-name-as-directory (make-temp-file "kiss-hook-" t)))
         (hook (concat dir "capture"))
         (result (concat dir "result")))
    (unwind-protect
        (progn
          (kiss-core-test-write
           hook
           (concat "#!/bin/sh\nprintf '%s|%s|%s|%s' \"$1\" \"$2\" \"$3\" \"$4\" > '"
                   result "'\n")
           t)
          (let ((kiss-hook (list hook)))
            (kiss--run-hook "post-build" "demo" "/stage" "extra"))
          (should (string= (kiss--read-text result)
                           "post-build|demo|/stage|extra")))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest kiss-hook/failure-is-fatal-except-for-build-fail ()
  "Hook errors abort normal operations but do not mask an existing build error."
  (let* ((dir (file-name-as-directory (make-temp-file "kiss-hook-" t)))
         (hook (concat dir "fail")))
    (unwind-protect
        (progn
          (kiss-core-test-write hook "#!/bin/sh\nexit 23\n" t)
          (let ((kiss-hook (list hook)))
            (should-error (kiss--run-hook "post-install" "demo"))
            (should-not (kiss--run-hook "build-fail" "demo"))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest kiss-os/current-user-round-trips-through-uid ()
  "The OS adapter maps the running user's name and uid consistently."
  (let ((name (user-login-name))
        (uid (user-real-uid)))
    (should (= (kiss--get-uid-from-user name) uid))
    (should (string= (kiss--get-user-from-uid uid) name))))

(ert-deftest kiss-update/dispatches-only-enabled-repository-backends ()
  "Update dispatch is governed solely by the detected feature set."
  (let ((calls '())
        (kiss-features '(:GIT :FOSSIL)))
    (cl-letf (((symbol-function 'kiss--update-git-repos)
               (lambda () (push 'git calls)))
              ((symbol-function 'kiss--update-hg-repos)
               (lambda () (push 'hg calls)))
              ((symbol-function 'kiss--update-fossil-repos)
               (lambda () (push 'fossil calls))))
      (kiss-update)
      (should (equal (nreverse calls) '(git fossil))))))

(ert-deftest elkiss/unknown-command-exits-successfully ()
  "The CLI wrapper can locate its library without a caller-managed load path."
  (should (= 0 (call-process (expand-file-name "elkiss" default-directory)
                             nil nil nil "not-a-kiss-command"))))

(ert-deftest elkiss/version-is-a-stable-public-command ()
  "The library and CLI expose the implementation and compatibility versions."
  (should (string-match-p (regexp-quote kiss-version) (kiss-version)))
  (should (string-match-p (regexp-quote kiss-compat-version) (kiss-version)))
  (with-temp-buffer
    (should (= 0 (call-process (expand-file-name "elkiss" default-directory)
                               nil t nil "version")))
    (should (string-match-p (regexp-quote kiss-version) (buffer-string)))))

(ert-deftest kiss-os/shell-commands-stops-on-the-first-failure ()
  "The command adapter returns a boolean and does not run trailing commands."
  (let (seen)
    (cl-letf (((symbol-function 'call-process-shell-command)
               (lambda (command &rest _)
                 (push command seen)
                 (if (string= command "bad") 1 0))))
      (should-not (kiss--shell-commands '("first" "bad" "later")))
      (should (equal (nreverse seen) '("first" "bad"))))))

(ert-deftest kiss-os/silent-shell-command-discards-subprocess-output ()
  "Status-only subprocesses leave the caller's output buffer untouched."
  (with-temp-buffer
    (insert "sentinel")
    (should (zerop (kiss--silent-shell-command "printf subprocess-output")))
    (should (equal (buffer-string) "sentinel"))))

(ert-deftest kiss-build/queue-status-reports-deterministic-progress ()
  "Building a package list reports one queue hook per dependency-order entry."
  (let (hooks built)
    (cl-letf (((symbol-function 'kiss-download) (lambda (_packages) t))
              ((symbol-function 'kiss--package-get-order)
               (lambda (_packages) '("base" "app")))
              ((symbol-function 'kiss--search-pkg-obj)
               (lambda (name) (kiss-package :name name)))
              ((symbol-function 'kiss--package-build)
               (lambda (package) (push (plist-get package :name) built)))
              ((symbol-function 'kiss--run-hook)
               (lambda (&rest args) (push args hooks))))
      (kiss-build '("app"))
      (should (equal (nreverse built) '("base" "app")))
      (should (equal (nreverse hooks)
                     '(("queue-status" "base" 1 2)
                       ("queue-status" "app" 2 2)))))))

(ert-deftest kiss-alternatives/lists-package-and-absolute-path ()
  "A choice filename maps reversibly to its package and selected path."
  (kiss-test-with-fixture
    (let ((choice (concat kiss-choices-db-dir "demo>usr>bin>tool")))
      (kiss-core-test-write choice "alternative")
      (should (equal (kiss-alternatives)
                     '(("demo" "/usr/bin/tool" "demo>usr>bin>tool")))))))

(ert-deftest kiss-preferred/returns-owner-for-a-selected-alternative ()
  "Preferred alternatives are resolved from choice names and package manifests."
  (kiss-test-with-fixture
    (kiss-test-make-pkg "demo" "1" "1" :manifest '("/usr/bin/tool"))
    (kiss-core-test-write (concat kiss-choices-db-dir "demo>usr>bin>tool") "alt")
    (should (equal (kiss-preferred) '(("demo" "/usr/bin/tool"))))))

(ert-deftest kiss-core/redundant-dependencies-are-eliminated ()
  "A package implied by another requested package need not be built twice."
  (cl-letf (((symbol-function 'kiss--package-get-dependencies)
             (lambda (pkg) (pcase pkg
                             ("app" '("lib"))
                             ("lib" '("base"))
                             (_ nil)))))
    (should (equal (kiss--remove-redundant-dependencies '("app" "lib" "base"))
                   '("app")))))

(ert-deftest kiss-search/repository-precedes-installed-database ()
  "Search retains KISS_PATH precedence while retaining the DB fallback."
  (kiss-test-with-fixture
    (kiss-core-test-with-repository
     (lambda (repo)
       (make-directory (concat repo "demo") t)
       (kiss-test-make-pkg "demo" "1" "1")
       (let ((kiss-path (list repo)))
         (should (equal (kiss-search "demo")
                        (list (concat repo "demo")
                              (concat kiss-installed-db-dir "demo")))))))))

(ert-deftest kiss-upgrade/uses-opaque-version-inequality ()
  "Upgrade selection treats version fields as strings, not numeric versions."
  (kiss-test-with-fixture
    (kiss-test-make-pkg "demo" "1.10" "1")
    (kiss-core-test-with-repository
     (lambda (repo)
       (kiss-core-test-write (concat repo "demo/version") "1.2 1\n")
       (let ((kiss-path (list repo)))
         (should (equal (kiss--get-out-of-date-pkgs) '("demo"))))))))

(ert-deftest tsort/order-respects-all-edges-and-detects-cycles ()
  "The declared tsort dependency orders dependencies and rejects cycles."
  (let ((order (tsort '((app (lib tools)) (lib (base)) (tools (base)) (base nil)))))
    (should (< (seq-position order 'base) (seq-position order 'lib)))
    (should (< (seq-position order 'base) (seq-position order 'tools)))
    (should (< (seq-position order 'lib) (seq-position order 'app)))
    (should (< (seq-position order 'tools) (seq-position order 'app))))
  (should-not (tsort '((a (b)) (b (a)))))
  )

;;; test-kiss-core.el ends here
