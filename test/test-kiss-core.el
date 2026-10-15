;;; test-kiss-core.el --- hermetic invariants for KISS core -*- lexical-binding: t; -*-

;; These tests deliberately exercise parsed package state rather than an
;; Emacs-specific UI.  The properties translate directly to a CL/Scheme
;; implementation: canonical records, graph ordering, and filesystem layout.

(require 'ert)
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

(ert-deftest kiss-source/parser-classifies-and-canonicalizes-records ()
  "Source parsing preserves each source form's semantic fields."
  (let ((git (kiss--string-to-source-obj
              "git+https://example.invalid/project.git#v1.2 build"))
        (remote (kiss--string-to-source-obj
                 "https://example.invalid/archive.tar.xz dist"))
        (local (kiss--string-to-source-obj "files/config.h")))
    (should (eq (slot-value git :type) 'git))
    (should (string= (slot-value git :uri)
                     "https://example.invalid/project.git"))
    (should (string= (slot-value git :commit-or-branch) "v1.2"))
    (should (string= (slot-value git :extracted-path) "build"))
    ;; Canonical output uses @ for either supported git ref delimiter.
    (should (string= (kiss--source-to-string git)
                     "git+https://example.invalid/project.git@v1.2 build"))
    (should (eq (slot-value remote :type) 'remote))
    (should (string= (slot-value remote :extracted-path) "dist"))
    (should (eq (slot-value local :type) 'local))))

(ert-deftest kiss-source/file-reader-ignores-comment-and-blank-records ()
  "Only source payload records become source objects."
  (let ((file (make-temp-file "kiss-sources-")))
    (unwind-protect
        (progn
          (kiss-core-test-write file
                                "# explanatory comment\n\nhttps://example.invalid/a.tar.gz out\n")
          (let ((sources (kiss--sources-file-to-sources-objs file)))
            (should (= (length sources) 1))
            (should (eq (slot-value (car sources) :type) 'remote))))
      (ignore-errors (delete-file file)))))

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
                     "demo"))))

(ert-deftest kiss-download/utility-arguments-follow-selected-downloader ()
  "Downloader option lookup preserves the explicit environment mapping."
  (let ((kiss-get "curl")
        (kiss-get-alist '(("curl" . " -fL -o ") ("wget" . " -O "))))
    (should (string= (kiss--get-download-utility-arguments) " -fL -o "))))

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

(ert-deftest tsort/order-respects-all-edges-and-detects-cycles ()
  "Topological sorting orders dependencies first and rejects cyclic graphs."
  (let ((order (tsort '((app (lib tools)) (lib (base)) (tools (base)) (base nil)))))
    (should (< (seq-position order 'base) (seq-position order 'lib)))
    (should (< (seq-position order 'base) (seq-position order 'tools)))
    (should (< (seq-position order 'lib) (seq-position order 'app)))
    (should (< (seq-position order 'tools) (seq-position order 'app))))
  (should-not (tsort '((a (b)) (b (a)))))
  )

;;; test-kiss-core.el ends here
