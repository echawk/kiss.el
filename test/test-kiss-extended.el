;;; test-kiss-extended.el --- broader hermetic KISS conformance tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'kiss)
(require 'kiss-test-fixture)

(defun kiss-extended--write (path contents &optional executable)
  "Write CONTENTS to PATH, optionally making it executable."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert contents))
  (when executable (set-file-modes path #o755)))

(defmacro kiss-extended-with-local-privileges (&rest body)
  "Execute privilege-adapter commands locally inside a temporary fixture."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'kiss--shell-command-as-user)
              (lambda (command _user) (shell-command command))))
     ,@body))

(ert-deftest kiss-package/recipe-roundtrip-preserves-canonical-fields ()
  "Writing and reading a recipe preserves all portable package fields."
  (let ((root (file-name-as-directory (make-temp-file "kiss-recipe-" t))))
    (unwind-protect
        (let* ((recipe (concat root "recipe/"))
               (build (concat recipe "build"))
               (local "files/config")
               (source (kiss-source :type 'local :uri local :checksum "sum"))
               (package (kiss-package :name "demo" :version "1.2" :release "3"
                                      :build-file build :depends '("runtime")
                                      :make-depends '("builder") :sources (list source))))
          (kiss-extended--write build "#!/bin/sh -e\n" t)
          (kiss-extended--write (concat recipe local) "configuration\n")
          (kiss--package-to-dir package root)
          (let* ((read (kiss--dir-to-kiss-package (concat root "demo")))
                 (read-source (car (plist-get read :sources))))
            (should (equal (plist-get read :name) "demo"))
            (should (equal (plist-get read :version) "1.2"))
            (should (equal (plist-get read :release) "3"))
            (should (equal (plist-get read :depends) '("runtime")))
            (should (equal (plist-get read :make-depends) '("builder")))
            (should (equal (plist-get read-source :uri) local))
            (should (equal (plist-get read-source :checksum) "sum"))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest kiss-property/source-record-roundtrips-for-supported-kinds ()
  "Parsing canonical source records preserves their portable semantic fields."
  (dolist (record '("files/local"
                    "https://example.invalid/source.tar.xz output"
                    "git+https://example.invalid/repo.git@v1 source"
                    "hg+https://example.invalid/repo@tip source"
                    "fossil+https://example.invalid/repo@trunk source"))
    (let* ((first (kiss--string-to-source-obj record))
           (second (kiss--string-to-source-obj (kiss--source-to-string first))))
      (dolist (field '(:type :uri :extracted-path :commit-or-branch))
        (should (equal (plist-get first field) (plist-get second field)))))))

(ert-deftest kiss-source/local-cache-validation-and-extraction-are-hermetic ()
  "Local sources resolve under KISS_PATH, validate, and extract without I/O outside it."
  (skip-unless (executable-find "b3sum"))
  (kiss-test-with-fixture
    (let* ((repo (file-name-as-directory (make-temp-file "kiss-source-repo-" t)))
           (recipe (concat repo "demo/"))
           (local "files/input")
           (source (kiss-source :type 'local :package "demo" :uri local))
           (package (kiss-package :name "demo" :sources (list source)))
           (destination (concat kiss-tmpdir "extract/")))
      (unwind-protect
          (progn
            (kiss-extended--write (concat recipe local) "source-data\n")
            (let ((kiss-path (list repo)))
              (should (kiss--source-download source))
              (setf (plist-get source :checksum) (kiss--source-get-local-checksum source))
              (should (kiss--source-validate-p source))
              (kiss--package-extract-sources package destination)
              (should (string=
                       (kiss--read-text (concat destination "/input"))
                       "source-data\n"))))
        (ignore-errors (delete-directory repo t))))))

(ert-deftest kiss-source/vcs-command-failure-is-not-a-success ()
  "A failed VCS command yields nil rather than a truthy nonzero status."
  (kiss-test-with-fixture
    (let ((source (kiss-source :type 'hg :package "demo"
                               :uri "https://example.invalid/demo")))
      (cl-letf (((symbol-function 'shell-command) (lambda (_command) 1)))
        (should-not (kiss--source-download source))))))

(ert-deftest kiss-source/local-transfer-runs-pre-and-post-source-hooks ()
  "Successful source acquisition emits both documented source hook events."
  (kiss-test-with-fixture
    (let* ((repo (file-name-as-directory (make-temp-file "kiss-hook-repo-" t)))
           (recipe (concat repo "demo/"))
           (source (kiss-source :type 'local :package "demo" :uri "source"))
           calls)
      (unwind-protect
          (progn
            (kiss-extended--write (concat recipe "source") "data")
            (let ((kiss-path (list repo)))
              (cl-letf (((symbol-function 'kiss--run-hook)
                         (lambda (&rest args) (push args calls))))
                (should (kiss--source-download source)))
              (should (equal (mapcar #'car (nreverse calls))
                             '("pre-source" "post-source")))))
        (ignore-errors (delete-directory repo t))))))

(ert-deftest kiss-file/archive-roundtrip-preserves-staged-contents ()
  "Tarball creation followed by extraction keeps file data beneath its stage."
  (let ((stage (file-name-as-directory (make-temp-file "kiss-archive-stage-" t)))
        (out (file-name-as-directory (make-temp-file "kiss-archive-out-" t)))
        (tarball (make-temp-file "kiss-archive-" nil ".tar")))
    (unwind-protect
        (progn
          (kiss-extended--write (concat stage "source/payload") "archive-data\n")
          (let ((kiss-compress ""))
            (should (kiss--file-make-tarball-of-dir stage tarball))
            (kiss-extended-with-local-privileges
              (kiss--file-extract-tarball tarball out))
            (should (string= (kiss--read-text (concat out "payload"))
                             "archive-data\n"))))
      (ignore-errors (delete-directory stage t))
      (ignore-errors (delete-directory out t))
      (ignore-errors (delete-file tarball)))))

(ert-deftest kiss-hook/package-hook-receives-isolated-kiss-root ()
  "A package hook sees fixture KISS_ROOT and executes through the adapter."
  (kiss-test-with-fixture
    (let ((result (concat kiss-tmpdir "hook-result")))
      (kiss-test-make-pkg "demo" "1" "1")
      (kiss-extended--write
       (concat kiss-installed-db-dir "demo/post-install")
       (format "#!/bin/sh\nprintf '%%s' \"$KISS_ROOT\" > '%s'\n" result) t)
      (kiss-extended-with-local-privileges
        (kiss--run-hook-pkg "post-install" "demo"))
      (should (string= (kiss--read-text result) kiss-root)))))

(ert-deftest kiss-public/list-owns-fork-and-install-filter-use-fixture-state ()
  "Public read-only operations and install filtering never inspect host state."
  (kiss-test-with-fixture
    (let ((repo (file-name-as-directory (make-temp-file "kiss-fork-repo-" t)))
          (target (file-name-as-directory (make-temp-file "kiss-fork-out-" t)))
          installed)
      (unwind-protect
          (progn
            (kiss-test-make-pkg "demo" "1" "1" :files '(("usr/bin/demo" . "payload")))
            (kiss-extended--write (concat repo "demo/version") "2 1\n")
            (let ((kiss-path (list repo)))
              (should (equal (kiss-list "demo") '("demo" "1 1")))
              (should (equal (kiss-owns "/usr/bin/demo") "demo"))
              (should (kiss-fork "demo" target))
              (should (file-exists-p (concat target "demo/version")))
              (cl-letf (((symbol-function 'kiss-install)
                         (lambda (names) (setq installed names))))
                (kiss--install-if-not-installed '("demo" "new")))
              (should (equal installed '("new")))))
        (ignore-errors (delete-directory repo t))
        (ignore-errors (delete-directory target t))))))

(ert-deftest kiss-build/hermetic-build-install-roundtrip ()
  "A no-source recipe builds an archive that the installer accepts in the fixture."
  (kiss-test-with-fixture
    (let* ((recipe (file-name-as-directory (make-temp-file "kiss-build-recipe-" t)))
           (build (concat recipe "build"))
           (package (kiss-package :name "demo" :version "1" :release "1"
                                  :build-file build))
           (kiss-force t)
           (kiss-hook nil)
           (kiss-strip 0)
           (kiss-compress "")
           (kiss-perfom-build-in-sandbox nil))
      (unwind-protect
          (progn
            (kiss-extended--write
             build
             (concat "#!/bin/sh -e\n"
                     "mkdir -p \"$1/usr/bin\" \"$1/usr/share\"\n"
                     "printf built > \"$1/usr/bin/demo\"\n"
                     "printf '%s' \"$KISS_ROOT\" > \"$1/usr/share/kiss-root\"\n") t)
            (should (kiss--package-build package))
            (let ((archive (concat kiss-bindir "demo@1-1.tar")))
              (should (file-exists-p archive))
              (kiss-extended-with-local-privileges
                (kiss-install archive))
              (should (kiss--pkg-is-installed-p "demo"))
              (should (string= (kiss--read-text (concat kiss-root "usr/bin/demo"))
                               "built"))
              (should (string= (kiss--read-text (concat kiss-root "usr/share/kiss-root"))
                               kiss-root))))
        (ignore-errors (delete-directory recipe t))))))

(ert-deftest kiss-build/discovered-library-owners-persist-in-runtime-depends ()
  "A build writes each newly discovered library owner to its package record."
  (kiss-test-with-fixture
    (let* ((recipe (file-name-as-directory (make-temp-file "kiss-build-recipe-" t)))
           (build (concat recipe "build"))
           (package (kiss-package :name "demo" :version "1" :release "1"
                                  :build-file build :depends '("runtime")))
           (kiss-force t)
           (kiss-hook nil)
           (kiss-strip 0)
           (kiss-compress "")
           (kiss-perfom-build-in-sandbox nil))
      (unwind-protect
          (progn
            (kiss-extended--write
             build
             (concat "#!/bin/sh -e\n"
                     "mkdir -p \"$1/usr/bin\"\n"
                     "printf built > \"$1/usr/bin/demo\"\n") t)
            ;; The controlled result includes a declared dependency, duplicate,
            ;; self-reference, and invalid owners.  Only the new valid owner
            ;; belongs in the generated runtime dependency record.
            (cl-letf (((symbol-function 'kiss--get-potential-binary-files)
                       (lambda (_manifest) '("/usr/bin/demo")))
                      ((symbol-function 'kiss--build-get-missing-dependencies)
                       (lambda (_dir files pkg)
                         (should (equal files '("/usr/bin/demo")))
                         (should (string= (plist-get pkg :name) "demo"))
                         '("runtime" "libdemo" "demo" nil "" "libdemo"))))
              (should (kiss--package-build package)))
            (should (equal (plist-get package :depends)
                           '("runtime" "libdemo")))
            (kiss-extended-with-local-privileges
              (kiss-install (concat kiss-bindir "demo@1-1.tar")))
            (should (equal (plist-get
                            (kiss--dir-to-kiss-package
                             (concat kiss-installed-db-dir "demo"))
                            :depends)
                           '("libdemo" "runtime"))))
        (ignore-errors (delete-directory recipe t))))))

(ert-deftest kiss-build/merge-discovered-dependencies-is-idempotent ()
  "Repeated library scans retain one canonical runtime dependency per owner."
  (let ((package (kiss-package :name "demo" :depends '("runtime"))))
    (should (equal (kiss--build-merge-discovered-dependencies
                    package '("runtime" "libdemo" "demo" "libdemo" nil ""))
                   '("runtime" "libdemo")))))

(ert-deftest kiss-build/dynamic-library-discovery-honors-redirected-root ()
  "A locally linked library resolves to its fixture package owner on GNU/Linux."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (executable-find "cc") (executable-find "ldd")
                    (executable-find "realpath")))
  (kiss-test-with-fixture
    (let* ((compiler (executable-find "cc"))
           (library (concat kiss-root "usr/lib/libkissfixture.so"))
           (app (concat kiss-root "usr/bin/kiss-fixture-app"))
           (source (concat kiss-tmpdir "fixture.c"))
           (app-source (concat kiss-tmpdir "app.c"))
           (wrapper-dir (concat kiss-tmpdir "bin/"))
           (wrapper (concat wrapper-dir "cc"))
           (package (kiss-package :name "consumer" :depends '())))
      (kiss-test-make-pkg "libowner" "1" "1"
                          :files '(("usr/lib/libkissfixture.so" . "placeholder")))
      (kiss-extended--write source "int fixture(void) { return 0; }\n")
      (kiss-extended--write app-source "int fixture(void); int main(void) { return fixture(); }\n")
      (should (zerop (call-process compiler nil nil nil "-shared" "-fPIC"
                                   "-o" library source)))
      (should (zerop (call-process compiler nil nil nil app-source "-L"
                                   (concat kiss-root "usr/lib")
                                   (concat "-Wl,-rpath," kiss-root "usr/lib")
                                   "-lkissfixture" "-o" app)))
      (kiss-extended--write
       wrapper
       (format "#!/bin/sh\ncase \"$1\" in\n  -print-file-name=libkissfixture.so) printf '%%s\\n' %s ;;\n  *) exec %s \"$@\" ;;\nesac\n"
               (shell-quote-argument library) (shell-quote-argument compiler)) t)
      (let ((process-environment (copy-sequence process-environment)))
        (setenv "PATH" (concat wrapper-dir ":" (getenv "PATH")))
        (should (equal (kiss--build-get-missing-dependencies
                        kiss-root '("/usr/bin/kiss-fixture-app") package)
                       '("libowner")))))))

(ert-deftest kiss-update/git-toplevel-is-local-and-deterministic ()
  "Git repository discovery works against a local disposable repository."
  (skip-unless (executable-find "git"))
  (let ((repo (file-name-as-directory (make-temp-file "kiss-git-repo-" t))))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil "-C" repo "init" "-q")))
          (should (string= (directory-file-name
                            (file-truename (kiss--get-git-dir-toplevel repo)))
                           (directory-file-name (file-truename repo)))))
      (ignore-errors (delete-directory repo t)))))

(ert-deftest kiss-update/git-pulls-from-a-local-bare-upstream ()
  "Updating a working clone fetches a new commit from a local bare remote."
  (skip-unless (executable-find "git"))
  (let* ((root (file-name-as-directory (make-temp-file "kiss-git-update-" t)))
         (remote (concat root "remote.git"))
         (producer (concat root "producer"))
         (checkout (concat root "checkout")))
    (unwind-protect
        (progn
          (should (zerop (call-process "git" nil nil nil "init" "--bare" "-q" remote)))
          (should (zerop (call-process "git" nil nil nil "clone" "-q" remote producer)))
          ;; Repository-local identity avoids relying on a caller's Git config.
          (should (zerop (call-process "git" nil nil nil "-C" producer "config"
                                       "user.email" "kiss-test@example.invalid")))
          (should (zerop (call-process "git" nil nil nil "-C" producer "config"
                                       "user.name" "KISS test")))
          (kiss-extended--write (concat producer "/marker") "initial\n")
          (should (zerop (call-process "git" nil nil nil "-C" producer "add" "marker")))
          (should (zerop (call-process "git" nil nil nil "-C" producer "commit" "-qm" "initial")))
          (should (zerop (call-process "git" nil nil nil "-C" producer "push" "-q"
                                       "origin" "HEAD")))
          (should (zerop (call-process "git" nil nil nil "clone" "-q" remote checkout)))
          (kiss-extended--write (concat producer "/marker") "updated\n")
          (should (zerop (call-process "git" nil nil nil "-C" producer "commit" "-am" "update")))
          (should (zerop (call-process "git" nil nil nil "-C" producer "push" "-q"
                                       "origin" "HEAD")))
          (let ((kiss-path (list checkout))
                (kiss-hook nil))
            (should (kiss--update-git-repos)))
          (should (string= (kiss--read-text (concat checkout "/marker"))
                           "updated\n")))
      (ignore-errors (delete-directory root t)))))

(ert-deftest kiss-update/git-commands-and-hooks-follow-successful-update ()
  "A successful update selects Git commands and emits pre/post hooks in order."
  (kiss-test-with-fixture
    (let ((repo (concat kiss-tmpdir "repo/"))
          commands hooks)
      (make-directory repo t)
      (cl-letf (((symbol-function 'kiss--kiss-path-git-repos)
                 (lambda () (list repo)))
                ((symbol-function 'kiss--get-git-dir-toplevel)
                 (lambda (directory) directory))
                ((symbol-function 'kiss--file-get-owner-name)
                 (lambda (_directory) "fixture-owner"))
                ((symbol-function 'kiss--file-am-owner-p)
                 (lambda (_directory) t))
                ((symbol-function 'kiss--run-hook)
                 (lambda (&rest args) (push args hooks)))
                ((symbol-function 'shell-command)
                 (lambda (command) (push command commands) 0)))
        (should (kiss--update-git-repos)))
      (should (equal (nreverse commands)
                     '("git pull" "git submodule update --remote --init -f")))
      (should (equal (mapcar #'car (nreverse hooks))
                     '("pre-update" "post-update"))))))

(ert-deftest kiss-update/failed-pull-is-reported-and-skips-post-update ()
  "A failed pull returns nil and never runs the update command or post hook."
  (kiss-test-with-fixture
    (let ((repo (concat kiss-tmpdir "repo/"))
          commands hooks)
      (make-directory repo t)
      (cl-letf (((symbol-function 'kiss--kiss-path-git-repos)
                 (lambda () (list repo)))
                ((symbol-function 'kiss--get-git-dir-toplevel)
                 (lambda (directory) directory))
                ((symbol-function 'kiss--file-get-owner-name)
                 (lambda (_directory) "fixture-owner"))
                ((symbol-function 'kiss--file-am-owner-p)
                 (lambda (_directory) t))
                ((symbol-function 'kiss--run-hook)
                 (lambda (&rest args) (push args hooks)))
                ((symbol-function 'shell-command)
                 (lambda (command) (push command commands) 1)))
        (should-not (kiss--update-git-repos)))
      (should (equal commands '("git pull")))
      (should (equal (mapcar #'car (nreverse hooks))
                     '("pre-update"))))))

;;; test-kiss-extended.el ends here
