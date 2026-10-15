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
             "#!/bin/sh -e\nmkdir -p \"$1/usr/bin\"\nprintf built > \"$1/usr/bin/demo\"\n" t)
            (should (kiss--package-build package))
            (let ((archive (concat kiss-bindir "demo@1-1.tar")))
              (should (file-exists-p archive))
              (kiss-extended-with-local-privileges
                (kiss-install archive))
              (should (kiss--pkg-is-installed-p "demo"))
              (should (string= (kiss--read-text (concat kiss-root "usr/bin/demo"))
                               "built"))))
        (ignore-errors (delete-directory recipe t))))))

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

;;; test-kiss-extended.el ends here
