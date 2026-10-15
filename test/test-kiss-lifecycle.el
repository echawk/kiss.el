;;; test-kiss-lifecycle.el --- hermetic KISS state-transition tests -*- lexical-binding: t; -*-
;;
;; These tests use real tar archives and the public install/remove operations,
;; but redirect every KISS path to `kiss-test-with-fixture'.  They deliberately
;; inspect filesystem state, because that state is the KISS package database.

(require 'ert)
(require 'cl-lib)
(require 'kiss)
(require 'kiss-test-fixture)

(defun kiss-lifecycle--write (path contents &optional executable)
  "Write CONTENTS to PATH, optionally marking it executable."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert contents))
  (when executable (set-file-modes path #o755)))

(defun kiss-lifecycle--archive (name version release files &optional dependencies)
  "Create an uncompressed KISS archive for NAME and return its file name.
FILES is an alist of root-relative regular-file names and their contents.
DEPENDENCIES uses the canonical dependency-record names.  The generated
manifest is produced by the implementation under test, which lets the archive
exercise the same manifest contract as the build pipeline."
  (let ((stage (file-name-as-directory (make-temp-file "kiss-stage-" t)))
        (archive (make-temp-file (concat "kiss-" name "-") nil ".tar")))
    (dolist (entry files)
      (kiss-lifecycle--write (concat stage (car entry)) (cdr entry)))
    (let ((db (concat stage "var/db/kiss/installed/" name "/")))
      (kiss-lifecycle--write (concat db "build") "#!/bin/sh -e\n" t)
      (kiss-lifecycle--write (concat db "version")
                              (format "%s %s\n" version release))
      (when dependencies
        (kiss-lifecycle--write (concat db "depends")
                                (concat (string-join dependencies "\n") "\n")))
      (kiss-lifecycle--write
       (concat db "manifest")
       (kiss--manifest-to-string (kiss--get-manifest-for-dir stage))))
    (unwind-protect
        (progn
          (should (= 0 (call-process "tar" nil nil nil "-cf" archive "-C" stage ".")))
          archive)
      (ignore-errors (delete-directory stage t)))))

(defmacro kiss-lifecycle-with-unprivileged-shell (&rest body)
  "Run BODY with the privilege adapter executing directly in the fixture."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'kiss--shell-command-as-user)
              (lambda (command _user) (shell-command command))))
     ,@body))

(ert-deftest kiss-lifecycle/install-persists-payload-and-authoritative-db ()
  "Installing a valid archive installs its payload and its package database."
  (kiss-test-with-fixture
    (let ((archive (kiss-lifecycle--archive
                    "demo" "1.0" "1"
                    '(("usr/bin/demo" . "one\n")
                      ("usr/share/demo/readme" . "documentation\n")))))
      (unwind-protect
          (let ((kiss-force nil) (kiss-hook nil) (kiss-choice 1))
            (kiss-lifecycle-with-unprivileged-shell
              (kiss--install-tarball archive))
            (should (kiss--pkg-is-installed-p "demo"))
            (should (string= (kiss--read-text (concat kiss-root "usr/bin/demo")) "one\n"))
            (should (seq-every-p #'file-exists-p
                                  (mapcar #'kiss--manifest-record-to-root-path
                                          (kiss-manifest "demo"))))
            (should (member "/usr/share/demo/readme" (kiss-manifest "demo"))))
        (ignore-errors (delete-file archive))))))

(ert-deftest kiss-lifecycle/temporary-workspaces-are-rooted-and-distinct ()
  "Installer workspaces are strings beneath KISS_TMPDIR and never collide."
  (kiss-test-with-fixture
    (let ((paths (mapcar (lambda (_) (kiss--get-tmp-destdir)) (number-sequence 1 12))))
      (should (= (length paths) (length (seq-uniq paths))))
      (dolist (path paths)
        (should (string-prefix-p kiss-tmpdir path))
        (should (file-directory-p path))))))

(ert-deftest kiss-lifecycle/install-rejects-missing-dependency-before-mutation ()
  "Dependency validation rejects an archive before it writes payload state."
  (kiss-test-with-fixture
    (let ((archive (kiss-lifecycle--archive
                    "dependent" "1" "1" '(("usr/bin/dependent" . "payload"))
                    '("required"))))
      (unwind-protect
          (let ((kiss-force nil) (kiss-hook nil))
            (kiss-lifecycle-with-unprivileged-shell
              (should-error (kiss--install-tarball archive)))
            (should-not (kiss--pkg-is-installed-p "dependent"))
            (should-not (file-exists-p (concat kiss-root "usr/bin/dependent"))))
        (ignore-errors (delete-file archive))))))

(ert-deftest kiss-lifecycle/conflict-choice-zero-refuses-before-live-replacement ()
  "A conflicting archive fails under KISS_CHOICE=0 without changing its provider."
  (kiss-test-with-fixture
    (kiss-test-make-pkg "provider" "1" "1" :files '(("usr/bin/tool" . "original\n")))
    (let ((archive (kiss-lifecycle--archive
                    "challenger" "1" "1" '(("usr/bin/tool" . "replacement\n")))))
      (unwind-protect
          (let ((kiss-force nil) (kiss-hook nil) (kiss-choice 0))
            (kiss-lifecycle-with-unprivileged-shell
              (should-error (kiss--install-tarball archive)))
            (should (kiss--pkg-is-installed-p "provider"))
            (should-not (kiss--pkg-is-installed-p "challenger"))
            (should (string= (kiss--read-text (concat kiss-root "usr/bin/tool"))
                             "original\n")))
        (ignore-errors (delete-file archive))))))

(ert-deftest kiss-lifecycle/conflict-preserves-provider-as-an-alternative ()
  "A permitted conflict keeps the installed provider and records the new choice."
  (kiss-test-with-fixture
    (kiss-test-make-pkg "provider" "1" "1" :files '(("usr/bin/tool" . "original\n")))
    (let ((archive (kiss-lifecycle--archive
                    "challenger" "1" "1" '(("usr/bin/tool" . "replacement\n")))))
      (unwind-protect
          (let ((kiss-force nil) (kiss-hook nil) (kiss-choice 1))
            (kiss-lifecycle-with-unprivileged-shell
              (kiss--install-tarball archive))
            (should (kiss--pkg-is-installed-p "challenger"))
            (should (string= (kiss--read-text (concat kiss-root "usr/bin/tool"))
                             "original\n"))
            (should (member "/var/db/kiss/choices/challenger>usr>bin>tool"
                            (kiss-manifest "challenger")))
            (should (member '("challenger" "/usr/bin/tool"
                             "challenger>usr>bin>tool")
                            (kiss-alternatives))))
        (ignore-errors (delete-file archive))))))

(ert-deftest kiss-lifecycle/upgrade-replaces-payload-and-removes-obsolete-files ()
  "An upgrade replaces retained payload and removes old manifest-only paths."
  (kiss-test-with-fixture
    (let ((v1 (kiss-lifecycle--archive
               "demo" "1" "1"
               '(("usr/bin/demo" . "old\n") ("usr/share/demo/obsolete" . "old\n"))))
          (v2 (kiss-lifecycle--archive
               "demo" "2" "1" '(("usr/bin/demo" . "new\n")))))
      (unwind-protect
          (let ((kiss-force nil) (kiss-hook nil) (kiss-choice 1))
            (kiss-lifecycle-with-unprivileged-shell
              (kiss--install-tarball v1)
              (kiss--install-tarball v2))
            (should (string= (kiss--read-text (concat kiss-root "usr/bin/demo")) "new\n"))
            (should-not (file-exists-p (concat kiss-root "usr/share/demo/obsolete")))
            (should (string= (kiss--get-installed-pkg-version "demo") "2 1"))
            (should-not
             (directory-files-recursively kiss-root "__kiss-el-tmp-")))
        (ignore-errors (delete-file v1))
        (ignore-errors (delete-file v2))))))

(ert-deftest kiss-lifecycle/remove-erases-package-payload-and-db ()
  "Removal trusts the manifest and removes both payload and installed state."
  (kiss-test-with-fixture
    (let ((archive (kiss-lifecycle--archive
                    "demo" "1" "1" '(("usr/bin/demo" . "payload\n")))))
      (unwind-protect
          (let ((kiss-force nil) (kiss-hook nil))
            (kiss-lifecycle-with-unprivileged-shell
              (kiss--install-tarball archive)
              (kiss-remove "demo"))
            (should-not (file-exists-p (concat kiss-root "usr/bin/demo")))
            (should-not (kiss--pkg-is-installed-p "demo")))
        (ignore-errors (delete-file archive))))))

(ert-deftest kiss-lifecycle/remove-refuses-installed-hard-dependency ()
  "Without force, removal leaves a dependency required by another package intact."
  (kiss-test-with-fixture
    (kiss-test-make-pkg "library" "1" "1" :files '(("usr/lib/libdemo.so" . "lib")))
    (kiss-test-make-pkg "consumer" "1" "1" :depends '("library"))
    (let ((kiss-force nil) (kiss-hook nil))
      (kiss-remove "library")
      (should (kiss--pkg-is-installed-p "library"))
      (should (file-exists-p (concat kiss-root "usr/lib/libdemo.so"))))))

(ert-deftest kiss-property/manifest-is-canonical-for-generated-trees ()
  "Generated trees always yield unique, root-relative, reverse-sorted manifests."
  (dotimes (seed 12)
    (let ((dir (file-name-as-directory (make-temp-file "kiss-tree-" t))))
      (unwind-protect
          (progn
            (dotimes (index (+ 1 (mod seed 6)))
              (kiss-lifecycle--write
               (format "%susr/share/p%d/file-%d" dir seed index)
               (format "%d:%d" seed index)))
            (let ((manifest (kiss--get-manifest-for-dir dir)))
              (should (equal manifest (reverse (sort (copy-sequence manifest) #'string-lessp))))
              (should (= (length manifest) (length (seq-uniq manifest))))
              (should (seq-every-p (lambda (path) (string-prefix-p "/" path)) manifest))
              (should (equal manifest
                             (string-split (kiss--manifest-to-string manifest) "\n" t)))))
        (ignore-errors (delete-directory dir t))))))

(ert-deftest kiss-property/dependency-order-is-deterministic-postorder-for-dags ()
  "Generated acyclic graphs always place each dependency before its dependent."
  (dotimes (seed 10)
    (let* ((names (mapcar (lambda (index) (format "p%d" index)) (number-sequence 0 7)))
           (dependencies
            (mapcar
             (lambda (index)
               (cons (nth index names)
                     (seq-filter
                      (lambda (candidate)
                        (and (< (seq-position names candidate) index)
                             (zerop (mod (+ seed index (seq-position names candidate)) 3))))
                      names)))
             (number-sequence 0 7))))
      (cl-letf (((symbol-function 'kiss--package-get-dependencies)
                 (lambda (name) (cdr (assoc name dependencies)))))
        (let ((first (kiss--package-get-dependency-order names))
              (second (kiss--package-get-dependency-order names)))
          (should (equal first second))
          (dolist (entry dependencies)
            (dolist (dependency (cdr entry))
              (should (< (seq-position first dependency)
                         (seq-position first (car entry)))))))))))

;;; test-kiss-lifecycle.el ends here
