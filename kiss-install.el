;;; kiss-install.el --- KISS installation functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provides functionality to install kiss packages.

;;; Code:

(progn
  (require 'seq)
  (require 'subr-x))

(require 'kiss-env)
(require 'kiss-file)
(require 'kiss-package)


(defun kiss--get-tmp-destdir ()
  "(I) Return a directory that can be used as a temporary destdir."
  ;; NOTE: This is not a *perfect* system, however, it is not as easy to
  ;; do the pid trick that the shell implementation of kiss does.
  ;; So the compromise is to pick a random number from 1 to 30000.
  (let ((rn (kiss--get-random-number)))
    (while (file-exists-p (concat kiss-tmpdir rn))
      (setq rn (kiss--get-random-number)))
    (make-directory (concat kiss-tmpdir rn) t)
    (concat kiss-tmpdir rn)))

(defun kiss--package-get-from-manifest (file-path-lst)
  "(I) Determine the package name from a manifest."
  (when (member kiss-installed-db-dir file-path-lst)
    (thread-last
      file-path-lst
      (seq-filter
       (lambda (fp) (string-match-p
                (rx
                 (literal kiss-installed-db-dir) (1+ (not "/")) "/" eol) fp)))
      (car)
      (funcall (lambda (str) (string-split str "/" t)))
      (reverse)
      (car))))

(defun kiss--install-tarball (tarball)
  "(I) Install TARBALL if it is a valid kiss package."
  (unless (kiss--file-exists-p tarball)
    (error (concat "kiss/install: " tarball " doesn't exist!")))

  (let* ((proc-dir       (kiss--get-tmp-destdir))
         (extr-dir       (concat proc-dir "/extracted"))
         (decomp-tarball (kiss--file-make-temp-file)))

    (make-directory extr-dir t)

    (kiss--decompress tarball decomp-tarball)
    (kiss--with-dir
     extr-dir
     (shell-command
      (concat "tar xf " decomp-tarball)))
    (kiss--file-remove-file decomp-tarball)

    (let ((pkg (kiss--package-get-from-manifest
                (kiss--get-manifest-for-dir extr-dir))))

      (unless pkg
        (error "kiss/install: Unable to detemine the package!"))

      ;; assume that the existence of the manifest file is all that
      ;; makes a valid KISS pkg.
      (unless (file-exists-p
               (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest"))
        (error "kiss/install: Not a valid kiss package!"))

      ;; Now that the pkg is verified to be a kiss pkg, we need
      ;; to validate the manifest that was shipped with the pkg.
      (unless (kiss--dir-matches-manifest-p
               extr-dir
               (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest"))
        (error "kiss/install: Manifest is not valid!"))

      ;; Only check for missing dependencies when 'kiss-force' is nil.
      (unless kiss-force
        ;; Check to make sure we aren't missing any dependencies.
        (let ((extr-depends
               (concat extr-dir "/var/db/kiss/installed/" pkg "/depends")))
          (when (kiss--file-exists-p extr-depends)
            (when (seq-contains-p
                   (mapcar #'kiss--pkg-is-installed-p
                           (kiss--get-dependencies-from-file extr-depends))
                   nil)
              (error "kiss/install: Missing dependencies!")))))

      (kiss--run-hook "pre-install" pkg extr-dir)

      (when (kiss--package-get-conflict-files pkg extr-dir)
        (if (eq 0 kiss-choice)
            (error "kiss/install: kiss-choice is equal to 0, erroring out!")
          (progn
            (message
             (concat "Fixing package conflicts for " pkg " in " extr-dir))
            (kiss--pkg-conflicts pkg extr-dir)
            (message
             (concat "Done fixing package conflicts.")))))

      ;; FIXME: This code is *technically* not needed, but hey, might as well.
      ;; This function is typically *super cheap* to run.
      ;; Now that the pkg is verified to be a kiss pkg, we need
      ;; to validate the manifest that was shipped with the pkg.
      (unless (kiss--dir-matches-manifest-p
               extr-dir
               (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest"))
        (error "kiss/install: Manifest is not valid!"))

      ;; If the pkg is already installed (and this is an upgrade)
      ;; make a backup of the manifest and etcsum files
      (if (kiss--pkg-is-installed-p pkg)
          (progn
            (shell-command
             (format "cp '%s%s/manifest' '%s/manifest-copy'"
                     kiss-installed-db-dir pkg proc-dir))
            (if (file-exists-p (concat kiss-installed-db-dir pkg "/etcsums"))
                (shell-command
                 (format "cp '%s%s/etcsums' '%s/etcsums-copy'"
                         kiss-installed-db-dir pkg proc-dir)))))

      ;; generate a list of files which exist in the current (installed)
      ;; manifest that do not exist in the new (to be installed) manifest.

      ;; FIXME: need to ensure that there is no breakage when
      ;; installing a package that is not presently intsalled.
      (let* ((new-manifest (kiss--file-read-file (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest")))
             (old-manifest (kiss--file-read-file (concat kiss-installed-db-dir pkg "/manifest")))
             (files-not-present-in-new-manifest
              ;; NOTE: the order here is backwards from upstream.
              (seq-difference old-manifest new-manifest)))
        ;; Reverse the manifest file so that we start shallow, and go deeper
        ;; as we iterate through each item. This is needed so that directories
        ;; are created in the proper order

        (message (concat "kiss/install: Installing " pkg "..."))

        ;; Install the packages files.
        (kiss--install-files extr-dir kiss-root (reverse new-manifest) pkg nil)

        ;; Remove any files that were in the old manifest that aren't
        ;; in the new one.
        (kiss--file-remove-files files-not-present-in-new-manifest)

        ;; Install the packages files for a second time to fix
        ;; any potential mess that could have been made from the
        ;; previous rm.
        (kiss--install-files extr-dir kiss-root (reverse new-manifest) pkg t)

        (kiss--run-hook-pkg "post-install" pkg)

        (kiss--run-hook "post-install" pkg (concat kiss-installed-db-dir pkg))

        (message (concat "kiss/install: Installation of " pkg " Finished.")))
      ;; FIXME: finish this func
      nil)))


;; FIXME: this need to be faster - currently it is super super slow
;; compared to the shell implementation.
;;;###autoload
(defun kiss-install (pkgs-l)
  (interactive)
  (cond
   ((listp pkgs-l)
    (mapcar #'kiss-install (kiss--package-get-order pkgs-l)))
   ((atom pkgs-l)
    (let* ((tarball
            (cond ((and (file-exists-p pkgs-l)
                        (kiss--file-is-tarball-p pkgs-l))
                   pkgs-l)
                  (t
                   (concat
                    kiss-bindir
                    (kiss--package-bin-name
                     (kiss--search-pkg-obj pkgs-l)))))))
      (when tarball
        (kiss--install-tarball tarball))))))


(provide 'kiss-install)
