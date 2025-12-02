;;; kiss-manifest.el --- KISS manifest functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Interact with manifests of packages

;;; Code:

(progn
  (require 'seq)
  (require 'subr-x))

(require 'kiss-env)
(require 'kiss-file)



(defun kiss--get-manifest-for-dir (dir)
  "(I) Return a kiss compatible manifest for DIR."
  (let ((files-and-dirs
         ;; find cmd
         (mapcar
          (lambda (file-path)
            (let ((cfp (replace-regexp-in-string dir "" file-path)))
              (cond
               ((kiss--file-is-directory-p file-path) (concat cfp "/"))
               (t cfp ))))

          ;; Filter out libtool .la files and charset.alias
          (seq-remove
           (lambda (fp)
             (string-match-p
              (rx (or (literal "charset.alias") (literal ".la")) eol)
              fp))
           (directory-files-recursively dir "." t)))))

    ;; Admittedly I'm not a huge fan of the below code, but it does work.
    (if (member "/var/db/kiss/installed/" files-and-dirs)
        (let ((pkg-db-dir
               (car (seq-filter
                     (lambda (s)
                       (string-match-p
                        (rx "/var/db/kiss/installed/" (1+ (not "/")) "/" eol)
                        s))
                     files-and-dirs))))
          ;; FIXME: this doesn't seem to work at the moment (at least etcsums)
          ;; add /var/db/kiss/installed/<pkg>/manifest
          ;; and /var/db/kiss/isntalled/<pkg>/etcsums
          (if (member "/etc" files-and-dirs)
              (setq files-and-dirs
                    (cons (concat pkg-db-dir "etcsums") files-and-dirs)))
          (setq files-and-dirs
                (cons (concat pkg-db-dir "manifest") files-and-dirs))))

    ;; sort -ur
    (reverse
     (seq-sort 'string-lessp (seq-uniq files-and-dirs)))))

(defun kiss--dir-matches-manifest-p (dir manifest-file)
  "(I) Return t or nil depending on whether a DIR matches MANIFEST-FILE."
  (equal (kiss--get-manifest-for-dir dir)
         (kiss--file-read-file manifest-file)))


(defun kiss--manifest-to-string (pkg-manifest)
  "(I) Convert our internal representation of PKG-MANIFEST into a string."
  (concat (string-join pkg-manifest "\n") "\n"))

;; (kiss--manifest-to-string (kiss-manifest "xdo"))

;;;###autoload
(defun kiss-manifest (pkg)
  "Return a list of all files owned by PKG."
  (kiss--file-read-file
   (concat kiss-installed-db-dir pkg "/manifest")))



(provide 'kiss-manifest)
