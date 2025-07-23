;;; kiss-file.el --- KISS file utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file is a collection of file handling utilities for kiss, meant to
;; keep the main file clean of some of the logic that is expressed here.

;;; Code:

(eval-when-compile
  (require 'rx))
(progn
  (require 'cl-lib)
  (require 'seq)
  (require 'subr-x))

(require 'kiss-env)
(require 'kiss-os)

;; FIXME: try to remove as many calls to external programs as possible.
(defconst *kiss-file-required-shell-commands*
  '("test" "sed" "od" "cut" "mktemp" "tar" "mv" "cp" "rm" "rmdir"))

;; Ensure all of the required commands are present on the host system.
(kiss-ensure-shell-commands-are-available *kiss-file-required-shell-commands*)

;; https://en.wikipedia.org/wiki/List_of_file_signatures
;; See section on tar files & friends - would like to be able to
;; determine file type to allow for more assertions in kiss.el

(defconst *kiss-file-tarball-magic-regex*
  (rx
   (* " ")
   "75"
   (* " ")
   "73"
   (* " ")
   "74"
   (* " ")
   "61"
   (* " ")
   "72"
   (* " ")
   (or "00" "20")
   (* " ")
   (or "20" "30")
   (* " ")
   (or "00" "30")))

(defconst *kiss-file-bzip2-magic-regex*
  (rx
   (* " ")
   "42"
   (* " ")
   (or "5a" "5A")
   (* " ")
   "68"))

(defconst *kiss-file-gzip-magic-regex*
  (rx
   (* " ")
   (or "1f" "1F")
   (* " ")
   (or "8b" "8B")))

(defconst *kiss-file-xz-magic-regex*
  (rx
   (* " ")
   (or "fd" "FD")
   (* " ")
   "37"
   (* " ")
   (or "7a" "7a")
   (* " ")
   "58"
   (* " ")
   (or "5a" "5A")
   (* " ")
   "00"))

(defconst *kiss-file-zstd-magic-regex*
  (rx
   (* " ")
   "28"
   (* " ")
   (or "b5" "B5")
   (* " ")
   (or "2f" "2F")
   (* " ")
   (or "fd" "FD")))

(defun kiss--file-is-tarball-p (file-path)
  (string-match-p
   *kiss-file-tarball-magic-regex*
   (shell-command-to-string
    (concat
     "od -t x1 -j 257 -N 8 "
     file-path
     " | "
     "sed -En -e \"/[0-9]{7}[ ]+/s/[0-9]{7}//p\""))))

(defun kiss--file-is-bzip2-p (file-path)
  (string-match-p
   *kiss-file-bzip2-magic-regex*
   (shell-command-to-string
    (concat
     "od -t x1 -j 0 -N 3 "
     file-path
     " | "
     "sed q | cut -d' ' -f2- "))))

(defun kiss--file-is-gzip-p (file-path)
  (string-match-p
   *kiss-file-gzip-magic-regex*
   (shell-command-to-string
    (concat
     "od -t x1 -j 0 -N 4 "
     file-path
     " | "
     "sed q | cut -d' ' -f2- "))))

(defun kiss--file-is-xz-p (file-path)
  (string-match-p
   *kiss-file-xz-magic-regex*
   (shell-command-to-string
    (concat
     "od -t x1 -j 0 -N 6 "
     file-path
     " | "
     "sed q | cut -d' ' -f2- "))))

(defun kiss--file-is-zstd-p (file-path)
  (string-match-p
   *kiss-file-zstd-magic-regex*
   (shell-command-to-string
    (concat
     "od -t x1 -j 0 -N 4 "
     file-path
     " | "
     "sed q | cut -d' ' -f2- "))))

(defun kiss--file-is-executable-p (file-path)
  "Return T if FILE-PATH exists and is executable."
  (eq 0 (shell-command (format "test -x '%s'" file-path))))

;; TODO: Look into rm'ing these funcs since they should not have to exist.
(defun kiss--file-is-regular-file-p (file-path)
  "Return T if FILE-PATH exists and is a regular file."
  (eq 0 (shell-command (format "test -f '%s'" file-path))))

(defun kiss--file-is-symbolic-link-p (file-path)
  "Return T if FILE-PATH exists and is a symbolic link."
  (eq 0 (shell-command (format "test -h '%s'" file-path))))

(defun kiss--file-is-directory-p (file-path)
  "Return T if FILE-PATH exists and is a directory."
  (eq 0 (shell-command (format "test -d '%s'" file-path))))

(defun kiss--file-identify (file-path)
  "Identify FILE-PATH as a symbol representing what kind of file it is."
  (pcase file-path
    ((pred kiss--file-is-directory-p)     'directory)
    ((pred kiss--file-is-symbolic-link-p) 'symlink)
    ((pred kiss--file-is-regular-file-p)  'file)))

;; FIXME: go back through the code and make this also check if a directory
;; exists as well.
;; NOTE: DO NOT USE THIS ANYWHERE THAT ISN'T ABSOLUTELY NECESSARY.
(defun kiss--file-exists-p (file-path)
  "This function should ideally not have to exist exist.
However, `file-exists-p' and `file-symlink-p' are fundamentally broken when it
comes to broken symlinks.  Hence the need for this function.
This function returns t if FILE-PATH exists and nil if it doesn't."
  (or
   (kiss--file-is-directory-p file-path)
   (kiss--file-is-regular-file-p file-path)
   (kiss--file-is-symbolic-link-p file-path)))


(defun kiss--file-normalize-file-path (file-path)
  "Normalize the number of '/' in FILE-PATH."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (rx (1+ "/") "/") "/" file-path))

;; FIXME: potentially change this function
;; to simply remove the final newline at the end of the text
;; and to then split on the newlines - it should result in the same
;; code as the following for kiss-manifest, but would allow this
;; code to be used other places too
(defun kiss--file-read-file (file-path)
  "Read FILE-PATH as a list of lines, with empty newlines removed."
  (when (kiss--file-exists-p file-path)
    (seq-remove
     (lambda (s) (string= "" s))
     (string-split (kiss--read-text file-path) "\n"))))

(defun kiss--write-text (text encoding file-path)
  (with-temp-file file-path
    (insert (format "%s" text))))

(defun kiss--read-text (file-path)
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun kiss--file-make-temp-file ()
  "(I) Make a temporary file using the `mktemp' utility."
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "mktemp")))


(defun kiss--rwx-lst-to-octal (lst)
  (cl-assert
   (seq-reduce
    (lambda (a b) (and a b))
    (mapcar
     (lambda (elt) (member elt (string-to-list "rwx-")))
     (flatten-list lst)) t))

  (let ((vals '(4 2 1))
        (tot 0))
    (dotimes (i (length lst))
      (setq tot (+ tot
                   (* (if (eq (nth i lst) 45) 0 1)
                      (nth i vals)))))
    tot))


(defun kiss--file-rwx (file-path)
  (thread-last
    file-path
    (file-attributes)
    (file-attribute-modes)
    (string-to-list)
    (cdr)
    (funcall (lambda (lst) (seq-partition lst 3)))
    (mapcar #'kiss--rwx-lst-to-octal)
    (funcall (lambda (lst) (mapconcat #'number-to-string lst "")))))


(defun kiss--file-get-owner (file-path)
  "Return the owner uid of FILE-PATH."
  (file-attribute-user-id (file-attributes file-path)))

(defun kiss--file-get-owner-name (file-path)
  "Return the owner name of FILE-PATH."
  (kiss--get-user-from-uid (kiss--file-get-owner file-path)))

(defun kiss--file-am-owner-p (file-path)
  "Return t if the current LOGNAME owns the FILE-PATH, nil otherwise."
  (eql
   (user-real-uid)
   (kiss--file-get-owner file-path)))


;; FIXME: use kiss--file-normalize-file-path
(defun kiss--dirname (file-path)
  (concat
   "/"
   (string-join
    (seq-reverse (seq-drop (seq-reverse (string-split file-path "/" t)) 1))
    "/")))

(defun kiss--basename (file-path)
  (car (seq-reverse (string-split file-path "/"))))

;; TODO: need to account for symlinks w/ (file-symlink-p

(defun kiss--file-remove-file (file-path)
  "Remove FILE-PATH as the appropriate user using rm(1)."
  (if (kiss--file-exists-p file-path)
      (let ((owner (kiss--file-get-owner-name file-path))
            (rmcmd (format "rm -- '%s'" file-path)))
        (eq 0
            (if (kiss--file-am-owner-p file-path)
                (shell-command rmcmd)
              (kiss--shell-command-as-user rmcmd owner))))))

(defun kiss--file-remove-directory (dir-path)
  "Remove DIR-PATH as the appropriate user using rmdir(1)."
  (if (and (kiss--file-is-directory-p dir-path)
           (not (kiss--file-is-symbolic-link-p dir-path)))
      (let ((owner (kiss--file-get-owner-name dir-path))
            (rmcmd (format "rmdir -- '%s'" dir-path)))
        (eq 0
            (if (kiss--file-am-owner-p dir-path)
                (shell-command rmcmd)
              (kiss--shell-command-as-user rmcmd owner))))))

(defun kiss--file-remove-files (file-path-lst)
  "Remove all files and empty directories in FILE-PATH-LST."

  ;; TODO: Check this function with packages that have more elaborate
  ;; symlink structures, and ensure that this function removes all of the
  ;; files in the manifest properly.
  ;; I think one way to remedy this would be to try to remove all of the
  ;; directories from the file-path-lst again, once all of the symlinks are
  ;; gone, so that way all of the dirs can be properly removed.

  ;; This will return all of the /etc files/dirs.
  ;; (cl-remove-if-not
  ;;  (lambda (file-path) (string-match-p "/etc/" file-path))
  ;;  file-path-lst)

  ;; NOTE: I'm not entirely sure if it removing the files in the proper
  ;; order since it should be removing all of the files in a dir first,
  ;; then the dir itself, but when testing on 'xdo' it does not remove
  ;; the actual directory in `kiss-installed-db-dir'.

  ;; Make this local variable since we need to rm the symlinks
  ;; separately.
  (let ((symlink-queue '()))
    (mapc
     (lambda (file-path)
       (pcase (kiss--file-identify file-path)
         ('directory (kiss--file-remove-directory file-path))
         ('symlink   (setq symlink-queue
                           (cons file-path symlink-queue)))
         ('file      (kiss--file-remove-file      file-path))))
     file-path-lst)
    ;; Now to cleanup broken symlinks.
    (mapcar #'kiss--file-remove-file symlink-queue)))

(defun kiss--file-extract-tarball (tarball dir)
  "(I) Extract TARBALL to DIR.  Emulates GNU Tar's --strip-components=1."
  (let ((decomp-tarball (kiss--file-make-temp-file)))
    ;; Decompress the tarball.
    (kiss--decompress tarball decomp-tarball)
    ;; Extract the tarball.
    (kiss--with-dir dir (shell-command (concat "tar xf " decomp-tarball)))
    ;; Get all of the top level directories from the tarball.
    (mapc
     (lambda (tld)
       (let* ((temp-f (kiss--file-make-temp-file))
              (temp-d (concat temp-f "-" tld)))
         (message "%s" (eq 0
                           (shell-command
                            (concat "mv -f " (concat dir "/" tld) " " temp-d))))

         ;; NOTE: we need to call directory-files twice here, since
         ;; First do the mv's
         (mapc
          (lambda (f)
            (shell-command
             (concat "mv -f " (concat temp-d f) " " dir)))
          (nthcdr 2 (directory-files temp-d)))

         ;; Then do the cp's
         (let ((files (nthcdr 2 (directory-files temp-d))))
           (if files
               (mapc
                (lambda (f)
                  (shell-command
                   (concat "cp -fRPp " (concat temp-d f) " " dir)))
                files)))

         ;; Make sure to rm the temp file.
         (kiss--shell-command-as-user
          (concat "rm -- " temp-f) (kiss--file-get-owner-name temp-f))
         ;; Also rm the temp directory.
         (kiss--shell-command-as-user
          (concat "rm -rf -- " temp-d) (kiss--file-get-owner-name temp-d))))

     ;; Make sure we only go through each top level directory *once*.
     (seq-uniq
      (mapcar
       ;; Get rid of any possible junk after the /.
       (lambda (str) (replace-regexp-in-string (rx "/" (0+ any) eol) "/" str))
       (seq-filter
        (lambda (line)
          ;; Keep lines that have only **1** / in them.
          (string-match-p (rx bol (0+ (not "/")) "/" (0+ (not "/")) eol) line))
        ;; TODO: can I remove this seq-uniq?
        (string-split
         (shell-command-to-string (concat "tar tf " tarball))
         "\n" t)))))

    ;; Remove our decompressed tarball now that we are done with it.
    (kiss--shell-command-as-user
     (concat "rm -f -- " decomp-tarball)
     (kiss--file-get-owner-name decomp-tarball))))

(provide 'kiss-file)
;;; kiss-file.el ends here
