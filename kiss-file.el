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
  '("test" "sed" "od" "cut" "mktemp"))

;; Ensure all of the required commands are present on the host system.
(cl-assert
 (not
  (member
   nil
   (mapcar
    #'executable-find
    *kiss-file-required-shell-commands*))))

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
  (eq 0 (kiss--silent-shell-command
         (concat "test -x " (kiss--single-quote-string file-path)))))

;; TODO: Look into rm'ing these funcs since they should not have to exist.
(defun kiss--file-is-regular-file-p (file-path)
  "Return T if FILE-PATH exists and is a regular file."
  (eq 0 (kiss--silent-shell-command
         (concat "test -f " (kiss--single-quote-string file-path)))))

(defun kiss--file-is-symbolic-link-p (file-path)
  "Return T if FILE-PATH exists and is a symbolic link."
  (eq 0 (kiss--silent-shell-command
         (concat "test -h " (kiss--single-quote-string file-path)))))

(defun kiss--file-is-directory-p (file-path)
  "Return T if FILE-PATH exists and is a directory."
  (eq 0 (kiss--silent-shell-command
         (concat "test -d " (kiss--single-quote-string file-path)))))

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

(provide 'kiss-file)
;;; kiss-file.el ends here
