;;; kiss-source.el --- KISS source object & methods -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains the object code & related methods for kiss sources
;; including all logic for handling remote & local sources.

;;; Code:
(eval-when-compile
  (require 'kiss-env))
(progn
  (require 'seq))
(require 'kiss-file)

(defun kiss-source (&rest fields)
  "Create a portable source record from keyword FIELDS.

The representation is a property list instead of an EIEIO object.  It maps
directly to a Scheme alist/SRFI record or a Common Lisp plist and deliberately
keeps no Emacs object-system dependency in the package format layer."
  (unless (zerop (mod (length fields) 2))
    (error "kiss-source requires keyword/value pairs"))
  (let ((source (list :package "" :type nil :uri "" :checksum ""
                      :extracted-path "" :commit-or-branch "")))
    (while fields
      (setf (plist-get source (pop fields)) (pop fields)))
    source))

(defun kiss--source-get-cache-path (obj)
  (declare (side-effect-free t))
  (let* ((ty (plist-get obj :type))
         (u  (plist-get obj :uri))
         (p  (plist-get obj :package))
         (ep (plist-get obj :extracted-path))
         (dest-dir (kiss--file-normalize-file-path
                    (concat kiss-srcdir p "/" ep "/"))))
    (pcase ty
      ((or 'git 'hg 'fossil 'remote)
       (concat dest-dir (car (reverse (string-split u "/")))))
      ('local
       (if (string-match (rx bol "/") u)
           u
         (concat (car (kiss-search p)) "/" u))))))

(defun kiss--source-download (obj)

  (let ((cache-path (kiss--source-get-cache-path obj)))
    (make-directory (kiss--dirname cache-path) t)
    (let* ((ty (plist-get obj :type))
           (u (plist-get obj :uri))
           (cb (plist-get obj :commit-or-branch))
           (p (plist-get obj :package))
           (commit
             (if (string-empty-p cb)
                 (pcase ty
                   ('git    "HEAD")
                   ('hg     "tip")
                   ('fossil "trunk"))
               cb)))
        (pcase ty
          ('git

           (progn
             (unless (kiss--file-exists-p (concat cache-path "/.git"))
               (make-directory cache-path t)
               (shell-command (concat "git init " cache-path)))

             (kiss--with-dir
              cache-path
              (progn
                (unless
                    (eq 0
                        (shell-command
                         (concat "git remote set-url origin " u " 2> /dev/null")))
                  (shell-command
                   (concat "git remote add origin " u)))
                (shell-command (concat "git fetch --depth=1 origin " commit))
                (shell-command (concat "git reset --hard FETCH_HEAD"))))
             ;; FIXME: return wether or not we were actually successful
             t))

          ;; FIXME: finish these implementations
          ('hg
           (progn
             (unless (kiss--file-exists-p (concat cache-path "/.hg"))
               (make-directory cache-path t)
               (kiss--with-dir cache-path (shell-command "hg init")))
             (kiss--with-dir
              (concat cache-path "/.hg")
              (kiss--write-text
               (concat "[paths]\n"
                       "default = " u)
               'utf-8 "hgrc"))
             (kiss--with-dir
              cache-path
              (shell-command (concat "hg pull -r " commit)))))

          ;; NOTE: the following could also potentially work:
          ;; (concat "hg clone -u " commit " " u)))

          ;; FIXME: Doesn't yet account for updated urls yet.
          ('fossil
           (if (file-exists-p (concat cache-path "/.fossil-settings"))
               (progn
                 (kiss--with-dir
                  cache-path
                  (shell-command (concat "fossil update " commit))))
             (progn
               (make-directory cache-path t)
               (kiss--with-dir
                cache-path
                (shell-command (concat "fossil open -f " u " " commit))))))

          ('remote
           (if (file-exists-p cache-path) t
             (eq 0
                 (shell-command
                  (concat
                   kiss-get
                   " " u (kiss--get-download-utility-arguments) cache-path)))))

          ('local
           (kiss--file-exists-p
            (if (string-match-p (rx bol "/") u) u
              (concat (car (kiss-search p)) "/" u))))))))


(defun kiss--source-get-local-checksum (obj)
  (pcase (plist-get obj :type)
    ((or 'git 'hg 'fossil) "")
    (_ (kiss--b3 (kiss--source-get-cache-path obj)))))

(defun kiss--source-validate-p (obj)
  (if (string= (plist-get obj :checksum) "SKIP")
      t
    (string= (plist-get obj :checksum) (kiss--source-get-local-checksum obj))))

(defun kiss--source-to-string (obj)
  "Return the canonical KISS `sources' record for OBJ."
  ;; OBJ is mutable, so this is effect-free but not compiler-pure.
  (declare (side-effect-free t))
  (let ((type (plist-get obj :type))
        (uri (plist-get obj :uri))
        (extracted-path (plist-get obj :extracted-path))
        (commit-or-branch (plist-get obj :commit-or-branch)))
    (concat
     (pcase type
       ('git    "git+")
       ('hg     "hg+")
       ('fossil "fossil+")
       (_ ""))
     uri
     (when (and (not (string-empty-p commit-or-branch))
                (pcase type ((or 'git 'hg 'fossil) t)))
       (concat "@" commit-or-branch))
     (unless (string-empty-p extracted-path)
       (concat " " extracted-path)))))


(defun kiss--string-to-source-obj (str)
  "(I) Generate a kiss-source object from STR. Will have an empty package slot."
  ;; Parsing allocates a fresh, mutable source object.
  (declare (side-effect-free t))

  (let ((type      nil)
        (uri       nil)
        (extr-path nil)
        (c-or-b    nil)
        (obj       nil)
        (str-split-on-spaces (string-split str " " t)))
    (setq type
          (pcase str
            ((rx bol "git+") 'git)
            ((rx bol "hg+") 'hg)
            ((rx bol "fossil+") 'fossil)
            ((rx "://") 'remote)
            (_ 'local)))
    (setq c-or-b
          (pcase type
            ((or 'git 'hg 'fossil)
             (thread-last
               str-split-on-spaces
               (car)
               (funcall (lambda (s) (string-split s (rx (or "#" "@")))))
               (nth 1)))
            (_ "")))
    (setq extr-path (nth 1 str-split-on-spaces))
    (setq uri
          (replace-regexp-in-string
           (rx bol (or "git+" "hg+" "fossil+"))
           ""
           (thread-last
             str-split-on-spaces
             (car)
             (funcall (lambda (s) (string-split s (rx (or "#" "@")))))
             (car))))

    (setq obj (kiss-source :type type :uri uri
                           :commit-or-branch (or c-or-b "")
                           :extracted-path (or extr-path "")))
    obj))


(defun kiss--sources-file-to-sources-objs (file-path)
  "Read source records from FILE-PATH, ignoring blank lines and comments."
  (mapcar #'kiss--string-to-source-obj
          (seq-remove (lambda (line)
                        (string-match-p (rx bol (* whitespace) "#") line))
                      (kiss--file-read-file file-path))))

(defun kiss--def-pkg-sources (name &rest arguments)
  (let ((src-objs (mapcar #'kiss--string-to-source-obj arguments)))
    (mapc (lambda (obj) (setf (plist-get obj :package) name)) src-objs)
    src-objs))

(provide 'kiss-source)
