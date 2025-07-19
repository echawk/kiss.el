;;; kiss-source.el --- KISS source object & methods -* lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains the object code & related methods for kiss sources
;; including all logic for handling remote & local sources.

(require 'kiss-file)

(defclass kiss-source ()
  ((package
    :initarg :package
    :initform ""
    :type string
    :documentation "The package which this source is a source of.")
   (type
    :initarg :type
    :type symbol
    :options '(git hg fossil remote local)
    :documentation "A symbol to determine what kind of source it is.")
   (uri
    :initarg :uri
    :type string
    :documentation "The URI for the kiss-source")
   (checksum
    :initarg :checksum
    :initform ""
    :type string
    :documentation "The checksum of the source if applicable"
    :optional)
   (extracted-path
    :initarg :extracted-path
    :initform ""
    :type string
    :documentation "The path where the source will be extracted to in the packages build directory."
    :optional)
   (commit-or-branch
    :initarg :commit-or-branch
    :initform ""
    :type string
    :documentation "The relevant commit or branch for a git source."
    :optional)))

(cl-defmethod kiss--source-get-cache-path ((obj kiss-source))
  (with-slots
      ((ty :type)
       (u  :uri)
       (p  :package)
       (ep :extracted-path))
      obj

    (let ((dest-dir
           (kiss--normalize-file-path (concat kiss-srcdir p "/" ep "/"))))
      (pcase ty
        ((or 'git 'hg 'fossil 'remote)
         (concat dest-dir (car (reverse (string-split u "/")))))
        ('local
         (if (string-match (rx bol "/") u)
             u
           (concat (car (kiss-search p)) "/" u)))))))

(cl-defmethod kiss--source-download ((obj kiss-source))

  (let ((cache-path (kiss--source-get-cache-path obj)))
    (make-directory (kiss--dirname cache-path) t)
    (with-slots
        ((ty :type)
         (u  :uri)
         (cb :commit-or-branch)
         (p  :package))
        obj
      (let ((commit
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
              (concat (car (kiss-search p)) "/" u)))))))))


(cl-defmethod kiss--source-get-local-checksum ((obj kiss-source))
  (with-slots
      ((ty :type))
      obj
    (pcase ty
      ((or 'git 'hg 'fossil) "")
      (_ (kiss--b3 (kiss--source-get-cache-path obj))))))

(cl-defmethod kiss--source-validate-p ((obj kiss-source))
  (if (string= (slot-value obj :checksum) "SKIP")
      t
    (string= (slot-value obj :checksum) (kiss--source-get-local-checksum obj))))

(cl-defmethod kiss--source-to-string ((obj kiss-source))
  (with-slots
      ((package :package)
       (type :type)
       (uri :uri)
       (checksum :checksum)
       (extracted-path :extracted-path)
       (commit-or-branch :commit-or-branch))
      obj
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


;; (mapcar #'kiss--source-to-string
;;         (slot-value
;;          (kiss--dir-to-kiss-package (car (kiss-search "llvm")))
;;          :sources)
;;         )

(defun kiss--string-to-source-obj (str)
  "(I) Generate a kiss-source object from STR. Will have an empty package slot."

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

    (setq obj (make-instance 'kiss-source :type type :uri uri))
    (oset obj :commit-or-branch (if (eq c-or-b nil) "" c-or-b))
    (oset obj :extracted-path (if (eq extr-path nil) "" extr-path))
    obj))


(defun kiss--sources-file-to-sources-objs (file-path)
  (mapcar #'kiss--string-to-source-obj (kiss--read-file file-path)))
