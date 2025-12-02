;;; kiss-update.el --- KISS repo update abstractions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Wrappers for updating repos.

;;; Code:

(eval-when-compile
  )

(progn
  )

(require 'kiss-env)
(require 'kiss-source)
(require 'kiss-hook)

;; TODO: consider expanding this macro to create all of the functions
;; that we will need - this will need to take a few more arguments (or
;; possibly not)
(defmacro kiss--update-repo-type (type)
  `(let ((repos
          (seq-uniq
           (mapcar (intern (concat "kiss--get-" (symbol-name ,type) "-dir-toplevel"))
                   (funcall
                    (intern (concat "kiss--kiss-path-" (symbol-name ,type) "-repos")))))))
     (dolist (repo repos)
       (kiss--with-dir
        repo
        (let ((repo-owner (kiss--file-get-owner-name repo))
              (am-owner-p (kiss--file-am-owner-p repo))
              (pull-cmd   (pcase ,type
                            ('git    "git pull")
                            ('hg     "hg pull")
                            ('fossil "fossil pull")))
              (update-cmd (pcase ,type
                            ('git    "git submodule update --remote --init -f")
                            ('hg     "hg update")
                            ('fossil "fossil update"))))

          (kiss--run-hook "pre-update" (if am-owner-p 0 1) repo-owner)
          ;; TODO: would like to make this a macro so that way this
          ;; code can be deduped
          (if am-owner-p
              (progn
                (shell-command pull-cmd)
                (shell-command update-cmd))
            (progn
              (kiss--shell-command-as-user pull-cmd repo-owner)
              (kiss--shell-command-as-user update-cmd repo-owner)))
          (kiss--run-hook "post-update"))))))


(defun kiss--dir-is-git-repo-p (dir)
  "(I) Return t if DIR is a git repo, nil otherwise."
  (eq 0 (shell-command (concat "git -C " dir " rev-parse 'HEAD@{upstream}'"))))

(defun kiss--git-subm-superproject-dir (dir)
  "(I) Return the directory for a git submodule's (DIR) superproject."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (concat "git -C " dir " rev-parse --show-superproject-working-tree"))))

(defun kiss--dir-is-git-subm-p (dir)
  "(I) Return t if DIR is a git submodule, nil otherwise."
  (not (string-empty-p (kiss--git-subm-superproject-dir dir))))

(defun kiss--get-git-dir-toplevel (dir)
  "(I) Return the toplevel directory for a git repo, of which DIR is a subdir."
  (let* ((dir-is-subm-p (kiss--dir-is-git-subm-p dir))
         (repo (if dir-is-subm-p
                   (kiss--git-subm-superproject-dir dir)
                 dir)))
    (replace-regexp-in-string
     "\n$" ""
     (shell-command-to-string
      (concat "git -C " repo " rev-parse --show-toplevel")))))

(defun kiss--kiss-path-git-repos ()
  "(I) Return only the repos in `kiss-path' that are git repos."
  (seq-filter 'kiss--dir-is-git-repo-p kiss-path))

;; TODO: display whether signature verification is enabled...
(defun kiss--update-git-repos ()
  "(I) Update all git repos in `kiss-path'."
  (kiss--update-repo-type 'git))

(defun kiss--get-hg-dir-toplevel (dir)
  (kiss--with-dir dir (shell-command-to-string "hg root")))

(defun kiss--dir-is-hg-repo-p (dir)
  (kiss--with-dir dir (eq 0 (shell-command "hg root"))))

(defun kiss--kiss-path-hg-repos ()
  (seq-filter #'kiss--dir-is-hg-repo-p kiss-path))

(defun kiss--update-hg-repos ()
  (kiss--update-repo-type 'hg))

(defun kiss--get-fossil-dir-toplevel (dir)
  (kiss--with-dir
   dir
   (let ((tld
          (thread-last
            (shell-command-to-string "fossil info")
            (funcall (lambda (output) (split-string output "\n" t)))
            (mapcar (lambda (line) (string-split line ":")))
            (seq-filter (lambda (lst) (string= "local-root" (car lst)))))))
     (when tld
       (replace-regexp-in-string (rx (1+ space)) "" (cadr (car tld)))))))

(defun kiss--dir-is-fossil-repo-p (dir)
  (if (kiss--get-fossil-dir-toplevel dir) t nil))

(defun kiss--kiss-path-fossil-repos ()
  (seq-filter #'kiss--dir-is-fossil-repo-p kiss-path))

(defun kiss--update-fossil-repos ()
  (kiss--update-repo-type 'fossil))

;; TODO: Rethink how to integrate this.
;; (defun kiss--print-git-repo-MOTD ()
;;   "(I) Print out all of the MOTDs from each git repo."
;;   (let ((git-repos (delete-dups (cl-mapcar 'kiss--get-git-dir-toplevel (kiss--kiss-path-git-repos)))))
;;     (dolist (repo git-repos)
;;       (if (file-exists-p (concat repo "/MOTD"))
;;           (shell-command-to-string (concat "cat " repo "/MOTD"))))))

;;;###autoload
(defun kiss-update ()
  (interactive)
  (when (member :GIT kiss-features)
    (kiss--update-git-repos))
  (when (member :MERCURIAL kiss-features)
    (kiss--update-hg-repos))
  (when (member :FOSSIL kiss-features)
    (kiss--update-fossil-repos)))

(provide 'kiss-update)
