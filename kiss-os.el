;;; kiss-os.el --- KISS OS abstractions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Wrappers for OS information.

;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'pcase))
(require 'kiss-env)

(defmacro kiss--silent-shell-command (command)
  "Run shell commands silently in the terminal."
  `(let ((inhibit-message t)
         (message-log-max nil))
     (shell-command ,command)))

(defun kiss--shell-commands (command-lst)
  "Run COMMAND-LST in order, stopping at the first nonzero status.
Return t only if every command succeeds.  This small adapter centralizes
command aggregation for callers that need portable, explicit failure
propagation."
  (let ((success t))
    (while (and success command-lst)
      (setq success (zerop (kiss--silent-shell-command (car command-lst))))
      (setq command-lst (cdr command-lst)))
    success))

(defun kiss--shell-command-as-user (command user)
  "Run COMMAND as USER using `kiss-su'."
  (kiss--silent-shell-command (concat kiss-su " -u " user " -- " command)))

(defun kiss--get-uid-from-user (user)
  (string-to-number
   (shell-command-to-string (concat "id -u " user))))

(defun kiss--shell-command-as-user (command user)
  "Run COMMAND as USER using `kiss-su'."
  (kiss--silent-shell-command (concat kiss-su " -u " user " -- " command)))

(defun kiss--get-user-from-uid (uid)
  "Return the name for UID.  `$ getent passwd' is parsed for the information."
  (pcase system-type
    ('darwin
     ;; macOS ships a working version of id.
     (car
      (string-split
       (shell-command-to-string (format "id -un %d" uid)) "\n" t)))
    ((or 'gnu/linux 'berkeley-unix)
     (let ((regex (rx bol
                      (group-n 1 (1+ (not ":"))) ":"
                      (0+ (not ":")) ":"
                      (literal (number-to-string uid))
                      (0+ any)
                      eol))
           ;; NOTE: there is a portability penalty here for using getent(1).
           ;; This will work fine on Linux and the *BSDs, but not on macOS.
           (cmd-out (string-split
                     (shell-command-to-string "getent passwd") "\n" t)))
       (thread-last
         cmd-out
         (seq-filter (lambda (s) (string-match regex s)))
         (car)
         (replace-regexp-in-string regex "\\1"))))
    (_ (error (concat system-type " is not supported by kiss.el")))))


(provide 'kiss-os)
