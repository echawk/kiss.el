;;; kiss-hook.el --- KISS hooks -*- lexical-binding: t; -*-


;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Kiss hook implementation.

;;; Code:

(require 'kiss-env)
(require 'kiss-file)

;;[006] List of hooks ----------------------------------------------------------

;;Each hook is executed in the order it appears in KISS_HOOK and is given its
;;own environment/arguments accordingly. The hooks are documented as follows.

;;+---------------+--------+----------+--------------------+----------------+
;;| hook          | arg1   | arg2     | arg3               | arg4           |
;;+---------------+--------+----------+--------------------+----------------+
;;|               |        |          |                    |                |
;;| build-fail    | Type   | Package  | Build directory    |                | x
;;| post-build    | Type   | Package  | DESTDIR            |                | x
;;| post-install  | Type   | Package  | Installed database |                | x
;;| post-package  | Type   | Package  | Tarball            |                | x
;;| post-source   | Type   | Package  | Verbatim source    | Resolved source|
;;| post-update   | Type   | [7]      |                    |                | x
;;| pre-build     | Type   | Package  | Build directory    |                | x
;;| pre-extract   | Type   | Package  | DESTDIR            |                | x
;;| pre-install   | Type   | Package  | Extracted package  |                | x
;;| pre-remove    | Type   | Package  | Installed database |                | x
;;| pre-source    | Type   | Package  | Verbatim source    | Resolved source|
;;| pre-update    | Type   | [7] [8]  |                    |                | x
;;| queue-status  | Type   | Package  | Number in queue    | Total in queue |
;;|               |        |          |                    |                |
;;+---------------+--------+----------+--------------------+----------------+

;;[7] The -update hooks start in the current repository. In other words, you can
;;    operate on the repository directly or grab the value from '$PWD'.

;;[8] The second argument of pre-update is '0' if the current user owns the
;;    repository and '1' if they do not. In the latter case, privilege
;;    escalation is required to preserve ownership.

(defun kiss--run-hook (hook &optional arg2 arg3 arg4)
  "(I) Run all hooks in `kiss-hook'."
  (dolist (kh kiss-hook)
    (when (kiss--file-is-executable-p kh)
      (let ((status (kiss--silent-shell-command
                     (format "%s %s %s %s %s" kh hook arg2 arg3 arg4))))
        ;; A build-fail hook observes an already-failing build, so it cannot
        ;; meaningfully make that failure worse.  Every other hook failure is
        ;; fatal, matching the external KISS hook contract.
        (when (and (not (string= hook "build-fail")) (not (zerop status)))
          (error "KISS hook failed: %s (%s)" kh status))))))

(defun kiss--run-hook-pkg (hook pkg)
  "(I) Run PKG's HOOK."
  (let ((hook-fp (concat kiss-installed-db-dir pkg "/" hook)))
    (when (kiss--file-is-executable-p hook-fp)
      ;; FIXME: need to expose the proper environment to this shell
      (with-environment-variables
          (("KISS_ROOT" kiss-root))
        (let ((status (kiss--shell-command-as-user
                       hook-fp (kiss--file-get-owner-name kiss-root))))
          (unless (zerop status)
            (error "KISS package hook failed: %s (%s)" hook-fp status)))))))

(provide 'kiss-hook)
