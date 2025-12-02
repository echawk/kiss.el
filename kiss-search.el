;;; kiss-search.el --- KISS search abstractions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Wrappers for searching.

;;; Code:
(progn
  (require 'seq)
  (require 'cl-lib))

(require 'kiss-env)
(require 'kiss-file)
(require 'kiss-package)

;; -> search       Search for packages
;; ===========================================================================
;;;###autoload
(defun kiss-search (q)
  (interactive "sQuery: ")
  ;; FIXME: use kiss--file-exists-p ?
  (seq-filter 'file-exists-p
              (mapcar (lambda (repo) (concat repo "/" q))
                      `(,@kiss-path
                        ,kiss-installed-db-dir))))

(defun kiss--search-pkg-obj (q)
  (let ((res (kiss-search q)))
    (when res
      (kiss--dir-to-kiss-package (car res)))))
