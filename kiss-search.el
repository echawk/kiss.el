;;; kiss-search.el --- KISS search abstractions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Wrappers for searching.

;;; Code:
(require 'seq)

(require 'kiss-env)
(require 'kiss-file)
(require 'kiss-package)

;; -> search       Search for packages
;; ===========================================================================
;;;###autoload
(defun kiss-search (q)
  (interactive "sQuery: ")
  "Return package directories matching the KISS glob Q in lookup order."
  (let ((pattern (wildcard-to-regexp q)))
    (seq-mapcat
     (lambda (repo)
       (if (file-directory-p repo)
           (seq-filter #'file-directory-p
                       (directory-files repo t pattern t))
         '()))
     `(,@kiss-path ,kiss-installed-db-dir))))

(defun kiss--search-pkg-obj (q)
  (let ((res (kiss-search q)))
    (when res
      (kiss--dir-to-kiss-package (car res)))))

(provide 'kiss-search)
