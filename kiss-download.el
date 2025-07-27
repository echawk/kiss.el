;;; kiss-download.el --- KISS source downloading -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Download sources for packges.

;;; Code:

(progn
  (require 'seq)
  (require 'subr-x))

(require 'kiss-env)
(require 'kiss-source)


(defun kiss--download-pkg-obj (pkg-obj)
  (thread-last
    (slot-value pkg-obj :sources)
    (mapcar (lambda (o) (oset o :package (slot-value pkg-obj :name)) o))
    (mapcar #'kiss--source-download)
    (funcall (lambda (l) (seq-reduce (lambda (x y) (and x y)) l t)))))

;;;###autoload
(defun kiss-download (pkgs-l)
  (interactive "sQuery: ")
  (cond ((listp pkgs-l)
         (seq-reduce
          (lambda (x y) (and x y))
          (flatten-list (mapcar #'kiss-download pkgs-l)) t))
        ((atom pkgs-l)
         ;; TODO: implement a seq-reduce to return a single value here?
         (thread-last
           pkgs-l
           (kiss--search-pkg-obj)
           (kiss--download-pkg-obj)))
        (t nil)))

;; (kiss-download '("kiss" "gdb"))
;; (kiss-download '("hugs"))

(defun kiss--get-download-utility-arguments ()
  "(I) Get the proper arguments for the `kiss-get' utility."
  (cdr (assoc kiss-get kiss-get-alist)))

(provide 'kiss-download)
