;;; kiss-package --- KISS package object & functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains the obejct code & functions for kiss packages.

;;; Code:

(eval-when-compile
  (require 'rx))
(progn
  (require 'eieio)
  (require 'seq))

(require 'kiss-env)
(require 'kiss-file)
(require 'kiss-source)

(defconst *kiss-package-required-shell-commands*
  '("cp"))

(kiss-ensure-shell-commands-are-available *kiss-package-required-shell-commands*)


;; FIXME: add support for post-install & pre-remove
(defclass kiss-package ()
  ((name
    :initarg :name
    :initform ""
    :type string
    :custom string
    :documentation "The name of a kiss-package.")
   (version
    :initarg :version
    :initform ""
    :type string
    :custom string
    :documentation "The version of a kiss-package")
   (release
    :initarg :release
    :initform ""
    :type string
    :custom string
    :documentation "The release of a kiss-package")
   ;; NOTE: would like this ot be optionally a build script -
   ;; that way one can define a package entirely through elisp -
   ;; when it comes time to build th package, there is a check
   ;; on whether or not the string that is within :build-file is a
   ;; file, if it is, execute said file, otherwise, (note this is an extension)
   ;; we save the string to a file, mark it executable, and execute that file
   (build-file
    :initarg :build-file
    :initform ""
    :type string
    :custom string
    :documentation "The file path to the build file for a kiss-package."
    :optional)
   (depends
    :initarg :depends
    :initform '()
    :type list
    :custom list
    :documentation "List of packages that are dependencies of this package."
    :optional)
   (make-depends
    :initarg :make-depends
    :initform '()
    :type list
    :custom list
    :documentation "List of packages that are make depends of this kiss-package."
    :optional)
   (sources
    :initarg :sources
    :initform '()
    :type list
    :custom list
    :documentation "List of kiss-source objects for the kiss-package"
    :optional)
   (pre-remove-file
    :initarg :pre-remove-file
    :initform ""
    :type string
    :custom string
    :optional)
   (post-install-file
    :initarg :post-install-file
    :initform ""
    :type string
    :custom string
    :optional)))

(defun kiss--package-bin-name (pkg)
  "Return the proper name for the binary for PKG at VERSION."
  (with-slots ((v :version) (r :release) (n :name)) pkg
    (concat n "@" v "-" r ".tar"
            (unless (string-empty-p kiss-compress)
              (concat "." kiss-compress)))))

(defun kiss--package-get-missing-dependencies (pkg-obj)
  (with-slots
      ((depends      :depends)
       (make-depends :make-depends))
      pkg-obj
    (seq-remove #'kiss--pkg-is-installed-p (append depends make-depends))))

(defun kiss--package-extract-sources (pkg-obj dir)
  ;; Ensure that the build directory exists. This is needed for packages
  ;; which have no sources.
  (unless (kiss--file-is-directory-p dir)
    ;; NOTE make-directory is not handled by us.
    (make-directory dir t))
  (dolist (source (slot-value pkg-obj :sources))
    (with-slots
        ((type      :type)
         (extr-path :extracted-path))
        source
      (let ((cache  (kiss--source-get-cache-path source))
            (outdir (concat dir "/" extr-path)))
        (unless (kiss--file-is-directory-p outdir)
          (make-directory outdir t))

        (pcase type
          ((or 'git 'hg 'fossil)
           (shell-command (concat "cp -PRf " cache "/. "  outdir)))
          (_
           (if (kiss--str-tarball-p cache)
               (kiss--extract-tarball cache outdir)
             (shell-command (concat "cp -PRf " cache " " outdir)))))))))

(defun kiss--dir-to-kiss-package (dir-path)
  (let ((name          (kiss--basename dir-path))
        (build-file    (concat dir-path "/build"))
        (ver-file      (concat dir-path "/version"))
        (source-file   (concat dir-path "/sources"))
        (depends-file  (concat dir-path "/depends"))
        (checksum-file (concat dir-path "/checksums"))

        (pre-remove-file   (concat dir-path "/pre-remove"))
        (post-install-file (concat dir-path "/post-install"))

        (obj nil)

        (ver  nil)
        (rel   nil)
        (srcs  nil)
        (deps  nil)
        (mdeps nil))

    (setq ver (car (string-split (car (kiss--file-read-file ver-file)) " " t)))
    (setq rel (cadr (string-split (car (kiss--file-read-file ver-file)) " " t)))

    (setq obj
          (make-instance
           'kiss-package
           :name name
           :build-file build-file
           :version ver
           :release rel))

    (when (kiss--file-exists-p source-file)
      (setq srcs (kiss--sources-file-to-sources-objs source-file))
      (mapc (lambda (o) (oset o :package name)) srcs))
    (when srcs (oset obj :sources srcs))

    (when (kiss--file-exists-p depends-file)
      (let ((read-data
             (seq-remove
              (lambda (s) (string-match-p (rx bol "#") s))
              (kiss--file-read-file depends-file))))

        (setq deps
              (seq-remove
               (lambda (str) (string-match-p (rx (1+ any) (1+ whitespace) "make") str))
               read-data))

        (setq mdeps
              (mapcar
               (lambda (str)
                 (replace-regexp-in-string (rx (1+ whitespace) (0+ any) eol) "" str))
               (seq-difference read-data deps))))
      (when deps  (oset obj :depends deps))
      (when mdeps (oset obj :make-depends mdeps)))

    ;; TODO: make all local sources absolute paths.
    ;; We obviosly still support the notion of relative paths in the
    ;; build files, but we may as well save ourselves the lookup
    ;; when it comes to actually building the package.

    (when (kiss--file-exists-p checksum-file)
      (let ((checksum-data
             (kiss--file-read-file checksum-file))
            (non-vc-src-objs
             (seq-remove
              (lambda (o)
                ;; NOTE: all supported vc systems (for source) need to be here.
                (pcase (slot-value o :type)
                  ((or 'git 'hg 'fossil) t)))
              (slot-value obj :sources))))

        (dotimes (i (length checksum-data))
          (oset (nth i non-vc-src-objs) :checksum (nth i checksum-data)))))

    (oset obj :post-install-file
          (if (kiss--file-exists-p post-install-file) post-install-file ""))
    (oset obj :pre-remove-file
          (if (kiss--file-exists-p pre-remove-file) pre-remove-file ""))
    obj))

(defun kiss--package-to-dir (package-obj dir)
  (with-slots
      ((name         :name)
       (version      :version)
       (release      :release)
       (build-file   :build-file)
       (depends      :depends)
       (make-depends :make-depends)
       (sources      :sources)
       (pre-remove-file   :pre-remove-file)
       (post-install-file :post-install-file))
      package-obj
    (let ((version-str (concat version " " release "\n"))
          (depends-str
           (concat
            (string-join
             (sort
              (append depends (mapcar (lambda (s) (concat s " make")) make-depends))
              #'string-lessp)
             "\n")
            "\n"))
          (sources-str
           (concat
            (string-join (mapcar #'kiss--source-to-string sources) "\n") "\n"))
          (checksum-str
           (concat
            (string-join
             (seq-remove
              #'string-empty-p
              (mapcar (lambda (o) (slot-value o :checksum)) sources))
             "\n")
            "\n")))
      (kiss--with-dir
       dir
       (progn
         (make-directory name)
         (kiss--write-text version-str 'utf-8 (concat name "/version"))
         (kiss--write-text depends-str 'utf-8 (concat name "/depends"))
         (kiss--write-text sources-str 'utf-8 (concat name "/sources"))
         (unless (string-empty-p checksum-str)
           (kiss--write-text checksum-str 'utf-8 (concat name "/checksums")))

         (let ((build-file-dir (kiss--dirname build-file))
               (local-sources
                (mapcar
                 (lambda (obj) (slot-value obj :uri))
                 (seq-filter
                  (lambda (obj)
                    (and (eq (slot-value obj :type) 'local)
                         (not (string-match-p "^/" (slot-value obj :uri)))))
                  sources))))
           ;; NOTE: copy-file is handled by emacs, not us.
           (copy-file build-file (concat name "/build"))
           (unless (string-empty-p pre-remove-file)
             (copy-file pre-remove-file (concat name "/pre-remove")))
           (unless (string-empty-p post-install-file)
             (copy-file post-install-file (concat name "/post-install")))
           (when local-sources
             (dolist (file local-sources)
               (make-directory (concat name "/" (kiss--dirname file)) t)
               (copy-file (concat build-file-dir "/" file) (concat name "/" file))))))))))


(provide 'kiss-package)
