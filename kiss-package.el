;;; kiss-package --- KISS package object & functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains the obejct code & functions for kiss packages.

;;; Code:

(eval-when-compile
  (require 'rx))
(progn
  (require 'tsort)
  (require 'seq))

(require 'kiss-env)
(require 'kiss-file)
(require 'kiss-manifest)
(require 'kiss-os)
(require 'kiss-source)

(defun kiss-package (&rest fields)
  "Create a portable package record from keyword FIELDS.

The record is a plist so that package-format logic maps one-for-one to a
Common Lisp plist or a Scheme alist/record."
  (unless (zerop (mod (length fields) 2))
    (error "kiss-package requires keyword/value pairs"))
  (let ((package (list :name "" :version "" :release "" :build-file ""
                       :depends '() :make-depends '() :sources '()
                       :pre-remove-file "" :post-install-file "")))
    (while fields
      (setf (plist-get package (pop fields)) (pop fields)))
    package))

(defun kiss--package-bin-name (pkg)
  "Return the proper name for the binary for PKG at VERSION."
  (declare (side-effect-free t))
  (concat (plist-get pkg :name) "@" (plist-get pkg :version) "-"
          (plist-get pkg :release) ".tar"
          (unless (string-empty-p kiss-compress)
            (concat "." kiss-compress))))

(defun kiss--package-dependencies-to-string (depends make-depends)
  "Return the canonical `depends' file contents for DEPENDS and MAKE-DEPENDS.
Runtime dependencies are written verbatim; build-only dependencies carry the
portable ` make' suffix.  Keeping this serialization in one place lets build
time dependency discovery update the staged database without changing the
meaning of recipe dependencies."
  (declare (side-effect-free t))
  (concat
   (string-join
    (sort
     (append depends (mapcar (lambda (dependency)
                              (concat dependency " make"))
                            make-depends))
     #'string-lessp)
    "\n")
   "\n"))

(defun kiss--package-get-missing-dependencies (pkg-obj)
  (seq-remove #'kiss--pkg-is-installed-p
              (append (plist-get pkg-obj :depends)
                      (plist-get pkg-obj :make-depends))))

(defun kiss--package-extract-sources (pkg-obj dir)
  ;; Ensure that the build directory exists. This is needed for packages
  ;; which have no sources.
  (unless (kiss--file-is-directory-p dir)
    ;; NOTE make-directory is not handled by us.
    (make-directory dir t))
  (dolist (source (plist-get pkg-obj :sources))
    (let ((type (plist-get source :type))
          (extr-path (plist-get source :extracted-path)))
      (let ((cache (kiss--source-get-cache-path source))
            (outdir (concat dir "/" extr-path)))
        (unless (kiss--file-is-directory-p outdir)
          (make-directory outdir t))

        (pcase type
          ((or 'git 'hg 'fossil)
           (shell-command (concat "cp -PRf " cache "/. "  outdir)))
          (_
           (if (kiss--file-is-tarball-p cache)
               (kiss--file-extract-tarball cache outdir)
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
          (kiss-package
           :name name
           :build-file build-file
           :version ver
           :release rel))

    (when (kiss--file-exists-p source-file)
      (setq srcs (kiss--sources-file-to-sources-objs source-file))
      (mapc (lambda (o) (setf (plist-get o :package) name)) srcs))
    (when srcs (setf (plist-get obj :sources) srcs))

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
      (when deps  (setf (plist-get obj :depends) deps))
      (when mdeps (setf (plist-get obj :make-depends) mdeps)))

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
                (pcase (plist-get o :type)
                  ((or 'git 'hg 'fossil) t)))
              (plist-get obj :sources))))

        (dotimes (i (length checksum-data))
          (setf (plist-get (nth i non-vc-src-objs) :checksum)
                (nth i checksum-data)))))

    (setf (plist-get obj :post-install-file)
          (if (kiss--file-exists-p post-install-file) post-install-file ""))
    (setf (plist-get obj :pre-remove-file)
          (if (kiss--file-exists-p pre-remove-file) pre-remove-file ""))
    obj))

(defun kiss--package-to-dir (package-obj dir)
  (let ((name (plist-get package-obj :name))
        (version (plist-get package-obj :version))
        (release (plist-get package-obj :release))
        (build-file (plist-get package-obj :build-file))
        (depends (plist-get package-obj :depends))
        (make-depends (plist-get package-obj :make-depends))
        (sources (plist-get package-obj :sources))
        (pre-remove-file (plist-get package-obj :pre-remove-file))
        (post-install-file (plist-get package-obj :post-install-file)))
    (let ((version-str (concat version " " release "\n"))
          (depends-str (kiss--package-dependencies-to-string
                        depends make-depends))
          (sources-str
           (concat
            (string-join (mapcar #'kiss--source-to-string sources) "\n") "\n"))
          (checksum-str
           (concat
            (string-join
             (seq-remove
              #'string-empty-p
              (mapcar (lambda (o) (plist-get o :checksum)) sources))
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
                 (lambda (obj) (plist-get obj :uri))
                 (seq-filter
                  (lambda (obj)
                    (and (eq (plist-get obj :type) 'local)
                         (not (string-match-p "^/" (plist-get obj :uri)))))
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

(defun kiss--package-sanitize-ver-str (str)
  "Sanitize a version string STR to be correctly compared against others."
  (declare (pure t) (side-effect-free t))
  (thread-last
    str
    (replace-regexp-in-string " " "")
    (replace-regexp-in-string "\n$" "")))

(defun kiss--package-remote-eq-pkg-local-p (pkg)
  "Return t if the version of PKG is the same locally and from the remotes."
  (string=
   (kiss--package-sanitize-ver-str
    (kiss--read-text (concat (car (kiss-search pkg)) "/version")))
   (kiss--package-sanitize-ver-str
    (kiss--get-installed-pkg-version pkg))))

;; FIXME: consider moving to kiss-file.el?
;; FIXME?: may need to have this code take in a general path
;; for pkg - this is so that this code can be reused to implement
;; the conflicts system.
;; pkg_manifest_replace() in kiss
(defun kiss--package-manifest-replace (pkg old new)
  "Replace the line matching OLD in the manifest of PKG with NEW."
  ;; Replace the matching line in the manifest w/ the desired
  ;; replacement.
  (let* ((manifest-f (concat kiss-installed-db-dir pkg "/manifest"))
         (temp-f     (kiss--file-make-temp-file))
         (owner      (kiss--file-get-owner-name manifest-f))
         (manifest-t (kiss--manifest-to-string
                      (reverse
                       (seq-sort 'string-lessp
                                 (mapcar (lambda (s) (if (string= s old) new s))
                                         (kiss-manifest pkg)))))))

    (kiss--write-text manifest-t 'utf-8 temp-f)

    ;; TODO: see if this can be avoided.
    ;; Ensure the ownership is preserved.
    ;; NOTE: chown can work with uids instead of names
    (kiss--shell-command-as-user
     (concat "chown " owner ":" owner " " temp-f) owner)
    ;; Ensure the permissions are set correctly.
    (kiss--shell-command-as-user
     (concat "chmod " (kiss--file-rwx manifest-f) " " temp-f) owner)
    ;; Move it into place.
    (kiss--shell-command-as-user
     (concat "mv -f " temp-f " " manifest-f)
     owner)))

(defun kiss--package-get-conflict-files (pkg dir)
  ;; It is assumed that DIR will be an extracted kiss pkg.
  (let ((manifest-file
         (concat dir "/var/db/kiss/installed/" pkg "/manifest")))

    (unless (kiss--file-exists-p manifest-file)
      (error "Missing package manifest: %s" manifest-file))

    ;; TODO: would like to investigate the penalty of using a pure
    ;; Emacs lisp based solution for this.
    (let ((current-installed-files
           (thread-last
             (kiss--get-installed-manifest-files)
             (seq-remove (lambda (fp) (string-match-p pkg fp)))
             (mapcar #'kiss--file-read-file)
             (apply #'append)
             (seq-remove (lambda (fp) (string-match-p (rx "/" eol) fp))))))
      (seq-filter
       (lambda (fp) (member fp current-installed-files))
       (kiss--file-read-file manifest-file)))))

(defun kiss--pkg-conflicts (pkg extr-dir)
  "(I) Fix up DIR for PKG so as to allow for alternatives."
  (let ((conf-files (kiss--package-get-conflict-files pkg extr-dir)))
    (when conf-files
      ;; The choices database is root-relative package data.  An extracted
      ;; archive has its own host prefix, so do not append KISS_ROOT's absolute
      ;; pathname into it.
      (let ((stage-choices-dir
             (concat (file-name-as-directory extr-dir)
                     (file-relative-name kiss-choices-db-dir kiss-root))))
        (make-directory stage-choices-dir t)

      ;; Move all of the files to the appropriate place.
        (dolist (path conf-files)
          (let* ((alt (string-replace "/" ">" path))
                 (alt-record
                  (concat "/"
                          (file-relative-name
                           (concat kiss-choices-db-dir pkg alt) kiss-root))))

          ;; Move the file to the choices directory.  Both paths are already
          ;; fully resolved within the staging tree; `rename-file' avoids a
          ;; shell parser treating the `>' characters in an alternative name
          ;; as redirection operators.
            (rename-file (concat extr-dir path) (concat extr-dir alt-record) t)))

      ;; Regenerate the manifest for the directory.
        (kiss--write-text
         (kiss--manifest-to-string (kiss--get-manifest-for-dir extr-dir))
         'utf-8 (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest"))))))

(defun kiss--package-get-orphan-alternatives (pkg)
  "(I) Return a list of orphaned alternatives that would result from removing PKG."
  (let ((orphaned-alternatives
         (seq-filter
          (lambda (pair) (seq-contains-p pair pkg))
          (kiss-preferred))))
    ;; Return the files associated with PKG.
    (if orphaned-alternatives
        (mapcar #'cadr orphaned-alternatives))))

(defun kiss--pkg-is-installed-p (pkg)
  "(I) Return t if PKG is installed, nil otherwise."
  (kiss--file-exists-p (concat kiss-installed-db-dir pkg)))

(defun kiss--get-installed-pkg-version (pkg)
  "(I) Return the version string for PKG, nil if PKG is not installed."
  (if (kiss--pkg-is-installed-p pkg)
      (let ((pdir (concat kiss-installed-db-dir pkg)))
        (replace-regexp-in-string
         "\n$" ""
         ;; TODO: see if there is a way to avoid
         ;; depending on f.el
         (kiss--read-text (concat pdir "/version"))))))

(defun kiss--pkg-is-removable-p (pkg)
  "(I) Return t if PKG is removable, nil otherwise."

  ;; A package is removable when the following conditions are met:
  ;; * the pkg is installed on the current system
  ;; * nothing on the system hard depends on the package
  ;; * the package does not leave any orphaned alternatives
  (and
   (kiss--pkg-is-installed-p pkg)
   (eq (kiss--package-get-hard-dependents pkg) nil)
   (eq (kiss--package-get-orphan-alternatives pkg) nil)))


(defun kiss--get-dependencies-from-file (file-path)
  "(I) Return the dependencies in FILE-PATH."
  (when (file-exists-p file-path)
    (seq-remove
     #'string-empty-p
     ;; All of this below is to emulate `awk '{print $1}' < file'
     (mapcar (lambda (s) (car (string-split s " ")))
             (string-split
              (replace-regexp-in-string
               "#.*$" ""
               (kiss--read-text file-path))
              "\n")))))

(defun kiss--package-get-dependencies (pkg &optional installed-p)
  "(I) Get the dependencies of PKG as a list, nil if PKG has no dependencies.

Optionally, if INSTALLED-P is t, then the system installed package will be
read instead."
  ;; Faster (older) version of the function here.
  (kiss--get-dependencies-from-file
   (if (and installed-p (kiss--pkg-is-installed-p pkg))
       (concat kiss-installed-db-dir pkg "/depends")
     (concat (car (kiss-search pkg)) "/depends")))

  )

(defun kiss--pkgs-without-repo ()
  "(I) Return all packages that are installed that are not in a remote repo."
  (let ((pkgs-l (mapcar 'car (kiss-list))))
    (seq-filter
     (lambda (p)
       ;; Naturally, anything that was only *installed* will have 0 other
       ;; occurances.
       (eq 0
           (length
            ;; Remove the installed-db-dir *repo* from the list.
            (seq-remove
             (lambda (repo) (string-match-p kiss-installed-db-dir repo))
             (kiss-search p)))))
     pkgs-l)))

(defun kiss--package-get-order (pkgs-lst)
  "(I) Get the proper build order for the packages in PKGS-LST."
  (seq-intersection (kiss--package-get-dependency-order pkgs-lst) pkgs-lst))

(defun kiss--package-get-local-checksums (pkg)
  "(I) Return the list of checksums for PKG from the files on disk, or nil."
  (thread-last
    (plist-get (kiss--search-pkg-obj pkg) :sources)
    (mapcar #'kiss--source-get-local-checksum)
    (seq-remove #'string-empty-p)))

(defun kiss--package-get-hard-dependents (pkg)
  "(I) Return a list of installed packages that have a runtime dependency on PKG."
  (mapcar
   (lambda (dep-file)
     (replace-regexp-in-string
      "/depends" ""
      (replace-regexp-in-string
       kiss-installed-db-dir ""
       dep-file)))
   (seq-filter
    (lambda (depfile)
      (string-match (rx bol (literal pkg) (zero-or-more " ") eol)
                    (kiss--read-text depfile)))
    (seq-filter
     #'file-exists-p
     (mapcar (lambda (p) (concat kiss-installed-db-dir p "/depends"))
             (nthcdr 2 (directory-files kiss-installed-db-dir)))))))

(defun kiss--package-get-make-orphans ()
  "(I) Return a list of packages which only have make dependents."
  ;; NOTE: This function is pretty slow at the moment.
  (seq-filter
   (lambda (pkg)
     (and (eq (kiss--package-get-hard-dependents pkg) nil)
          (not
           (eq (kiss--package-get-make-dependents pkg) nil))))
   (mapcar #'car (kiss-list))))

(defun kiss--package-get-make-dependents (pkg)
  "(I) Return a list of installed packages that have a make dependency on PKG."
  (mapcar
   (lambda (dep-file)
     (replace-regexp-in-string
      "/depends" ""
      (replace-regexp-in-string
       kiss-installed-db-dir ""
       dep-file)))
   (seq-filter
    (lambda (depfile)
      (string-match (rx bol (literal pkg) (one-or-more " ") (literal "make"))
                    (kiss--read-text depfile)))
    (seq-filter
     #'file-exists-p
     (mapcar (lambda (p) (concat kiss-installed-db-dir p "/depends"))
             (nthcdr 2 (directory-files kiss-installed-db-dir)))))))

;; FIXME: have this tak a list of package objects??
(defun kiss--package-get-dependency-graph (pkg-lst &optional installed-p)
  "(I) Generate a graph of the dependencies for PKG-LST.

Optionally, if INSTALLED-P is t, then the dependencies for each
package in PKG-LST are queried from the installed dependency file,
provided that the package is actually installed."
  (let ((queue
         (cond
          ((atom pkg-lst) `(,pkg-lst))
          ((listp pkg-lst) pkg-lst)))
        (seen '())
        (res '()))
    (while queue
      (setq queue (seq-remove (lambda (e) (member e seen)) queue))
      (if (car queue)
          (progn
            (setq seen (cons (car queue) seen))
            (let* ((dep (car queue))
                   (dep-deps
                    ;; If installed-p is t and the package is installed.
                    (if (and installed-p (kiss--pkg-is-installed-p dep))
                        (kiss--package-get-dependencies dep t)
                      (kiss--package-get-dependencies dep)))
                   (item `(,dep ,dep-deps)))
              (if (not (member item res))
                  (setq res (cons item res)))
              (setq queue (append dep-deps (cdr queue)))))))
    res))

(defun kiss--package-get-dependency-graph-rec-step (queue seen res)
  "Build a dependency graph from QUEUE, carrying SEEN and RES.
This named helper replaces a local recursive binding so the algorithm maps
directly to a normal Scheme or Common Lisp function."
  (let* ((new-queue (seq-remove (lambda (entry) (member entry seen)) queue))
         (dep (car new-queue)))
    (if dep
        (let* ((new-seen (cons dep seen))
               (dep-deps (kiss--package-get-dependencies dep))
               (item `(,dep ,dep-deps))
               (new-res (if (member item res) res (cons item res))))
          (kiss--package-get-dependency-graph-rec-step
           (append dep-deps (cdr new-queue)) new-seen new-res))
      res)))

;; FIXME: implement installed-p argument here.
(defun kiss--package-get-dependency-graph-rec (pkg-lst)
  "(I) A Recursive & TCO-ed implementation of `kiss--package-get-dependency-graph'.

This version of the function is meant primarily as a resource for people
looking to implement kiss in other functional programming languages.
It is also important to note that there is no meaninful decrease in speed
when using this function compared with the iterative version."
  (let ((pkgs (if (listp pkg-lst) pkg-lst (list pkg-lst))))
    (kiss--package-get-dependency-graph-rec-step pkgs '() '())))

(defun kiss--package-get-dependency-order (pkg-lst &optional installed-p)
  "(I) Return the proper build order of the dependencies for each pkg in PKG-LST."
  (let ((res (tsort
              (if installed-p
                  (kiss--package-get-dependency-graph pkg-lst t)
                (kiss--package-get-dependency-graph pkg-lst)))))
    (if res res (error "Circular dependency detected!"))))

(provide 'kiss-package)
