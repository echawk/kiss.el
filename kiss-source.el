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
(require 'kiss-hook)

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

(defun kiss--source-destination-p (destination)
  "Return non-nil when DESTINATION is a safe relative extraction directory."
  (declare (side-effect-free t))
  (and (stringp destination)
       (not (string-match-p (rx (or "\0" "\n")) destination))
       (not (string-prefix-p "/" destination))
       (not (member ".." (string-split destination "/" t)))))

(defun kiss--source-validate-object (source)
  "Validate SOURCE's portable fields and return SOURCE.
This checks metadata syntax only.  Whether a local pathname exists is a
resolution-time concern, since repository-relative sources need their recipe
directory to be known first."
  (declare (side-effect-free t))
  (let ((type (plist-get source :type))
        (uri (plist-get source :uri))
        (destination (plist-get source :extracted-path))
        (ref (plist-get source :commit-or-branch)))
    (unless (memq type '(local remote git hg fossil))
      (error "Invalid source type: %S" type))
    (unless (and (stringp uri) (not (string-empty-p uri))
                 (not (string-match-p (rx (or "\0" "\n" whitespace)) uri)))
      (error "Invalid source URI: %S" uri))
    (when (eq type 'remote)
      (unless (string-match-p "://" uri)
        (error "Remote source lacks a URL scheme: %s" uri)))
    (when (memq type '(git hg fossil))
      (unless (string-match-p "://" uri)
        (error "VCS source lacks a URL scheme: %s" uri))
      (unless (and (stringp ref)
                   (not (string-match-p (rx (or "\0" "\n" whitespace)) ref)))
        (error "Invalid source reference: %S" ref)))
    (unless (kiss--source-destination-p destination)
      (error "Invalid source destination: %S" destination))
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
  "Fetch OBJ into its cache and return t only when every command succeeds."
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
      (kiss--run-hook "pre-source" p (kiss--source-to-string obj) cache-path)
      (let ((success
             (pcase ty
          ('git
           (and
            (or (kiss--file-exists-p (concat cache-path "/.git"))
                (progn
                  (make-directory cache-path t)
                  (eq 0 (kiss--silent-shell-command (concat "git init " cache-path)))))
            (kiss--with-dir
             cache-path
             (and
              (or (eq 0 (kiss--silent-shell-command
                         (concat "git remote set-url origin " u " 2> /dev/null")))
                  (eq 0 (kiss--silent-shell-command (concat "git remote add origin " u))))
              (eq 0 (kiss--silent-shell-command (concat "git fetch --depth=1 origin " commit)))
              (eq 0 (kiss--silent-shell-command "git reset --hard FETCH_HEAD"))))))
          ('hg
           (and
            (or (kiss--file-exists-p (concat cache-path "/.hg"))
                (progn
                  (make-directory cache-path t)
                  (kiss--with-dir cache-path (eq 0 (kiss--silent-shell-command "hg init")))))
            (kiss--with-dir
             (concat cache-path "/.hg")
             (progn
               (kiss--write-text (concat "[paths]\n" "default = " u)
                                  'utf-8 "hgrc")
               t))
            (kiss--with-dir cache-path
                            (eq 0 (kiss--silent-shell-command (concat "hg pull -r " commit))))))
          ('fossil
           (if (file-exists-p (concat cache-path "/.fossil-settings"))
               (kiss--with-dir cache-path
                                (eq 0 (kiss--silent-shell-command (concat "fossil update " commit))))
             (progn
               (make-directory cache-path t)
               (kiss--with-dir cache-path
                                (eq 0 (kiss--silent-shell-command
                                       (concat "fossil open -f " u " " commit)))))))

          ('remote
           (if (file-exists-p cache-path) t
             (eq 0
                 (kiss--silent-shell-command
                  (concat
                   kiss-get
                   " " u (kiss--get-download-utility-arguments) cache-path)))))

          ('local
           (kiss--file-exists-p
            (if (string-match-p (rx bol "/") u) u
              (concat (car (kiss-search p)) "/" u)))))))
        (when success
          (kiss--run-hook "post-source" p (kiss--source-to-string obj) cache-path))
        success))))


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
  (kiss--source-validate-object obj)
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
  "Parse one portable `sources' record STR into a source object.
Blank and comment records belong to the file reader and are not valid here."
  ;; Parsing allocates a fresh, mutable source object.
  (declare (side-effect-free t))

  (unless (and (stringp str)
               (not (string-match-p (rx (or "\0" "\n")) str)))
    (error "Invalid source record: %S" str))
  (let ((trimmed (string-trim str)))
    (unless (string-match
             "\\`\\([^[:space:]]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?\\'" trimmed)
      (error "Source record must contain SOURCE and optional DESTINATION: %s" str))
    (let ((source-field (match-string 1 trimmed))
          ;; Unlike `depends', the destination is the remainder of the line.
          ;; Retain embedded spaces for consumer compatibility, though portable
          ;; producer files conventionally avoid them.
          (destination (match-string 2 trimmed)))
    (let ((type (cond ((string-prefix-p "git+" source-field) 'git)
                      ((string-prefix-p "hg+" source-field) 'hg)
                      ((string-prefix-p "fossil+" source-field) 'fossil)
                      ((string-match-p "://" source-field) 'remote)
                      (t 'local)))
          (uri source-field)
          (reference ""))
      (when (memq type '(git hg fossil))
        (setq uri (replace-regexp-in-string
                   (rx bol (or "git+" "hg+" "fossil+")) "" uri))
        ;; The final @ or # introduces the optional shallow reference.
        (when (string-match "\\`\\(.+\\)[@#]\\([^@#]+\\)\\'" uri)
          (setq reference (match-string 2 uri)
                uri (match-string 1 uri)))
        (when (string-match-p (rx (or "@" "#") eol) uri)
          (error "Source reference is empty: %s" source-field)))
      (let ((object (kiss-source :type type :uri uri
                                 :commit-or-branch reference
                                 :extracted-path
                                 (replace-regexp-in-string "/+\\'" ""
                                                           (or destination "")))))
        (kiss--source-validate-object object))))))


(defun kiss--sources-file-to-sources-objs (file-path)
  "Read source records from FILE-PATH, ignoring blank lines and comments."
  (mapcar #'kiss--string-to-source-obj
          (seq-remove (lambda (line)
                        (or (string-match-p (rx bol (* whitespace) eol) line)
                            (string-match-p (rx bol (* whitespace) "#") line)))
                      (kiss--file-read-file file-path))))

(defun kiss--def-pkg-sources (name &rest arguments)
  (let ((src-objs (mapcar #'kiss--string-to-source-obj arguments)))
    (mapc (lambda (obj) (setf (plist-get obj :package) name)) src-objs)
    src-objs))

(provide 'kiss-source)
