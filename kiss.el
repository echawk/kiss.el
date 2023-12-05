;;; kiss.el --- KISS package manager in ELisp -*- lexical-binding: t -*-
;; SPDX-License-Identifier: MIT

;; Author: Ethan Hawk <ethan.hawk@valpo.edu>
;; Maintainer: Ethan Hawk <ethan.hawk@valpo.edu>
;; URL: https://github.com/ehawkvu/kiss.el
;; Keywords: package-manager, tools
;; Package-Requires: ((emacs "29.1") (tsort))
;; Version: 0.0.1

;; This file is under the MIT license.

;;; Commentary:

;; kiss.el is an implementation of the
;; KISS Linux package manager (https://codeberg.org/kiss-community/kiss)
;; This package can also be thought of as providing an "API" into
;; the kiss package system since, at the moment, the only way to do
;; any interesting computation with the kiss package system is to
;; parse and write everything yourself.

;; My hope is that the publishing and finishing of kiss.el will inspire
;; others to write their own implementations of kiss.

;; TODO: see if I can reduce the required Emacs version.

;; TODO: go through and mark functions as being pure or side-effect free if
;; they are, as it could improve the performance when byte compiled.

;; NOTE: IMPORTANT ~ If I want to properly support darwin as an OS
;; I will need to properly get group information for each user, since
;; at least on macOS, each user is (generally) part of the staff group.
;; see here: https://superuser.com/questions/590683/how-do-i-find-my-user-id-and-group-in-mac-os-x#590691

;; WHY??? - cuz it's good to have multiple implementations of kiss.
;; also, I don't want to leave Emacs, and this could lead to some
;; delcarative configurations of kiss.

;; Need to figure out what exactly to support - I'm thinking of
;; having a subset of kiss' features supported, primarily enough
;; for interactive use, as well as to support a delcarative configuration.
;; Anything else will be out of scope (for the time being).

;; TODO: support crux style usage???
;; - Would require that this file could be ran as an Emacs script, argument
;; parsing and all.

;; Long term I would like to have a tui/gui for managing kiss
;; packages - something similar to how guix and list-packages works.
;; This would be pretty nice, and ideally I'd like to support the same
;; type of ux as list-packages - mapping all of the common actions to
;; keybinds and whatnot.

;; Additionally, I may add some extensions/configuration specific to this
;; version of kiss.  Nothing that would break compatibilty with upstream kiss,
;; since you should be able to rely on that to fix you system should something
;; break, things such as having a nicer syntax to create your KISS_PATH,
;; the ability to export the settings in here to a shell file that you
;; can use with standard kiss, etc.

;; META-TODO: Need to go through every function and add much more
;; detailed documentation - ideally examples as well as why you
;; might care about the function as well.

;; TODO: add in (message) everywhere!!! This will be espescially
;; useful when there is a cli wrapper for this program.  Having these
;; messages is also useful more generally as it allows you to see what
;; exactly the package manager is doing at any given time.

;; TODO: remove dependency on 'cl-lib, and replace all relevant
;; calls either with dash.el, or with seq.

;; FIXME: in addition to these messages, there also needs to be
;; assertions/error checking done throughout the code.

;; FIXME: need to go through and fixup some of the bulid-install-download
;; logic to ensure that *every* missing dependency is accounted for.
;; Also, need to make sure that they return values which are *sensible*
;; and that can be used for further reasoning.

;; FIXME: generally speaking, we don't do a great job of respecting
;; the potential dependencies of /var/db/kiss/installed/*pkg

;; Also, need to go through this code once I get a fully working POC done
;; and ruthlessly remove all duplicated code, since rn there are many
;; little redundancies spread about the current source.

;; https://www.emacswiki.org/emacs/UnitTesting

;; Fun little piece of trivia for the KISS Linux veterans out there:
;; https://github.com/dylanaraps/community/commit/e370a224520d07e6e42ba045845674b39dea03a4

;;; Code:

;;(require 'subp)
(eval-when-compile
  (require 'cl-lib)
  (require 'eieio)
  (require 'ert)
  (require 'pcase)
  (require 'rx)
  (require 'seq)
  (require 'subr-x)
  (require 'tsort))

;; FIXME: Find out what the containing group should be...
(defgroup kiss nil
  "The KISS package manager, in ELisp."
  :prefix "kiss-"
  :group :application)

;; TODO: cleanup these names, they're not consistent...
(defconst kiss-root "/")
(defconst kiss-installed-db-dir (concat kiss-root "var/db/kiss/installed/"))
(defconst kiss-choices-db-dir (concat kiss-root "var/db/kiss/choices/"))

(defconst kiss-tmpdir
  (concat (getenv "HOME") "/.cache/kiss/proc/"))

(defconst kiss-srcdir
  (concat (getenv "HOME") "/.cache/kiss/sources/"))

(defconst kiss-bindir
  (concat (getenv "HOME") "/.cache/kiss/bin/"))

(defconst kiss-logdir
  (concat (getenv "HOME") "/.cache/kiss/logs/"))

(defconst kiss-version "0.0.1"
  "The version of kiss.el.")
(defconst kiss-compat-version "5.6.4"
  "The version of kiss that kiss.el is compatible with.")

(defcustom kiss-valid-download-utils
  '("aria2c" "axel" "curl" "wget" "wget2")
  "List of valid download utilities for kiss.el"
  :type '(string))

(defcustom kiss-get
  (car (seq-filter #'executable-find kiss-valid-download-utils))
  "The utility for downloading http sources."
  :type 'string
  :options kiss-valid-download-utils)

(defcustom kiss-get-alist
  '(("aria2c" . " -d / -o ")
    ("axel"   . " -o ")
    ("curl"   . " -fLo ")
    ("wget"   . " -O ")
    ("wget2"  . " -O "))
  "Association List for looking up the proper arguments for a given 'kiss-get'."
  :type 'alist)

(defcustom kiss-valid-shasum-utils
  '("openssl" "sha256sum" "sha256" "shasum" "digest")
  "List of valid sha256sum utils for kiss.el"
  :type '(string))

(defcustom kiss-chk
  (car (seq-filter #'executable-find kiss-valid-shasum-utils))
  "The utility for computing SHA256 checksums."
  :type 'string
  :options kiss-valid-shasum-utils)

(defcustom kiss-valid-sudo-like-utils
  '("ssu" "sudo" "doas" "su")
  "List of valid sudo-like utils for kiss.el"
  :type '(string))

;; FIXME: when root, we do as root does - we don't need any of this.
;; FIXME: Using 'su' is currently not supported by this package manager.
(defcustom kiss-su
  (car (seq-filter #'executable-find kiss-valid-sudo-like-utils))
  "The utility that will be used to elevate priviledges."
  :type 'string
  :options kiss-valid-sudo-like-utils)

(defcustom kiss-valid-elf-utils
  '("readelf" "eu-readelf" "llvm-readelf" "ldd")
  "List of valid readelf-like utilities for kiss.el"
  :type '(string))

(defcustom kiss-elf
  (car (seq-filter #'executable-find kiss-valid-elf-utils))
  "The utility that will be used to dump elf information."
  :type 'string
  :options kiss-valid-elf-utils)

(defcustom kiss-valid-compress
  '("" "bz2" "gz" "lz" "lzma" "xz" "zst")
  "List of valid options for 'kiss-compress'."
  :type '(string))

(defcustom kiss-compress
  "gz"
  "The compression algorithm that should be used when making packages."
  :type 'string
  :options kiss-valid-compress)

(defcustom kiss-compress-alist
  '((""     . "cat")
    ("bz2"  . "bzip2 -c")
    ("gz"   . "gzip -c")
    ("lz"   . "lzip -c")
    ("lzma" . "lzma -cT0")
    ("xz"   . "xz -cT0")
    ("zst"  . "zstd -cT0"))
  "Association List for looking up the proper command for a given 'kiss-compress'."
  :type 'alist)

(defcustom kiss-decompress-alist
  `((,(rx ".tar" eol)             . "cat ")
    (,(rx (or ".tbz" ".bz2") eol) . "bzip2 -dc ")
    (,(rx ".lz" eol)              . "lzip -dc ")
    (,(rx (or ".tgz" ".gz") eol)  . "gzip -dc ")
    (,(rx ".lzma" eol)            . "lzma -dcT0 ")
    (,(rx (or ".txz" ".xz") eol)  . "xz -dcT0 ")
    (,(rx ".zst" eol)             . "zstd -dcT0 "))
  "Association List for determining how to decompress a tarball."
  :type 'alist)

(defcustom kiss-force
  nil
  "Set to t to force the package manager to skip certain sanity checks.

This is primarily used in the installation and removal process, to force
the package manager to execute the actions - even when dependencies
would be broken or not present on the system."
  :type 'boolean)

(defcustom kiss-choice
  1
  "Set to '0' disable the alternatives system and error on any file conflicts."
  :type 'integer)

;; TODO: make the default 1, once package stripping actually works.
(defcustom kiss-strip
  0
  "Set to '1' to enable the stripping of packages."
  :type 'integer)

(defcustom kiss-path
  (let ((kp (getenv "KISS_PATH"))) (when kp (split-string kp ":")))
  "A list of directories in decreasing precedence to look for packages in."
  :type '(string))

(defcustom kiss-hook
  (let ((kh (getenv "KISS_HOOK"))) (when kh (split-string kh ":")))
  "A list of absolute paths to executable files."
  :type '(string))

(defcustom kiss-make-chroot-strategy 'prohibit-user-alternatives
  "Denotes the strategy that kiss--make-chroot-dir-for-pkg will use."
  :type 'symbol
  :options '(permit-user-alternatives prohibit-user-alternatives))

(defcustom kiss-perfom-build-in-sandbox nil
  "Set to t if you want build to be performed in a sandbox."
  :type 'boolean)

(defcustom kiss-sandbox-utility "bwrap"
  "set to an executable for sandboxing.

Valid strings: bwrap, proot."
  :type 'string
  :options '("bwrap" "proot"))

;; Macros

(defmacro kiss--with-dir (dir-path expr)
  `(let ((default-directory ,dir-path))
     ,expr))

(ert-deftest kiss--with-dir ()
  (should
   (string= "/opt"
            (kiss--with-dir "/opt" default-directory))))

(defmacro kiss--update-repo-type (type)
  `(let ((repos
          (seq-uniq
           (mapcar (intern (concat "kiss--get-" (symbol-name ,type) "-dir-toplevel"))
                   (funcall
                    (intern (concat "kiss--kiss-path-" (symbol-name ,type) "-repos")))))))
     (dolist (repo repos)
       (kiss--with-dir
        repo
        (let ((repo-owner (kiss--get-owner-name repo))
              (am-owner-p (kiss--am-owner-p repo))
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

;; EIEIO Classes

(defclass kiss-source ()
  ((package
    :initarg :package
    :initform ""
    :type string
    :documentation "The package which this source is a source of.")
   (type
    :initarg :type
    :type symbol
    :options '(git remote local)
    :documentation "A symbol to determine what kind of source it is.")
   (uri
    :initarg :uri
    :type string
    :documentation "The URI for the kiss-source")
   ;; FIXME: implement the logic for 'SKIP'
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
        ((or 'git 'remote)
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
          ('hg (concat "hg clone -u " commit " " u))

          ('fossil (concat "fossil open -f " u " " commit))

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
  (string= (slot-value obj :checksum) (kiss--source-get-local-checksum obj)))

;; FIXME: write a test for this + support hg&fossil
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
                (not (string= commit-or-branch "HEAD"))
                (eq type 'git))
       (concat "@" commit-or-branch))
     (unless (string-empty-p extracted-path)
       (concat " " extracted-path)))))

(ert-deftest kiss--source-to-string ()
  (should
   (let* ((str-to-str (lambda (src) (kiss--source-to-string (kiss--string-to-source-obj src))))
          (src-to-str-test (lambda (str) (string= str (funcall str-to-str str)))))
     (and
      (funcall src-to-str-test "git+https://github.com/ehawkvu/clasp@musl path/")
      (funcall src-to-str-test "hg+https://github.com/ehawkvu/tsort.el tsort/")
      (funcall src-to-str-test "patches/fix.patch patches/")
      (funcall src-to-str-test "/usr/share/src/file")))))

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
    (setq extr-path (nth 1 (string-split str " " t)))
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

(ert-deftest kiss--string-to-source-obj ()
  ;; Git source w/ branch
  (should
   (with-slots
       ((package          :package)
        (type             :type)
        (uri              :uri)
        (checksum         :checksum)
        (extracted-path   :extracted-path)
        (commit-or-branch :commit-or-branch))
       (kiss--string-to-source-obj "git+https://github.com/ehawkvu/clasp@musl")
     (and
      (string-empty-p package)
      (eq type 'git)
      (string= uri "https://github.com/ehawkvu/clasp")
      (string-empty-p checksum)
      (string-empty-p extracted-path)
      (string= "musl" commit-or-branch))))
  ;; Remote source w/ extracted path & package
  (should
   (let ((obj
          (kiss--string-to-source-obj
           "https://github.com/llvm/llvm-project/releases/download/llvmorg-17.0.5/llvm-17.0.5.src.tar.xz llvm")))
     (oset obj :package "llvm")
     (with-slots
         ((package          :package)
          (type             :type)
          (uri              :uri)
          (checksum         :checksum)
          (extracted-path   :extracted-path)
          (commit-or-branch :commit-or-branch))
         obj
       (and
        (string= package "llvm")
        (eq type 'remote)
        (string= uri "https://github.com/llvm/llvm-project/releases/download/llvmorg-17.0.5/llvm-17.0.5.src.tar.xz")
        (string-empty-p checksum)
        (string= extracted-path "llvm")
        (string-empty-p commit-or-branch))))))

(defun kiss--sources-file-to-sources-objs (file-path)
  (mapcar #'kiss--string-to-source-obj (kiss--read-file file-path)))

(defun kiss--def-pkg-sources (name &rest arguments)
  (let ((src-objs (mapcar #'kiss--string-to-source-obj arguments)))
    (mapc (lambda (obj) (oset obj :package name)) src-objs)
    src-objs))

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
    :optional
    )
   (depends
    :initarg :depends
    :initform '()
    :type list
    :custom list
    :documentation "List of packages that are dependencies of this package."
    :optional
    )
   (make-depends
    :initarg :make-depends
    :initform '()
    :type list
    :custom list
    :documentation "List of packages that are make depends of this kiss-package."
    :optional
    )
   (sources
    :initarg :sources
    :initform '()
    :type list
    :custom list
    :documentation "List of kiss-source objects for the kiss-package"
    :optional
    )
   )
  )

(cl-defmethod kiss--package-bin-name ((pkg kiss-package))
  "(I) Return the proper name for the binary for PKG at VERSION."
  (with-slots ((v :version) (r :release) (n :name)) pkg
    (concat n "@" v "-" r ".tar"
            (unless (string-empty-p kiss-compress)
              (concat "." kiss-compress)))))

(defun kiss--dir-to-kiss-package (dir-path)
  (let ((name          (kiss--basename dir-path))
        (build-file    (concat dir-path "/build"))
        (ver-file      (concat dir-path "/version"))
        (source-file   (concat dir-path "/sources"))
        (depends-file  (concat dir-path "/depends"))
        (checksum-file (concat dir-path "/checksums"))
        ;; FIXME: post-install & pre-remove

        (obj nil)

        (ver  nil)
        (rel   nil)
        (srcs  nil)
        (deps  nil)
        (mdeps nil)
        )

    (setq ver (car (string-split (car (kiss--read-file ver-file)) " " t)))
    (setq rel (cadr (string-split (car (kiss--read-file ver-file)) " " t)))

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
      (let ((read-data (kiss--read-file depends-file)))

        (setq deps
              (seq-remove
               (lambda (str) (string-match-p (rx (1+ any) (1+ " ") "make") str))
               read-data))

        (setq mdeps
              (mapcar
               (lambda (str)
                 (replace-regexp-in-string (rx (1+ " ") (0+ any) eol) "" str))
               (seq-difference read-data deps))))
      (when deps  (oset obj :depends deps))
      (when mdeps (oset obj :make-depends mdeps)))

    ;; TODO: make all local sources absolute paths.
    ;; We obviosly still support the notion of relative paths in the
    ;; build files, but we may as well save ourselves the lookup
    ;; when it comes to actually building the package.

    (when (kiss--file-exists-p checksum-file)
      (let ((checksum-data
             (kiss--read-file checksum-file))
            (non-git-src-objs
             (seq-remove (lambda (o) (eq (slot-value o :type) 'git))
                         (slot-value obj :sources))))

        ;; TODO: will need to update this anytime we add support for another
        ;; remote source.
        (dotimes (i (length checksum-data))
          (oset (nth i non-git-src-objs) :checksum (nth i checksum-data)))))
    obj))

(defun kiss--package-to-dir (package-obj dir)
  (with-slots
      ((name         :name)
       (version      :version)
       (release      :release)
       (build-file   :build-file)
       (depends      :depends)
       (make-depends :make-depends)
       (sources      :sources))
      package-obj
    (let ((version-str (concat version " " release "\n"))
          (depends-str
           (concat
            (mapconcat
             #'identity
             (sort
              (append depends (mapcar (lambda (s) (concat s " make")) make-depends))
              #'string-lessp)
             "\n")
            "\n"))
          (sources-str
           (concat
            (mapconcat
             #'identity (mapcar #'kiss--source-to-string sources) "\n")
            "\n"))
          (checksum-str
           (concat
            (mapconcat
             #'identity
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
           (copy-file build-file (concat name "/build"))
           (when local-sources
             (dolist (file local-sources)
               (make-directory (concat name "/" (kiss--dirname file)) t)
               (copy-file (concat build-file-dir "/" file) (concat name "/" file))))))))))

(defun kiss--def-build-file (build-type) ;;&optional &rest args)
  "Return the proper shell commands to perform a build with BUILD-TYPE."
  (mapconcat
   #'identity
   (append
    '("#!/bin/sh -e"
      "export DESTDIR=$1")
    (pcase build-type
      ('meson
       '("meson setup -Dprefix=/usr output"
         "ninja -C output"
         "ninja -C output install"))
      ('cmake
       '("cmake -B build -DCMAKE_INSTALL_PREFIX=/usr"
         "cmake --build build"
         "cmake --install build"))
      ('gnu
       '("./configure --prefix=/usr --sysconfdir=/etc"
         "make"
         "make install"))
      ('cargo
       '("cargo fetch"
         "cargo build --release"
         "mkdir -p $1/usr/bin/"
         "cp target/release/* $1/usr/bin/"))
      (_ (error (concat (symbol-name build-type) " is not supported!")))))
   "\n"))

;;(slot-value (kiss--dir-to-kiss-package (car (kiss-search "kiss"))) :sources)

;; ===========================================================================

;; Internal function definitions, these are considered to not be stable.
;; It's best not to rely on them outside of this file.

(defun kiss--normalize-file-path (file-path)
  "(I) Normalize the number of '/' in FILE-PATH."
  (replace-regexp-in-string (rx (1+ "/") "/") "/" file-path))

(ert-deftest kiss--normalize-file-path ()
  (should
   (and
    (string= "/opt" (kiss--normalize-file-path "/opt"))
    (string= "/opt" (kiss--normalize-file-path "//opt"))
    (string= "/opt/kiss/test/here/"
             (kiss--normalize-file-path "//opt/kiss//test/here//")))))

(defun kiss--lst-to-str (lst)
  "(I) Convert LST to a string."
  (mapconcat (lambda (s) (format "%s" s)) lst " "))

(defun kiss--sanitize-ver-str (str)
  "(I) Sanitize a version string STR to be correctly compared against others."
  (thread-last
    str
    (replace-regexp-in-string " " "")
    (replace-regexp-in-string "\n$" "")))

(defun kiss--write-text (text encoding file-path)
  (with-temp-file file-path
    (insert (format "%s" text))))

(defun kiss--read-text (file-path)
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun kiss--shell-command (command)
  ;; Wrapper over 'shell-command' that prevents a ton of messages being
  ;; printed.
  (let ((inhibit-message t)
        (message-log-max nil))
    (shell-command command)))

;; FIXME: impl this which can take a list of arbitrary
;; length commands and execute them all in one fell swoop
;; by making a shell script that will execute them.
;; Naturally, we just return the exit code of the
;; shell script.
(defun kiss--shell-commands (command-lst)

  nil)

;; FIXME: potentially change this function
;; to simply remove the final newline at the end of the text
;; and to then split on the newlines - it should result in the same
;; code as the following for kiss-manifest, but would allow this
;; code to be used other places too
(defun kiss--read-file (file-path)
  "(I) Read FILE-PATH as a list of lines, with empty newlines removed."
  (when (kiss--file-exists-p file-path)
    (seq-remove
     (lambda (s) (string= "" s))
     (string-split (kiss--read-text file-path) "\n"))))

(defun kiss--get-user-from-uid (uid)
  "(I) Return the name for UID.  `$ getent passwd' is parsed for the information."
  (pcase system-type
    ('darwin
     ;; macOS ships a working version of id.
     (shell-command-to-string (concat "id -un " uid)))
    ((or 'gnu/linux 'berkley-unix)
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

(defun kiss--get-uid-from-user (user)
  (string-to-number
   (shell-command-to-string (concat "id -u " user))))

(ert-deftest kiss--uid ()
  (let ((user "root"))
    (should
     (and
      (string=
       (user-login-name)
       (kiss--get-user-from-uid (user-uid)))
      (eq
       (user-uid)
       (kiss--get-uid-from-user (user-login-name)))
      (string= user
               (kiss--get-user-from-uid
                (kiss--get-uid-from-user user)))))))

(defun kiss--get-owner (file-path)
  "(I) Return the owner uid of FILE-PATH."
  (file-attribute-user-id (file-attributes file-path)))

(defun kiss--get-owner-name (file-path)
  "(I) Return the owner name of FILE-PATH."
  (kiss--get-user-from-uid
   (kiss--get-owner file-path)))

(defun kiss--am-owner-p (file-path)
  "(I) Return t if the current LOGNAME owns the FILE-PATH, nil otherwise."
  (eql
   (user-real-uid)
   (kiss--get-owner file-path)))

(defun kiss--shell-command-as-user (command user)
  "(I) Run COMMAND as USER using `kiss-su'."
  (shell-command (concat kiss-su " -u " user " -- " command)))

(defun kiss--get-decompression-command (file-path)
  (let ((matched-rgx
         (thread-last
           kiss-decompress-alist
           (mapcar #'car)
           (seq-filter (lambda (rgx) (string-match-p rgx file-path)))
           (car))))
    (cdr (assoc matched-rgx kiss-decompress-alist))))

(ert-deftest kiss--get-decompression-command ()
  (let ((examples
         '((".tar"  . "cat ")
           (".tbz"  . "bzip2 -dc ")
           (".bz2"  . "bzip2 -dc ")
           (".lz"   . "lzip -dc ")
           (".tgz"  . "gzip -dc ")
           (".gz"   . "gzip -dc ")
           (".lzma" . "lzma -dcT0 ")
           (".txz"  . "xz -dcT0 ")
           (".xz"   . "xz -dcT0 ")
           (".zst"  . "zstd -dcT0 "))))
    (should
     (thread-last
       examples
       (mapcar
        (lambda (pair)
          (string= (cdr pair)
                   (kiss--get-decompression-command (car pair)))))
       (funcall (lambda (l) (seq-reduce (lambda (x y) (and x y)) l t)))))))

(defun kiss--decompress (file-path out-path)
  "(I) Decompress FILE-PATH to OUT-PATH based on the file name."
  (let ((cmd (kiss--get-decompression-command file-path)))
    (when cmd
      (shell-command (concat cmd file-path " > " out-path)))))

(defun kiss--b3 (file-path)
  "(I) Run b3sum on FILE-PATH."
  (thread-last
    file-path
    (concat "b3sum -l 33 ")
    (shell-command-to-string)
    (replace-regexp-in-string "\n$" "")
    (funcall (lambda (str) (string-split str " ")))
    (car)))

(defun kiss--sh256 (file-path)
  "(I) Run `kiss-chk' with proper arguments on FILE-PATH."
  (let ((args
         (pcase kiss-chk
           ("openssl"   " dgst -sha256 -r ")
           ("sha256sum" "")
           ("sha256"    " -r ")
           ("shasum"    " -a 256 ")
           ("digest"    " -a sha256 "))))
    (thread-last
      (concat kiss-chk args file-path)
      (shell-command-to-string)
      (replace-regexp-in-string "\n$" "")
      (string-split)
      (car))))

;; Public code below.

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
      (shell-command (format "%s %s %s %s %s" kh hook arg2 arg3 arg4)))))

(defun kiss--run-hook-pkg (hook pkg)
  "(I) Run PKG's HOOK."
  (let ((hook-fp (concat kiss-installed-db-dir pkg "/" hook)))
    (when (kiss--file-is-executable-p hook-fp)
      ;; FIXME: need to expose the proper environment to this shell
      (with-environment-variables
          (("KISS_ROOT" kiss-root))
        (kiss--shell-command-as-user hook-fp (kiss--get-owner kiss-root))))))

;; -> kiss [a|b|c|d|i|l|p|r|s|u|U|v] [pkg]...
;; -> alternatives List and swap alternatives
;; ===========================================================================

;;;###autoload
(defun kiss-alternatives (&optional pkg path)
  (interactive)
  (if (or (eq nil pkg) (eq nil path))
      (mapcar
       (lambda (s)
         (let ((d (split-string s ">")))
           (list (car d)
                 (concat "/" (string-join (cdr d) "/"))
                 s)))
       (nthcdr 2 (directory-files kiss-choices-db-dir)))
    (kiss--pkg-swap pkg path)))

;; (benchmark-elapse (kiss-alternatives))
;; (kiss-alternatives "util-linux" "/usr/bin/mkswap")
;; (kiss-alternatives "busybox" "/usr/bin/mkswap")
;; (kiss-alternatives)

;; FIXME?: may need to have this code take in a general path
;; for pkg - this is so that this code can be reused to implement
;; the conflicts system.
;; pkg_manifest_replace() in kiss
(defun kiss--pkg-manifest-replace (pkg old new)
  "(I) Replace the line matching OLD in the manifest of PKG with NEW."
  ;; Replace the matching line in the manifest w/ the desired
  ;; replacement.
  (let* ((manifest-f (concat kiss-installed-db-dir pkg "/manifest"))
         (temp-f     (kiss--make-temp-file))
         (owner      (kiss--get-owner-name manifest-f))
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

;; (kiss--manifest-to-string (kiss-manifest "xdo"))

;; FIXME: impl kiss--file-executable-p
(defun kiss--file-is-executable-p (file-path)
  "(I) Return T if FILE-PATH exists and is executable."
  (eq 0 (shell-command (concat "test -x "
                               (kiss--single-quote-string file-path)))))

;; TODO: Look into rm'ing these funcs since they should not have to exist.
(defun kiss--file-is-regular-file-p (file-path)
  "(I) Return T if FILE-PATH exists and is a regular file."
  (eq 0 (shell-command (concat "test -f "
                               (kiss--single-quote-string file-path)))))

(defun kiss--file-is-symbolic-link-p (file-path)
  "(I) Return T if FILE-PATH exists and is a symbolic link."
  (eq 0 (shell-command (concat "test -h "
                               (kiss--single-quote-string file-path)))))

(defun kiss--file-is-directory-p (file-path)
  "(I) Return T if FILE-PATH exists and is a directory."
  (eq 0 (shell-command (concat "test -d "
                               (kiss--single-quote-string file-path)))))

(defun kiss--file-identify (file-path)
  "(I) Identify FILE-PATH as a symbol representing what kind of file it is."
  (cond
   ((kiss--file-is-directory-p     file-path) 'directory)
   ((kiss--file-is-symbolic-link-p file-path) 'symlink)
   ((kiss--file-is-regular-file-p  file-path) 'file)))

;; FIXME: go back through the code and make this also check if a directory
;; exists as well.
;; NOTE: DO NOT USE THIS ANYWHERE THAT ISN'T ABSOLUTELY NECESSARY.
(defun kiss--file-exists-p (file-path)
  "(I) This function should NOT exist.
However, `file-exists-p' and `file-symlink-p' are fundamentally broken when it
comes to broken symlinks.  Hence the need for this function.
This function returns t if FILE-PATH exists and nil if it doesn't."
  (or
   (kiss--file-is-directory-p file-path)
   (kiss--file-is-regular-file-p file-path)
   (kiss--file-is-symbolic-link-p file-path)))


(defun kiss--single-quote-string (str)
  "(I) Add quotes around STR.  Useful when interacting with the cli."
  (concat "'" str "'"))

(ert-deftest kiss--single-quote-string ()
  (should (string= "'as df'" (kiss--single-quote-string "as df"))))

(defun kiss--pkg-swap (pkg path)
  "(I) Swap the owner of PATH to PKG, modifying the relevant package manifests."
  (if (kiss--pkg-is-installed-p pkg)
      ;; NOTE: The quotes surrounding the string are very important.
      ;; This is because this string is only interpreted as a command argument.
      ;; This means that the shell can mangle it if it is not properly
      ;; esacped.
      (let* ((alt      (string-replace "/" ">" path))
             (alt-path (concat kiss-choices-db-dir pkg alt))
             (path-own (kiss-owns path)))
        (if (kiss--file-exists-p  alt-path)
            (progn
              ;; If the file is owned by a package in the database.
              (if path-own
                  (progn
                    (message (concat "Swapping " path
                                     " from " path-own
                                     " to " pkg))
                    ;; Save the path into kiss-choices-db-dir
                    (kiss--shell-command-as-user
                     (concat "cp -Pf " path " "
                             (kiss--single-quote-string
                              (concat kiss-choices-db-dir path-own alt)))
                     (kiss--get-owner-name path))

                    ;; Update the manifest file to reflect the new version.
                    (kiss--pkg-manifest-replace
                     path-own path (concat kiss-choices-db-dir path-own alt))))
              ;; Move over our new desired alternative to the real file.
              (kiss--shell-command-as-user
               (concat "mv -f " (kiss--single-quote-string alt-path)
                       " " path)
               (kiss--get-owner-name path))
              (kiss--pkg-manifest-replace pkg alt-path path))))))

;; (f-symlink-p "/var/db/kiss/choices/gawk\\>usr\\>bin\\>awk")
;; (f-exists?  "/var/db/kiss/choices/busybox\\>usr\\>bin\\>sh")
;; (kiss--file-exists-p "/var/db/kiss/choices/gawk\\>usr\\>bin\\>awk")
;; (if (kiss-owns "/usr/bin/awk") 1)
;; (cl-remove-if-not
;;  (lambda (s) (string= "gawk" s))
;;  (cl-mapcar #'car (kiss-alternatives)))
;; (concat "mawk" (string-replace "/" ">" "/usr/bin/awk"))

(defun kiss--manifest-to-string (pkg-manifest)
  "(I) Convert our internal representation of PKG-MANIFEST into a string."
  (concat (mapconcat #'identity pkg-manifest "\n") "\n"))

;; (kiss--manifest-to-string (kiss-manifest "xdo"))

;;;###autoload
(defun kiss-manifest (pkg)
  "Return a list of all files owned by PKG."
  (kiss--read-file
   (concat kiss-installed-db-dir pkg "/manifest")))

;; (benchmark-elapse (kiss-manifest "kiss"))

(defun kiss--get-installed-manifest-files ()
  "(I) Return a list of all of the installed manifest files."
  (mapcar
   (lambda (pkg) (concat kiss-installed-db-dir pkg "/manifest"))
   (mapcar 'car (kiss-list))))

;; FIXME: would like to remove the reliance on 'grep' for these functions.
;;;###autoload
(defun kiss-owns (file-path)
  ;; TODO: See if this can be made a little less ugly.
  (let* ((cmd (concat "grep " (rx bol (literal file-path) eol) " "
                      (kiss--lst-to-str
                       (kiss--get-installed-manifest-files))))
         (cmd-out (shell-command-to-string cmd)))
    (unless (string-empty-p cmd-out)
      (car
       (string-split
        (replace-regexp-in-string kiss-installed-db-dir "" cmd-out) "/")))))

;; (cl-mapcar
;;  (lambda (file)
;;    (list (kiss-owns file) file))
;;  (delete-dups (cl-mapcar #'cadr (kiss-alternatives))))

;; (rgrep "/usr/bin/awk$" "manifest" "/var/db/kiss/installed/")

;; FIXME: this function will include '("")  in the return.
;;;###autoload
(defun kiss-preferred ()
  (mapcar
   ;; NOTE: this may split files with ':' in the name...
   ;; Do the final split of package and file.
   (lambda (s) (string-split s ":"))
   ;; Split up each line from each other.
   (string-split
    ;; Clean up the string to just contain the package name
    ;; on the left and the file on the right.
    (replace-regexp-in-string
     kiss-installed-db-dir ""
     (replace-regexp-in-string
      "/manifest:" ":" ;; NOTE: change this to be a diff character


      (shell-command-to-string
       (concat
        ;; Use printf since it's the closest thing we have to being
        ;; able to pipe without using mkfifo(1)
        "printf '"
        ;; TODO: think about factoring this code out.
        (mapconcat
         #'identity
         (seq-uniq
          (mapcar
           (lambda (file-str)
             (concat
              "/"
              (mapconcat #'identity
                         (cdr (split-string file-str ">"))
                         "/")))
           (nthcdr 2 (directory-files kiss-choices-db-dir))))
         "\\n")
        "'"
        ;; Now for the piping into grep.
        " | "
        "grep -Fxf - "
        (kiss--lst-to-str
         (kiss--get-installed-manifest-files))
        " /dev/null"))))
    "\n")))

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

(defun kiss--get-pkg-dependencies (pkg &optional installed-p)
  "(I) Get the dependencies of PKG as a list, nil if PKG has no dependencies.

Optionally, if INSTALLED-P is t, then the system installed package will be
read instead."
  ;; Faster (older) version of the function here.
  (kiss--get-dependencies-from-file
   (if (and installed-p (kiss--pkg-is-installed-p pkg))
       (concat kiss-installed-db-dir pkg "/depends")
     (concat (car (kiss-search pkg)) "/depends")))

  ;; (with-slots
  ;;     ((hd :depends)
  ;;      (md :make-depends))
  ;;     (kiss--dir-to-kiss-package
  ;;      (if (and installed-p (kiss--pkg-is-installed-p pkg))
  ;;          (concat kiss-installed-db-dir pkg)
  ;;        (car (kiss-search pkg))))
  ;;   (append md hd))
  )


;; FIXME: have this tak a list of package objects??
(defun kiss--get-pkg-dependency-graph (pkg-lst &optional installed-p)
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
                        (kiss--get-pkg-dependencies dep t)
                      (kiss--get-pkg-dependencies dep)))
                   (item `(,dep ,dep-deps)))
              (if (not (member item res))
                  (setq res (cons item res)))
              (setq queue (append dep-deps (cdr queue)))))))
    res))

;; FIXME: implement installed-p argument here.
(defun kiss--get-pkg-dependency-graph-rec (pkg-lst)
  "(I) A Recursive & TCO-ed implementation of `kiss--get-pkg-dependency-graph'.

This version of the function is meant primarily as a resource for people
looking to implement kiss in other functional programming languages.
It is also important to note that there is no meaninful decrease in speed
when using this function compared with the iterative version."
  (let* ((pkgs (cond
                ((atom pkg-lst) `(,pkg-lst))
                ((listp pkg-lst) pkg-lst))))

    (named-let kiss--get-pkg-dependency-graph-rec-impl
        ((queue pkgs) (seen '()) (res '()))
      (let* ((new-queue (seq-remove (lambda (e) (member e seen)) queue))
             (dep (car new-queue)))
        (if dep
            (let* ((new-seen (cons dep seen))
                   (dep-deps (kiss--get-pkg-dependencies dep))
                   (item `(,dep ,dep-deps))
                   (new-res (if (not (member item res)) (cons item res) res)))
              (kiss--get-pkg-dependency-graph-rec-impl
               (append dep-deps (cdr new-queue)) new-seen new-res))
          res)))))

;; (benchmark-elapse (kiss--get-pkg-dependency-graph (mapcar #'car (kiss-list))))
;; (benchmark-elapse (kiss--get-pkg-dependency-graph-rec (mapcar #'car (kiss-list))))

(defun kiss--get-pkg-dependency-order (pkg-lst &optional installed-p)
  "(I) Return the proper build order of the dependencies for each pkg in PKG-LST."
  ;; FIXME: need to do some error checking here w/ tsort
  (tsort
   (if installed-p
       (kiss--get-pkg-dependency-graph pkg-lst t)
     (kiss--get-pkg-dependency-graph pkg-lst))))

(defun kiss--remove-redundant-dependencies (dep-lst)
  "(I) Remove redundant dependencies from DEP-LST."
  (seq-remove
   (lambda (pkg)
     (member pkg
             (thread-last
               dep-lst
               (mapcar #'kiss--get-pkg-dependencies)
               (flatten-list)
               (seq-uniq))))
   dep-lst))

(defun kiss--get-pkg-make-dependents (pkg)
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
;; (kiss--get-pkg-make-dependents "ant")

(defun kiss--get-pkg-make-orphans ()
  "(I) Return a list of packages which only have make dependents."
  ;; NOTE: This function is pretty slow at the moment.
  (seq-filter
   (lambda (pkg)
     (and (eq (kiss--get-pkg-hard-dependents pkg) nil)
          (not
           (eq (kiss--get-pkg-make-dependents pkg) nil))))
   (mapcar #'car (kiss-list))))

;; (kiss--get-pkg-make-orphans)
;; (length (kiss--get-pkg-make-orphans))

(defun kiss--get-pkg-hard-dependents (pkg)
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

;; (kiss--get-pkg-hard-dependents "mpfr")

(defun kiss--get-pkg-missing-dependencies (pkg)
;; FIXME: have this function take a package object
  "(I) Return a list of dependencies that are missing for PKG, nil otherwise."
  (seq-remove
   #'kiss--pkg-is-installed-p
   (seq-uniq
    (flatten-list
     (mapcar #'cadr
             (kiss--get-pkg-dependency-graph pkg))))))

;; (kiss--get-pkg-missing-dependencies "gimp")
;; (kiss--get-pkg-missing-dependencies "gcc")

(defun kiss--get-pkg-orphan-alternatives (pkg)
  "(I) Return a list of orphaned alternatives that would result from removing PKG."
  (let ((orphaned-alternatives
         (seq-filter
          (lambda (pair) (seq-contains-p pair pkg))
          (kiss-preferred))))
    ;; Return the files associated with PKG.
    (if orphaned-alternatives
        (mapcar #'cadr orphaned-alternatives))))

;; (benchmark-elapse (kiss--get-pkg-make-dependents "meson"))

(defun kiss--pkg-is-removable-p (pkg)
  "(I) Return t if PKG is removable, nil otherwise."

  ;; A package is removable when the following conditions are met:
  ;; * the pkg is installed on the current system
  ;; * nothing on the system hard depends on the package
  ;; * the package does not leave any orphaned alternatives
  (and
   (kiss--pkg-is-installed-p pkg)
   (eq (kiss--get-pkg-hard-dependents pkg) nil)
   (eq (kiss--get-pkg-orphan-alternatives pkg) nil)))

;; -> build        Build packages
;; ===========================================================================

;; Just some testing code to get all of the missing dependencies for the
;; pkgs to be updated.
;;(kiss--get-pkg-order
;; (flatten-list
;; (seq-remove
;;  (lambda (i) (eq i nil))
;;  (mapcar #'kiss--get-pkg-missing-dependencies
;;          (cons "freecad" (cons "gimp" (kiss--get-out-of-date-pkgs))))))

(defun kiss--get-manifest-for-dir (dir)
  "(I) Return a kiss compatible manifest for DIR."
  (let ((files-and-dirs
         ;; find cmd
         (mapcar
          (lambda (file-path)
            (let ((cfp (replace-regexp-in-string dir "" file-path)))
              (cond
               ((kiss--file-is-directory-p file-path) (concat cfp "/"))
               (t cfp ))))

          ;; Filter out libtool .la files and charset.alias
          (seq-remove
           (lambda (fp)
             (string-match-p
              (rx (or (literal "charset.alias") (literal ".la")) eol)
              fp))
           (directory-files-recursively dir "." t)))))

    ;; Admittedly I'm not a huge fan of the below code, but it does work.
    (if (member "/var/db/kiss/installed/" files-and-dirs)
        (let ((pkg-db-dir
               (car (seq-filter
                     (lambda (s)
                       (string-match-p
                        (rx "/var/db/kiss/installed/" (1+ (not "/")) "/" eol)
                        s))
                     files-and-dirs))))
          ;; FIXME: this doesn't seem to work at the moment (at least etcsums)
          ;; add /var/db/kiss/installed/<pkg>/manifest
          ;; and /var/db/kiss/isntalled/<pkg>/etcsums
          (if (member "/etc" files-and-dirs)
              (setq files-and-dirs
                    (cons (concat pkg-db-dir "etcsums") files-and-dirs)))
          (setq files-and-dirs
                (cons (concat pkg-db-dir "manifest") files-and-dirs))))

    ;; sort -ur
    (reverse
     (seq-sort 'string-lessp (seq-uniq files-and-dirs)))))

(defun kiss--dir-matches-manifest-p (dir manifest-file)
  "(I) Return t or nil depending on whether a DIR matches MANIFEST-FILE."
  (equal (kiss--get-manifest-for-dir dir)
         (kiss--read-file manifest-file)))

;; FIXME: rm this function eventually
(defun kiss--get-pkg-version (pkg)
  "(I) Get the version for PKG using the car of `kiss-search'."
  (with-slots ((v :version) (r :release))
      (kiss--dir-to-kiss-package (car (kiss-search pkg)))
    (concat v " " r)))

;; FIXME: rm this function and use 'kiss--package-bin-name'
(defun kiss--get-pkg-bin-name (pkg version)
  "(I) Return the proper name for the binary for PKG at VERSION."
  (concat pkg "@"
          (replace-regexp-in-string " " "-" (string-trim-right version))
          ".tar" (unless (string-empty-p kiss-compress)
                   (concat "." kiss-compress))))

(defun kiss--get-compression-command ()
  "(I) Return the proper command based on `kiss-compress'."
  (cdr (assoc kiss-compress kiss-compress-alist)))

(defun kiss--get-pkg-cached-bin (pkg)
  "(I) Return the path of the binary for PKG, nil if binary is not in the cache."
  (let ((ver (kiss--get-pkg-version pkg)))
    (if ver
        (let ((bin (concat kiss-bindir
                           (kiss--get-pkg-bin-name pkg ver))))
          (if (file-exists-p bin) bin)))))

(defun kiss--get-random-number (&optional upper-bound)
  "(I) Number from 1 to UPPER-BOUND, exclusive. Default UPPER-BOUND is 30000."
  (message "%s" (mod (abs (random)) (if upper-bound upper-bound 30000))))

(defun kiss--get-tmp-destdir ()
  "(I) Return a directory that can be used as a temporary destdir."
  ;; NOTE: This is not a *perfect* system, however, it is not as easy to
  ;; do the pid trick that the shell implementation of kiss does.
  ;; So the compromise is to pick a random number from 1 to 30000.
  (let ((rn (kiss--get-random-number)))
    (while (file-exists-p (concat kiss-tmpdir rn))
      (setq rn (kiss--get-random-number)))
    (make-directory (concat kiss-tmpdir rn) t)
    (concat kiss-tmpdir rn)))

;;;###autoload
(defun kiss-fork (pkg dir)
  "Fork PKG to DIR."
  (eq 0 (shell-command
         (concat "cp -Lrf " (car (kiss-search pkg)) " " dir))))

(defun kiss--make-tarball-of-dir (dir file-path)
  "(I) Make a compressed tarball of DIR saved into FILE-PATH."
  (kiss--with-dir
   dir
   (eq 0
       (shell-command
        (concat "tar -cf -  . | "
                (kiss--get-compression-command)
                " > " (kiss--single-quote-string file-path))))))

(defun kiss--get-potential-binary-files (file-path-lst)
  "(I) Return a list of files in FILE-PATH-LST that `strip` or `ldd` could use."
  (seq-filter
   (lambda (file-path)
     (string-match-p
      (rx
       (0+ any)
       (or
        (: "/sbin")
        (: "/bin")
        (: "/lib" (? (** 2 4 any))))
       "/"
       (0+ any)
       (1+ (not "/")) eol)
      file-path))
   file-path-lst))

(defun kiss--get-potential-binary-files-fast (file-path-lst)
  "A *much* faster version of kiss--get-potential-binary-files.

Takes the same argument as the other function, a FILE-PATH-LST.

Not yet the default, since I have not yet been able to confirm that
this implementation will always result in exactly the same files
as the other implementation.  It is important to note though, that at
least with chromium, as well as some other test packages, the lists
are the same."
  ;; First, we need to detect all of the potenial names for 'lib'
  ;; This inolves us doing a pretty liberal string match, which
  ;; after we get the relevant lines, we extract the symbol name
  ;; that we are interested in.
  (let ((lib-symbols
         (seq-uniq
          (mapcar
           #'intern
           (flatten-list
            (mapcar (lambda (l)
                      (seq-filter
                       (lambda (s) (string-match-p "lib.?.?.?.?" s)) l))
                    (mapcar
                     (lambda (s) (string-split s "/"))
                     (seq-filter
                      (lambda (s) (string-match-p "/lib.?.?.?.?/$" s))
                      file-path-lst))))))))
    (let ((found-files
           ;; This is the code to approximate the rx or match.
           (seq-filter (lambda (l)
                         (and
                          (or
                           (seq-contains-p l 'sbin)
                           (seq-contains-p l 'bin)
                           ;; NOTE: may ned to double check the logic
                           ;; on this remove - may need to actually
                           ;; check for this to be non-nil?
                           (remove nil (mapcar (lambda (libs) (member libs l))
                                               lib-symbols)))
                          (not (eq '## (car (reverse l))))))

                       ;; Here is where the big speed up is.
                       ;; Instead of comparing strings, which is slow
                       ;; all the time always (except maybe in snobol4...)
                       ;; we convert our file path strings into lists
                       ;; of symbols, which are much, much faster to act on.
                       (mapcar
                        (lambda (fp) (mapcar #'intern (cdr (string-split fp "/"))))
                        file-path-lst))))
      (mapcar
       (lambda (ff) (concat "/" (mapconcat #'symbol-name ff "/")))
       found-files))))


(defun kiss--build-make-script (k-el-build build-script build-dir install-dir pkg-ver log-file)

  ;; Essentially, we want to build out a script that contains

  ;; all of the info that we need.

  ;; * Environment to build the package in
  ;; * The package build script
  ;; * The proper arguments to said pkg build script

  ;; ~/.cache/kiss/logs/$(date +%Y-%m-%d)/<pkg>-$(date +%Y-%m-%d-%H:%M)-<procnum>
  (kiss--write-text
   (concat
    "#!/bin/sh -xe\n"

    ;; cd into the build dir so our env is correct.
    "cd " build-dir " \n"

    "export AR=${AR:-ar} \n"
    "export CC=${CC:-cc} \n"
    "export CXX=${CXX:-c++} \n"
    "export NM=${NM:-nm} \n"
    "export RANLIB=${RANLIB:-ranlib} \n"
    "export RUSTFLAGS=\"--remap-path-prefix=$PWD=. $RUSTFLAGS\" \n"
    "export GOFLAGS=\"-trimpath -modcacherw $GOFLAGS\" \n"
    "export GOPATH=\"$PWD/go\" \n"
    "export DESTDIR=\"" install-dir "\" \n"
    ;; TODO: this might not be explicitly needed at this part?
    ;; "export KISS_ROOT=" (getenv "KISS_ROOT") "\n"

    ;; TODO: see if I can't implement something like this?
    ;; https://codeberg.org/kiss-community/repo/issues/121
    ;; thinking of doing kiss-build-env-hooks, which will take the package name

    build-script " " install-dir " " pkg-ver " > " log-file)
   ;; Write this script to a temporary location.
   'utf-8 k-el-build)
  ;; Mark the script as executable.
  (shell-command (concat "chmod +x " (kiss--single-quote-string k-el-build))))

;; NOTE: this function will need to be updated as kiss itself updates the
;; equivalent functionality.
(defun kiss--lib-is-system-p (lib-string)
  "(I) Return T if the lib indicated by LIB-STRING is expected on a posix system."
  (string-match-p
   (rx
    (or
     (: "ld-" (0+ any))
     (: "ldd")
     (:
      (or
       (: "lib" (or "c" "m"))
       (: "libc++")
       (: "libc++abi")
       (: "libcrypt")
       (: "libdl")
       (: "libgcc_s")
       (: "libmvec")
       (: "libpthread")
       (: "libresolv")
       (: "librt")
       (: "libstdc++")
       (: "libtrace")
       (: "libunwind")
       (: "libutil")
       (: "libxnet"))
      ".so" (0+ any))))
   lib-string))

(defun kiss--build-get-missing-dependencies (dir file-path-lst package)
  ;; NOTE: only works w/ ldd.
  (seq-difference
   (thread-last
     ;; dir
     ;; (kiss--get-manifest-for-dir)
     ;; (kiss--get-potential-binary-files)
     file-path-lst
     (mapcar (lambda (fp) (shell-command-to-string (concat "ldd " dir "/" fp))))
     (string-join)
     (string-split)
     (seq-filter (lambda (s) (string-match-p (rx ".so") s)))
     (mapcar #'kiss--basename)
     (seq-uniq)
     (seq-remove #'kiss--lib-is-system-p)
     ;; Now we have to figure out where the lib is on the system.
     (mapcar
      ;; FIXME: It would be nice to use kiss's algo for this part
      ;; since I don't know how portable this technique is when you are
      ;; using kiss as a personal package manager
      (lambda (lib) (shell-command-to-string (concat "cc -print-file-name=" lib))))
     (mapcar (lambda (lib) (shell-command-to-string (concat "realpath " lib))))
     (mapcar (lambda (fp) (replace-regexp-in-string "\n" "" fp)))
     (mapcar #'kiss-owns)
     (seq-uniq)
     (seq-remove (lambda (p) (string= p package))))
   (kiss--get-pkg-dependencies package)))

(defun kiss--strip-file (file)
  "(I) Run strip(1) on FILE with the proper arguments."
  (let* ((od-cmd-output
          (string-split
           (shell-command-to-string
            (concat "od -A o -t c -N 18" " " file))
           "\n"))
         (elf-rx (rx (0+ any) "177"
                     (0+ " ") "E"
                     (0+ " ") "L"
                     (0+ " ") "F"))
         (arch-rx (rx (0+ any) "!"
                      (0+ any) "<"
                      (0+ any) "a"
                      (0+ any) "r"
                      (0+ any) "c"
                      (0+ any) "h"
                      (0+ any) ">")))
    ;; .o & .a files.
    (when (or
           (and
            (string-match-p elf-rx (nth 0 od-cmd-output))
            (string-match-p (rx "0000020 001") (nth 1 od-cmd-output)))
           (string-match-p arch-rx (nth 0 od-cmd-output)))
      (message (concat "strip -g -R .comment -R .note " file))
      (shell-command (concat "strip -g -R .comment -R .note " file)))

    ;; .so & executables
    (when (and
           (string-match-p elf-rx (nth 0 od-cmd-output))
           (string-match-p (rx "0000020 00" (or "2" "3")) (nth 1 od-cmd-output)))
      (message (concat "strip -s -R .comment -R .note " file))
      (shell-command (concat "strip -s -R .comment -R .note " file)))))

(defun kiss--build-strip-files (dir file-path-lst)
  (mapcar
   (lambda (fp) (kiss--strip-file (concat dir fp)))
   file-path-lst))

;; FIXME: need to implement some kind of overwrite protection here,
;; since, making new chroots for each package in a list of
;; packages will be *very* expensive to do each time.
;; it makes much more sense to simply add the files that are missing
;; in the chroot each iteration, instead of rm-ing the dir and
;; remaking from scratch
(defun kiss--make-chroot-dir-for-pkg (dir package &optional strategy)
  ;; TODO: see if we can reuse some of the logic that I use for the
  ;; installation of files here as well. Just have to source the files
  ;; from current system instead of the tarballs.

  ;; TODO: assert that we have all of the dependencies for package
  ;; installed before we try copying over files.

  ;; Also, theoretically, this should be doable w/ hardlinks, provided
  ;; that the target dir is on the same file system as the source files.
  ;; Linking also has the added bonus of being *much* faster than copying.

  ;; FIXME: use new EIEIO code to figure out exactly which dependencies
  ;; need to be present - never include a dependencies make dependencies...
  ;; since it can just lead to problems

  (let ((missing-pkgs '())
        (package-needs-to-provide-lst '())
        (all-pkgs '())
        (alts (kiss-alternatives))
        (needed-pkgs
         ;; The reason we have to call out to get-pkg-dependency-order twice
         ;; is because for the first call, we are only interested in the
         ;; package graph of the dependencies for package, as they are
         ;; upstream. The second get-pkg-dependency-order will add any packages
         ;; that our locally installed version of the packages also requires
         ;; (dependencies that could have been dynamically picked up).

         ;; FIXME: when building a "system" package (such as curl), the installed
         ;; dependencies are also copied over aswell, breaking the attempt at
         ;; isolation. I'm sure that something will have to be modified here
         ;; with this get-pkg-dependency-order to take into account this edge
         ;; case.
         ;; The motivation behind this fixme is to help facilitate people
         ;; removing random (picked up) dependencies that a package might
         ;; have on their system.

         ;; One (potentially clever) way to do this would be to
         ;; check which packages are installed on the system version of
         ;; the package, and which ones are expected in the upsteam version
         ;; and remove the appropriate libraries...
         ;; (let ((package "curl"))
         ;;   (seq-difference
         ;;    (kiss--get-pkg-dependency-graph package)
         ;;    (kiss--get-pkg-dependency-graph package t)))

         (kiss--get-pkg-dependency-order
          (seq-uniq
           (append
            (cdr (reverse (kiss--get-pkg-dependency-order package)))
            ;; FIXME: need to discuss *how* this should be
            ;; done w/ the rest of the community.
            ;; I'm personally in favor of having a "kiss-system" package.
            '("baselayout" "certs" "musl"
              "linux-headers" "zlib" "b3sum"
              "bzip2" "pigz" "xz"
              "m4" "flex" "bison"
              "binutils" "gmp" "mpfr"
              "libmpc" "gcc" "busybox"
              "openssl" "curl" "git"
              "kiss" "make")))
          t)))

    ;; Two strategies for making the chroot - first one is
    ;; "permit-user-alternatives", the other is "prohibit-user-alternatives".
    ;; The behavior between the two strategies is as follows.

    ;; When using the "permit-user-alternatives" strategy,
    ;; the package manager will simply see that there are files
    ;; on the system that are not currently provided by needed-pkgs.
    ;; As a result, the package manager looks up the pacakges that own
    ;; each of the files it is missing, and adds said packages to the
    ;; list to eventually be installed.

    ;; When using the "prohibit-user-alternatives" strategy,
    ;; the package manage will instead overwrite the user's
    ;; chosen alternatives in the temporary chroot. This
    ;; involves gathering the list of files that are currently
    ;; in the choices database that are provided by needed-pkgs,
    ;; and then ensuring that we copy them over appropriately.

    (let ((strat (if strategy strategy kiss-make-chroot-strategy)))
      (pcase strat
        ('permit-user-alternatives
         ;; We need to look up the package owners who own the alternatives.
         ;; this is to ensure that all expected utilities are present in the
         ;; system.

         ;; This get's the list of "missing" packages that we will also need
         ;; in the chroot - the reason they are missing is because these
         ;; packages provide files that would otherwise be provided by
         ;; our current list of 'needed-pkgs'
         (setq missing-pkgs
               (thread-last
                 needed-pkgs
                 (mapcar
                  (lambda (pkg)
                    (thread-last
                      alts
                      (seq-filter (lambda (triple) (string= pkg (car triple))))
                      (mapcar (lambda (tr) (nth 1 tr)))
                      (seq-remove (lambda (s) (string-match-p "/$" s))))))
                 (flatten-list)
                 ;; Ensure that if there are multiple providers (for example
                 ;; /usr/bin/ls), that it only shows up once.
                 (seq-uniq)
                 (mapcar #'kiss-owns)
                 (seq-uniq)))

         (setq all-pkgs (append missing-pkgs needed-pkgs)))

        ('prohibit-user-alternatives
         (thread-last
           needed-pkgs
           ;; This could just be enirely placebo, but
           ;; this is intended to have packages who are closer to
           ;; the "core" of the system be prioritized when
           ;; having their alternatives swapped to.
           (kiss--get-pkg-order)

           ;; Get all of the manifest files and look files that are in choices dir.
           (mapcar #'kiss-manifest)
           (flatten-list)
           (seq-uniq)
           (seq-filter
            (lambda (s) (string-match-p (rx (literal kiss-choices-db-dir) (1+ any)) s)))
           (mapcar (lambda (s) (split-string s ">")))
           (mapcar (lambda (l) (list (kiss--basename (car l))
                                (concat "/" (string-join (cdr l) "/")))))
           ;; Convert the pairs to dotted pairs.
           (mapcar (lambda (p) (cons (car p) (cadr p))))

           ;; We now may have multiple providers of each package in the cdr
           ;; of each list.
           ;; While it is not necessarily perfect, I think it
           (mapc
            (lambda (pair)
              (unless (rassoc (cdr pair) package-needs-to-provide-lst)
                (setq package-needs-to-provide-lst (cons pair package-needs-to-provide-lst))))))

         (setq all-pkgs needed-pkgs)))

      ;; Remove any packages that have already been installed into dir
      ;; from all-packages
      (let ((dir-install-db (kiss--normalize-file-path
                             (concat dir kiss-installed-db-dir))))
        (when (kiss--file-is-directory-p dir-install-db)
          (setq all-pkgs (seq-difference
                          all-pkgs
                          (nthcdr 2 (directory-files dir-install-db))))))
      (let ((needed-files
             (thread-last
               all-pkgs
               (mapcar #'kiss-manifest)
               (flatten-list)
               (seq-uniq)
               (seq-sort #'string-lessp))))

        ;; We have to run the below code *twice* since it is possible for
        ;; the installation of symlinks to potentially fail.
        ;; This isn't ideal, but it works.
        (dotimes (_ 2)
          (dolist (file needed-files)
            (let ((normalized-file (kiss--normalize-file-path
                                    (concat dir file))))
              ;; TODO: make this also take in the validate argument?
              (unless (or (kiss--file-exists-p normalized-file)
                          (kiss--file-is-directory-p normalized-file))
                (pcase file
                  ((rx "/" eol)
                   (shell-command (concat "mkdir -p '" normalized-file "'")))
                  (_
                   (shell-command (concat "cp -fP '" file "' '" normalized-file "'"))))))))

        ;; FIXME: will need to eventually update the manifests that are installed
        ;; in the system, just in case if packages decide to (wrongly) use
        ;; 'kiss owns' in the build script to look up who owns a particular file.
        (pcase strat
          ('prohibit-user-alternatives
           (mapc
            (lambda (pair)
              (let ((pkg (car pair))
                    (file (cdr pair)))
                ;; We just have to move over the file from the
                ;; choices dir in the fake chroot to the right place.
                (shell-command
                 (concat
                  "mv -f "
                  (kiss--single-quote-string
                   (concat
                    (kiss--normalize-file-path
                     (concat dir kiss-choices-db-dir pkg))
                    (concat ">" (string-join (string-split file "/" t) ">"))))
                  " "
                  (kiss--single-quote-string
                   (kiss--normalize-file-path
                    (concat dir file)))))))
            package-needs-to-provide-lst)))))))


;; FIXME: pretty sure we bug out whenever we try to build a package
;; with zero sources. we need to support that functionality
;; FIXME: should try to see what functionality I can move out of this function
;; FIXME: have this function take a kiss-package obj
(defun kiss--build-pkg (pkg)
  "(I) Build PKG, return t if PKG was built successfully, nil otherwise."
  (let ((missing-deps (kiss--get-pkg-missing-dependencies pkg))
        (pkg-ver (replace-regexp-in-string
                  " " "-"
                  (string-trim-right (kiss--get-pkg-version pkg)))))

    ;; Only check for missing dependencies when kiss-force is nil.
    (unless kiss-force
      ;; Install/build missing dependencies
      (when missing-deps
        (mapc #'kiss--try-install-build missing-deps))

      ;; Recheck to make sure that we aren't missing any deps.
      (setq missing-deps (kiss--get-pkg-missing-dependencies pkg))

      (when missing-deps
        (error (concat "missing dependencies: "
                       (kiss--lst-to-str missing-deps)))))

    (let* ((build-cmd       "")
           (build-exit-code 1)
           (build-script    (concat (car (kiss-search pkg)) "/build"))
           (proc-dir        (kiss--get-tmp-destdir))
           (build-dir       (concat proc-dir "/build/" pkg "/"))
           (install-dir     (concat proc-dir "/pkg/" pkg))
           (k-el-build      (concat proc-dir "/build-" pkg "-kiss-el"))
           (log-dir         (concat kiss-logdir (format-time-string "%Y-%m-%d" (current-time)) "/"))
           (log-file        (concat log-dir pkg "-" (format-time-string "%Y-%m-%d-%H:%M" (current-time)))))


      (kiss--run-hook "pre-extract" pkg install-dir)

      ;; Extract pkg's sources to the build directory.
      (message (concat "Extracting " pkg "..."))
      (kiss--extract-pkg-sources pkg build-dir)
      (make-directory install-dir t)
      (make-directory log-dir t)
      (kiss--build-make-script k-el-build
                               build-script
                               build-dir
                               install-dir
                               pkg-ver
                               log-file)

      ;; NOTE: will need to be somewhat more clever when
      ;; executing the build script, since I would like to be able
      ;; to see the build as it is occurring, like in
      ;; async-shell-command

      ;; Now actually execute the script.
      (message (concat "Building " pkg " at version: " pkg-ver))

      (kiss--run-hook "pre-build" pkg build-dir)
      ;; (kiss--run-hook "queue" pkg left-in-queue tot-in-queue)

      ;; Additionally, we can use jails on freebsd to
      ;; achieve similar isolation.
      ;; https://docs.freebsd.org/en/books/handbook/jails/

      ;; TODO: add check for linux...
      (if kiss-perfom-build-in-sandbox
          ;; TODO: make these variables user configurable
          (let ((fake-chroot-dir "/tmp/kiss-fake-chroot/")
                (fake-home-dir "/tmp/kiss-fake-home/"))

            ;; TODO: make this user-configurable? making chroots is
            ;; expensive...
            ;; (when (kiss--file-is-directory-p fake-chroot-dir)
            ;;   (shell-command (concat "/usr/bin/rm -rvf " fake-chroot-dir)))

            (kiss--make-chroot-dir-for-pkg fake-chroot-dir pkg)
            (make-directory fake-home-dir t)
            (setq
             build-cmd
             (pcase kiss-sandbox-utility
               ("proot"
                (concat
                 "proot "
                 " -r " fake-chroot-dir " "
                 " -b " fake-home-dir ":" "/home" " "
                 " -b " (kiss--dirname k-el-build) ":" (kiss--dirname k-el-build) " "
                 " -b " (kiss--dirname build-script) ":" (kiss--dirname build-script) " "
                 " -b " build-dir ":" build-dir " "
                 " -b " install-dir ":" install-dir " "
                 " -b " log-dir ":" log-dir" "
                 " -w " build-dir " "
                 k-el-build))
               ("bwrap"
                (concat
                 "bwrap "
                 " --unshare-net "
                 ;; TODO: enforce / being read-only
                 " --bind " fake-chroot-dir " / "
                 " --bind " fake-home-dir " /home "
                 " --bind " (kiss--dirname k-el-build) " " (kiss--dirname k-el-build) " "
                 " --bind " (kiss--dirname build-script) " " (kiss--dirname build-script) " "
                 " --bind " build-dir " " build-dir " "
                 " --bind " install-dir " " install-dir " "
                 " --bind " log-dir " " log-dir " "
                 k-el-build))
               (_ (error (concat kiss-sandbox-utility " is not supported!"))))))
        (setq build-cmd k-el-build))

      (message "-----")
      (message build-cmd)
      (message "-----")

      ;; FIXME: will need to break out the code below
      ;; so it can handle being called from an async process's
      ;; finish func.
      ;; https://github.com/jwiegley/emacs-async

      (setq build-exit-code (shell-command build-cmd))
      (message "%s" build-exit-code)

      (when (> build-exit-code 0)
        (kiss--run-hook "build-fail" pkg build-dir)
        ;; FIXME: cleanup
        (error "build failed"))

      ;; Now we have to fork over the package files that we
      ;; used to the repo dir.
      (let ((pkg-install-db
             (concat install-dir kiss-installed-db-dir)))
        (make-directory pkg-install-db t)
        (kiss-fork pkg pkg-install-db)
        (message (concat "Successful Build!"))

        (kiss--run-hook "post-build" pkg install-dir)

        ;; FIXME: look over kiss code & implement /dev/null
        ;; for symlinks
        ;; Need to compute etcsums if they exist.
        (let* ((manifest-lst
                (kiss--get-manifest-for-dir install-dir))
               (etc-files
                (seq-filter
                 (lambda (s)
                   (and (string-match-p (rx bol "/etc") s)
                        (kiss--file-exists-p (concat install-dir "/" s))))
                 manifest-lst)))

          ;; If we have any etcfiles, create etcsums
          (if etc-files
              (kiss--write-text
               (mapconcat
                #'identity
                (mapcar #'kiss--b3 etc-files)
                "\n")
               'utf-8 (concat pkg-install-db pkg "/etcsums")))

          ;; Next, create the manifest
          (kiss--write-text
           ;; FIXME: I don't think this should be needed,
           ;; since, *technically* kiss--get-manifest-for-dir
           ;; should have already taken care of this...
           (kiss--manifest-to-string
            (kiss--get-manifest-for-dir install-dir))
           'utf-8 (concat pkg-install-db pkg "/manifest")))

        (let ((potential-binary-files
               (kiss--get-potential-binary-files
                (kiss--read-file
                 (concat pkg-install-db pkg "/manifest")))))
          (when (and (eq 1 kiss-strip)
                     (not (kiss--file-exists-p (concat build-dir "nostrip"))))
            (kiss--build-strip-files
             install-dir potential-binary-files))

          ;; TODO: finish up this impl.
          ;; FIXME: also need to do dependency fixing
          (kiss--build-get-missing-dependencies
           install-dir potential-binary-files pkg)))

      ;; Finally create the tarball
      (message (concat "Creating tarball for " pkg))
      (kiss--make-tarball-of-dir
       install-dir
       (concat kiss-bindir
               (kiss--get-pkg-bin-name pkg pkg-ver)))
      (message (concat "Tarball saved as: " kiss-bindir
                       (kiss--get-pkg-bin-name pkg pkg-ver)))

      (kiss--run-hook "post-package" pkg
                      (concat kiss-bindir
                              (kiss--get-pkg-bin-name pkg pkg-ver)))

      ;; rm the build directory
      (message (concat "Removing the build directory (" proc-dir ")"))
      ;; FIXME: need kiss--single-quote-string?
      (shell-command (concat "rm -rf -- " proc-dir))
      ;; Have the expr eval to t.
      t)))

;; (kiss--build-pkg "xdo")

;; FIXME: add in checks to the appropriate places.
(defun kiss--build-install (pkg)
  "(I) Attempt to build and install PKG, nil if unsuccessful."
  (when (kiss-build pkg)
    (kiss-install pkg)))

(defun kiss--try-install-build (pkg)
  "(I) Attempt to install a binary of PKG, else build and install PKG."
  (if (kiss--get-pkg-cached-bin pkg)
      (kiss-install pkg)
    (kiss--build-install pkg)))

;;;###autoload
(defun kiss-build (pkgs-l)
  (interactive)
  (cond ((listp pkgs-l)
         (progn
           ;; Download the package sources now.
           (when (kiss-download pkgs-l)
             (mapcar #'kiss--build-pkg
                     (kiss--get-pkg-order pkgs-l)))))
        ((atom pkgs-l)
         (progn
           (kiss-download pkgs-l)
           (kiss--build-pkg pkgs-l)))))

;; -> checksum     Generate checksums
;; ===========================================================================

;; Initial working impl of kiss-checksum below; need to refactor some of
;; the functionality since kiss-download has similar needs.

(defun kiss--get-pkg-local-checksums (pkg)
  "(I) Return the list of checksums for PKG from the files on disk, or nil."
  (seq-remove
   #'string-empty-p
   (mapcar
    #'kiss--source-get-local-checksum
    (slot-value (kiss--dir-to-kiss-package (car (kiss-search pkg))) :sources))))

(defun kiss--pkg-verify-local-checksums (pkg)
  "(I) Return t if local checksums equal the repo checksums for PKG, nil otherwise."
  (thread-last
    pkg
    (kiss-search)
    (car)
    (kiss--dir-to-kiss-package)
    (funcall (lambda (o) (slot-value o :sources)))
    (mapcar #'kiss--source-validate-p)
    (seq-remove #'identity)
    (length)
    (> 1)))

;;;###autoload
(defun kiss-checksum (pkgs-l)
  (cond
   ((listp pkgs-l)
    (mapcar #'kiss-checksum pkgs-l))
   ((atom pkgs-l)
    (let* ((pkg-path (car (kiss-search pkgs-l)))
           (chk-path (concat pkg-path "/checksums"))
           (chk-sums (mapconcat
                      #'identity
                      (kiss--get-pkg-local-checksums pkgs-l) "\n")))
      (when (and (kiss--am-owner-p chk-path)
                 (not (string-empty-p chk-sums)))
        (kiss--write-text chk-sums 'utf-8 chk-path))))))

;; (kiss--pkg-verify-local-checksums "chromium")

;; -> download     Download sources
;; ===========================================================================

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
           (kiss-search)
           (car)
           (kiss--dir-to-kiss-package)
           (funcall (lambda (o) (slot-value o :sources)))
           (mapcar #'kiss--source-download)
           (funcall (lambda (l) (seq-reduce (lambda (x y) (and x y)) l t)))))
        (t nil)))

;; (kiss-download '("kiss" "gdb"))
;; (kiss-download '("hugs"))

(defun kiss--make-temp-file ()
  "(I) Make a temporary file using the `mktemp' utility."
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "mktemp")))

(defun kiss--get-download-utility-arguments ()
  "(I) Get the proper arguments for the `kiss-get' utility."
  (cdr (assoc kiss-get kiss-get-alist)))

;; (kiss--get-pkg-sources-cache-path "kiss")
(defun kiss--get-pkg-sources-cache-path (pkg)
  "(I) Return the cache path in `kiss-srcdir' for each of PKG's sources."
  (mapcar
   #'kiss--source-get-cache-path
   (slot-value (kiss--dir-to-kiss-package (car (kiss-search pkg))) :sources)))

(defun kiss--pkg-sources-available-p (pkg)
  "(I) Return t if all of the sources for PKG are available locally, nil otherwise."
  (seq-reduce (lambda (x y) (and x y))
              (mapcar #'file-exists-p (kiss--get-pkg-sources-cache-path pkg)) t))

;; pkg_source_tar()
(defun kiss--extract-tarball (tarball dir)
  "(I) Extract TARBALL to DIR.  Emulates GNU Tar's --strip-components=1."
  (let ((decomp-tarball (kiss--make-temp-file)))
    ;; Decompress the tarball.
    (kiss--decompress tarball decomp-tarball)
    ;; Extract the tarball.
    (kiss--with-dir dir (shell-command (concat "tar xf " decomp-tarball)))
    ;; Get all of the top level directories from the tarball.
    (mapc
     (lambda (tld)
       (let* ((temp-f (kiss--make-temp-file))
              (temp-d (concat temp-f "-" tld)))
         (message "%s" (eq 0
                           (shell-command
                            (concat "mv -f " (concat dir "/" tld) " " temp-d))))

         ;; NOTE: we need to call directory-files twice here, since
         ;; First do the mv's
         (mapc
          (lambda (f)
            (shell-command
             (concat "mv -f " (concat temp-d f) " " dir)))
          (nthcdr 2 (directory-files temp-d)))

         ;; Then do the cp's
         (let ((files (nthcdr 2 (directory-files temp-d))))
           (if files
               (mapc
                (lambda (f)
                  (shell-command
                   (concat "cp -fRPp " (concat temp-d f) " " dir)))
                files)))

         ;; Make sure to rm the temp file.
         (kiss--shell-command-as-user
          (concat "rm -- " temp-f) (kiss--get-owner-name temp-f))
         ;; Also rm the temp directory.
         (kiss--shell-command-as-user
          (concat "rm -rf -- " temp-d) (kiss--get-owner-name temp-d))))

     ;; Make sure we only go through each top level directory *once*.
     (seq-uniq
      (mapcar
       ;; Get rid of any possible junk after the /.
       (lambda (str) (replace-regexp-in-string (rx "/" (0+ any) eol) "/" str))
       (seq-filter
        (lambda (line)
          ;; Keep lines that have only **1** / in them.
          (string-match-p (rx bol (0+ (not "/")) "/" (0+ (not "/")) eol) line))
        ;; TODO: can I remove this seq-uniq?
        (string-split
         (shell-command-to-string (concat "tar tf " tarball))
         "\n" t)))))

    ;; Remove our decompressed tarball now that we are done with it.
    (kiss--shell-command-as-user
     (concat "rm -f -- " decomp-tarball)
     (kiss--get-owner-name decomp-tarball))))

;; pkg_extract() in kiss
(defun kiss--extract-pkg-sources (pkg dir)
  "(I) Extract the cached sources of PKG to DIR."

  (dolist (source
           (slot-value
            (kiss--dir-to-kiss-package (car (kiss-search pkg))) :sources))

    (let ((cache  (kiss--source-get-cache-path source))
          (outdir (concat dir "/" (slot-value source :extracted-path))))
      (unless (kiss--file-is-directory-p outdir)
        (make-directory outdir t))

      (pcase (slot-value source :type)
        ('git (shell-command (concat "cp -PRf " cache "/. "  outdir)))
        (_
         (if (kiss--str-tarball-p cache)
             (kiss--extract-tarball cache outdir)
           (shell-command (concat "cp -PRf " cache " " outdir))))))))


(defun kiss--str-tarball-p (str)
  "(I) Predicate to determine if STR matches the regex for a tarball."
  (string-match-p
   (rx
    (or (: "t" any "z")
        (: "tar." any any)
        (: "tar." any any any)
        (: "tar." any any any any)) eol) str))

(ert-deftest kiss--str-tarball-p ()
  (should
   (eq 0 (and
          (kiss--str-tarball-p "txz")
          (kiss--str-tarball-p "tar.xz")
          (kiss--str-tarball-p "tar.zst")
          (kiss--str-tarball-p "tar.lzma")))))

;; -> install      Install packages
;; ===========================================================================

;; (defun kiss--pkg-is-installable-p (pkg)
;;   "(I) Return t if PKG is installable, nil otherwise."
;;   ;; A package is installable when the following conditions are met:
;;   ;; * all of the dependencies for the packge are installed
;;   (and
;;    (kiss--pkg-is-installed-p pkg)
;;    (eq (kiss--get-pkg-hard-dependents pkg) nil)
;;    (eq (kiss--get-pkg-orphan-alternatives pkg) nil)))

(defun kiss--get-pkg-conflict-files (pkg dir)
  ;; It is assumed that DIR will be an extracted kiss pkg.
  (let ((manifest-file
         (concat dir "/var/db/kiss/installed/" pkg "/manifest")))

    (unless (kiss--file-exists-p manifest-file)
      (error "manifest file does not exit!"))

    ;; TODO: would like to investigate the penalty of using a pure
    ;; Emacs lisp based solution for this.
    (let ((res '())
          (temp-file (kiss--make-temp-file)))
      (kiss--write-text
       (thread-last
         manifest-file
         (kiss--read-file)
         (seq-remove (lambda (str) (string-match-p "/$" str)))
         (kiss--manifest-to-string))
       'utf-8 temp-file)

      (setq res
            (mapcar
             (lambda (str) (reverse (string-split str ":")))
             (string-split
              (shell-command-to-string
               (concat "cat " temp-file
                       " | grep -Fxf - " (kiss--lst-to-str
                                          (kiss--get-installed-manifest-files))
                       " | grep -v " (concat pkg "/manifest:")))
              "\n" t)))

      (kiss--remove-file temp-file)
      res)))

;; FIXME: this *technically* doesn't report numbers in octal?
(defun kiss--rwx-lst-to-octal (lst)
  ;; TODO: add assert here to ensure that there are no extra chars in the lst
  ;;(string-to-list "rwx-")
  (let ((vals '(4 2 1))
        (tot 0))
    (message "%s" (length lst))
    (dotimes (i (length lst))
      (message "%s" i)
      (setq tot (+ tot
                   (* (if (eq (nth i lst) 45) 0 1)
                      (nth i vals)))))
    tot))

;;https://quickref.me/chmod.html
(ert-deftest kiss--rwx-lst-to-octal ()
  (should
   (and
    (= 7 (kiss--rwx-lst-to-octal '(114 119 120)))
    (= 6 (kiss--rwx-lst-to-octal '(114 119 45)))
    (= 5 (kiss--rwx-lst-to-octal '(114 45 120)))
    (= 4 (kiss--rwx-lst-to-octal '(114 45 45)))
    (= 3 (kiss--rwx-lst-to-octal '(45 119 120)))
    (= 2 (kiss--rwx-lst-to-octal '(45 119 45)))
    (= 1 (kiss--rwx-lst-to-octal '(45 45 120)))
    (= 0 (kiss--rwx-lst-to-octal '(45 45 45))))))

(defun kiss--file-rwx (file-path)
  (thread-last
    file-path
    (file-attributes)
    (file-attribute-modes)
    (string-to-list)
    (cdr)
    (funcall (lambda (lst) (seq-partition lst 3)))
    (mapcar #'kiss--rwx-lst-to-octal)
    (funcall (lambda (lst) (mapconcat #'number-to-string lst "")))))

(defun kiss--dirname (file-path)
  (mapconcat
   #'identity
   (seq-reverse (seq-drop (seq-reverse (string-split file-path "/")) 1))
   "/"))

(defun kiss--basename (file-path)
  (car (seq-reverse (string-split file-path "/"))))

(defun kiss--pkg-conflicts (pkg extr-dir)
  "(I) Fix up DIR for PKG so as to allow for alternatives."
  (let ((conf-files (kiss--get-pkg-conflict-files pkg extr-dir)))
    (when conf-files
      (let ((files-to-be-alts (mapcar #'car conf-files)))

        ;; Make the choices dir in the extracted tarball.
        (make-directory (concat extr-dir kiss-choices-db-dir) t)

        ;; Move all of the files to the appropriate place.
        (dolist (path files-to-be-alts)
          (let* ((alt      (string-replace "/" ">" path))
                 (alt-path (concat kiss-choices-db-dir pkg alt)))

            ;; Move the file to the choices directory.
            (shell-command
             (concat
              "mv -f " (kiss--single-quote-string (concat extr-dir path))
              " " (kiss--single-quote-string (concat extr-dir alt-path))))))

        ;; Regenerate the manifest for the directory.
        (kiss--write-text
         (kiss--manifest-to-string (kiss--get-manifest-for-dir extr-dir))
         'utf-8 (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest"))))))

;; TODO: make this take a root directory - that way this same code
;; can be used over in `kiss--make-chroot-dir-for-pkg'.
(defun kiss--install-files (source-dir file-path-lst pkg verify-p)
  ;; Copy files and create directories (while preserving permissions)
  ;; The 'test $1' will run w/ '-z' for overwrite and '-e' for verify.
  (let ((rn (kiss--get-random-number)))
    (dolist (file file-path-lst)
      (let ((actual-file (kiss--normalize-file-path (concat kiss-root file)))
            (source-file (concat source-dir file)))
        (pcase (kiss--file-identify source-file)

          ('directory
           (unless (kiss--file-is-directory-p actual-file)
             (kiss--shell-command-as-user
              (concat
               "mkdir -m " (kiss--file-rwx source-file) " "
               (kiss--single-quote-string actual-file))
              (kiss--get-owner-name kiss-root))))

          ('symlink
           (kiss--shell-command-as-user
            (concat "cp -fP " (kiss--single-quote-string source-file)
                    " " (kiss--single-quote-string
                         (concat (kiss--dirname actual-file) "/.")))
            (kiss--get-owner-name kiss-root)))

          ('file
           (let ((tmp
                  (concat
                   (kiss--dirname actual-file)
                   "__kiss-el-tmp-" pkg
                   "-" (kiss--basename actual-file)
                   "-" rn)))

             (kiss--shell-command-as-user
              (concat "cp -fP " (kiss--single-quote-string source-file)
                      " " tmp)
              (kiss--get-owner-name kiss-root))

             (kiss--shell-command-as-user
              (concat "mv -f " (kiss--single-quote-string tmp)
                      " " (kiss--single-quote-string actual-file))
              (kiss--get-owner-name kiss-root))))))))
  ;; FIXME: have a better return than nil
  nil)

(defun kiss--get-pkg-from-manifest (file-path-lst)
  "(I) Determine the package name from a manifest."
  (when (member kiss-installed-db-dir file-path-lst)
    (thread-last
      file-path-lst
      (seq-filter
       (lambda (fp) (string-match-p
                (rx
                 (literal kiss-installed-db-dir) (1+ (not "/")) "/" eol) fp)))
      (car)
      (funcall (lambda (str) (string-split str "/" t)))
      (reverse)
      (car))))

(ert-deftest kiss--get-pkg-from-manifest ()
  (should
   (string= "kiss" (kiss--get-pkg-from-manifest
                    `(,kiss-installed-db-dir
                      ,(concat kiss-installed-db-dir "kiss/"))))))


(defun kiss--install-tarball (tarball)
  "(I) Install TARBALL if it is a valid kiss package."
  (unless (kiss--file-exists-p tarball)
    (error (concat "kiss/install: " tarball " doesn't exist!")))

  (let* ((proc-dir       (kiss--get-tmp-destdir))
         (extr-dir       (concat proc-dir "/extracted"))
         (decomp-tarball (kiss--make-temp-file)))

    (make-directory extr-dir t)

    (kiss--decompress tarball decomp-tarball)
    (kiss--with-dir
     extr-dir
     (shell-command
      (concat "tar xf " decomp-tarball)))
    (kiss--remove-file decomp-tarball)

    (let ((pkg (kiss--get-pkg-from-manifest
                (kiss--get-manifest-for-dir extr-dir))))

      (unless pkg
        (error "kiss/install: Unable to detemine the package!"))

      ;; assume that the existence of the manifest file is all that
      ;; makes a valid KISS pkg.
      (unless (file-exists-p
               (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest"))
        (error "kiss/install: Not a valid kiss package!"))

      ;; Now that the pkg is verified to be a kiss pkg, we need
      ;; to validate the manifest that was shipped with the pkg.
      (unless (kiss--dir-matches-manifest-p
               extr-dir
               (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest"))
        (error "kiss/install: Manifest is not valid!"))

      ;; Only check for missing dependencies when 'kiss-force' is nil.
      (unless kiss-force
        ;; Check to make sure we aren't missing any dependencies.
        (let ((extr-depends
               (concat extr-dir "/var/db/kiss/installed/" pkg "/depends")))
          (when (kiss--file-exists-p extr-depends)
            (when (seq-contains-p
                   (mapcar #'kiss--pkg-is-installed-p
                           (kiss--get-dependencies-from-file extr-depends))
                   nil)
              (error "kiss/install: Missing dependencies!")))))

      (kiss--run-hook "pre-install" pkg extr-dir)

      (when (kiss--get-pkg-conflict-files pkg extr-dir)
        (if (eq 0 kiss-choice)
            (error "kiss/install: kiss-choice is equal to 0, erroring out!")
          (kiss--pkg-conflicts pkg extr-dir)))

      ;; FIXME: This code is *technically* not needed, but hey, might as well.
      ;; This function is typically *super cheap* to run.
      ;; Now that the pkg is verified to be a kiss pkg, we need
      ;; to validate the manifest that was shipped with the pkg.
      (unless (kiss--dir-matches-manifest-p
               extr-dir
               (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest"))
        (error "kiss/install: Manifest is not valid!"))

      ;; If the pkg is already installed (and this is an upgrade)
      ;; make a backup of the manifest and etcsum files
      (if (kiss--pkg-is-installed-p pkg)
          (progn
            (shell-command
             (concat "cp " kiss-installed-db-dir pkg "/manifest"
                     " " proc-dir "/manifest-copy"))
            (if (file-exists-p (concat kiss-installed-db-dir pkg "/etcsums"))
                (shell-command
                 (concat "cp " kiss-installed-db-dir pkg "/etcsums"
                         " " proc-dir "/etcsums-copy")))))

      ;; generate a list of files which exist in the current (installed)
      ;; manifest that do not exist in the new (to be installed) manifest.

      ;; FIXME: need to ensure that there is no breakage when
      ;; installing a package that is not presently intsalled.
      (let* ((new-manifest (kiss--read-file (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest")))
             (old-manifest (kiss--read-file (concat kiss-installed-db-dir pkg "/manifest")))
             (files-not-present-in-new-manifest
              ;; NOTE: the order here is backwards from upstream.
              (seq-difference old-manifest new-manifest)))
        ;; Reverse the manifest file so that we start shallow, and go deeper
        ;; as we iterate through each item. This is needed so that directories
        ;; are created in the proper order

        (message (concat "kiss/install: Installing " pkg "..."))

        ;; Install the packages files.
        (kiss--install-files extr-dir (reverse new-manifest) pkg nil)

        ;; Remove any files that were in the old manifest that aren't
        ;; in the new one.
        (kiss--remove-files files-not-present-in-new-manifest)

        ;; Install the packages files for a second time to fix
        ;; any potential mess that could have been made from the
        ;; previous rm.
        (kiss--install-files extr-dir (reverse new-manifest) pkg t)

        (kiss--run-hook-pkg "post-install" pkg)

        (kiss--run-hook "post-install" pkg (concat kiss-installed-db-dir pkg))

        (message (concat "kiss/install: Installation of " pkg " Finished.")))
      ;; FIXME: finish this func
      nil)))

;; FIXME: this need to be faster - currently it is super super slow
;; compared to the shell implementation.
;;;###autoload
(defun kiss-install (pkgs-l)
  (interactive)
  (cond
   ((listp pkgs-l)
    (mapcar #'kiss-install (kiss--get-pkg-order pkgs-l)))
   ((atom pkgs-l)
    (let* ((tarball
            (cond ((and (file-exists-p pkgs-l)
                        (kiss--str-tarball-p pkgs-l))
                   pkgs-l)
                  (t (kiss--get-pkg-cached-bin pkgs-l)))))
      (when tarball
        (kiss--install-tarball tarball))))))

(defun kiss--pkg-is-installed-p (pkg)
  "(I) Return t if PKG is installed, nil otherwise."
  (kiss--file-exists-p (concat kiss-installed-db-dir pkg)))

(defun kiss--install-if-not-installed (pkgs-l)
  "Only install packages in PKGS-L if they are not already installed."
  (kiss-install (seq-remove #'kiss--pkg-is-installed-p pkgs-l)))

;; -> list         List installed packages
;; ===========================================================================

(defun kiss--get-installed-pkg-version (pkg)
  "(I) Return the version string for PKG, nil if PKG is not installed."
  (if (kiss--pkg-is-installed-p pkg)
      (let ((pdir (concat kiss-installed-db-dir pkg)))
        (replace-regexp-in-string
         "\n$" ""
         ;; TODO: see if there is a way to avoid
         ;; depending on f.el
         (kiss--read-text (concat pdir "/version"))))))

;; TODO: add docstring.
;;;###autoload
(defun kiss-list (&optional pkg-q)
  (cond
   ((eq nil pkg-q)
    (let ((pkgs (nthcdr 2 (directory-files kiss-installed-db-dir))))
      (mapcar (lambda (p)
                (list p (kiss--get-installed-pkg-version p)))
              pkgs)))
   ((listp pkg-q)
    (mapcar #'kiss-list pkg-q))
   ((atom pkg-q)
    (when (kiss--pkg-is-installed-p pkg-q)
      (list pkg-q (kiss--get-installed-pkg-version pkg-q))))))

(ert-deftest kiss-list ()
  (should
   (and
    (equal
     (mapcar #'car (kiss-list))
     (string-split
      (shell-command-to-string (concat "ls " kiss-installed-db-dir )) "\n" t))
    (equal
     (mapcar #'cdr (kiss-list '("kiss" "git")))
     (thread-last
       '("kiss" "git")
       (mapcar (lambda (pkg) (concat kiss-installed-db-dir pkg "/version")))
       (mapcar #'kiss--read-file)))
    (string=
     (cadr (kiss-list "kiss"))
     (car (kiss--read-file (concat kiss-installed-db-dir "kiss/version")))))))

;; -> remove       Remove packages
;; ===========================================================================

;; TODO: need to account for symlinks w/ (file-symlink-p

(defun kiss--remove-file (file-path)
  "(I) Remove FILE-PATH as the appropriate user using rm(1)."
  (if (kiss--file-exists-p file-path)
      (let ((owner (kiss--get-owner-name file-path))
            (rmcmd (concat "rm -- " (kiss--single-quote-string file-path))))
        (eq 0
            (if (kiss--am-owner-p file-path)
                (shell-command rmcmd)
              (kiss--shell-command-as-user rmcmd owner))))))

(defun kiss--remove-directory (dir-path)
  "(I) Remove DIR-PATH as the appropriate user using rmdir(1)."
  (if (and (kiss--file-is-directory-p dir-path)
           (not (kiss--file-is-symbolic-link-p dir-path)))
      (let ((owner (kiss--get-owner-name dir-path))
            (rmcmd (concat "rmdir -- " dir-path)))
        (eq 0
            (if (kiss--am-owner-p dir-path)
                (shell-command rmcmd)
              (kiss--shell-command-as-user rmcmd owner))))))

(defun kiss--remove-files (file-path-lst)
  "(I) Remove all files and empty directories in FILE-PATH-LST."

  ;; TODO: Check this function with packages that have more elaborate
  ;; symlink structures, and ensure that this function removes all of the
  ;; files in the manifest properly.
  ;; I think one way to remedy this would be to try to remove all of the
  ;; directories from the file-path-lst again, once all of the symlinks are
  ;; gone, so that way all of the dirs can be properly removed.

  ;; This will return all of the /etc files/dirs.
  ;; (cl-remove-if-not
  ;;  (lambda (file-path) (string-match-p "/etc/" file-path))
  ;;  file-path-lst)

  ;; NOTE: I'm not entirely sure if it removing the files in the proper
  ;; order since it should be removing all of the files in a dir first,
  ;; then the dir itself, but when testing on 'xdo' it does not remove
  ;; the actual directory in `kiss-installed-db-dir'.

  ;; Make this local variable since we need to rm the symlinks
  ;; separately.
  (let ((symlink-queue '()))
    (mapc
     (lambda (file-path)
       (pcase (kiss--file-identify file-path)
         ('directory (kiss--remove-directory file-path))
         ('symlink   (setq symlink-queue
                           (cons file-path symlink-queue)))
         ('file      (kiss--remove-file      file-path))))
     file-path-lst)
    ;; Now to cleanup broken symlinks.
    (mapcar #'kiss--remove-file symlink-queue)))

;; NOTE: this function is slowed by the need
;; to use my custom file detection commands.
;;;###autoload
(defun kiss-remove (pkgs-l)
  (interactive)

  (cond ((listp pkgs-l)
         (mapcar #'kiss-remove
                 (reverse
                  (kiss--get-pkg-order pkgs-l))))
        ((atom pkgs-l)
         (if (or kiss-force (kiss--pkg-is-removable-p pkgs-l))
             (progn

               ;; FIXME: impl pkg hooks...
               (kiss--run-hook-pkg "pre-remove" pkgs-l)

               (kiss--run-hook "pre-remove"
                               pkgs-l (concat kiss-installed-db-dir pkgs-l))

               (kiss--remove-files
                (kiss-manifest pkgs-l)))))
        (t nil)))

;; -> search       Search for packages
;; ===========================================================================
;;;###autoload
(defun kiss-search (q)
  (interactive "sQuery: ")
  (seq-filter 'file-exists-p
              (mapcar (lambda (repo) (concat repo "/" q))
                      `(,@kiss-path
                        ,kiss-installed-db-dir))))

;; -> update       Update the repositories
;; ===========================================================================

;; TODO: see if Emacs has something built-in to do most of this (vc.el).

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
  (message "kiss-update")
  (kiss--update-git-repos)
  (when (executable-find "hg")
    (kiss--update-hg-repos))
  (when (executable-find "fossil")
    (kiss--update-fossil-repos)))

;; -> upgrade      Update the system
;; ===========================================================================

(defun kiss--pkg-remote-eq-pkg-local-p (pkg)
  "(I) Return t if the version of PKG is the same locally and from the remotes."
  (string=
   (kiss--sanitize-ver-str
    (kiss--read-text (concat (car (kiss-search pkg)) "/version")))
   (kiss--sanitize-ver-str
    (kiss--get-installed-pkg-version pkg))))

;; TODO: consider making this *not* internal.
(defun kiss--get-out-of-date-pkgs ()
  "(I) Return a list of PKGS that are out of date."
  (seq-remove 'kiss--pkg-remote-eq-pkg-local-p
              (mapcar 'car (kiss-list))))

;;;###autoload
(defun kiss-upgrade ()
  (interactive)

  (let* ((oodpkgs (kiss--get-out-of-date-pkgs)))
    (when oodpkgs
      (when (member "kiss" oodpkgs)
        (kiss--build-install "kiss")
        (setq oodpkgs (remove "kiss" oodpkgs)))
      ;; Build & install each package individually
      ;; - ensures that new dependencies are installed before their dependents.
      (dolist (pkg (kiss--get-pkg-order oodpkgs))
        (kiss--try-install-build pkg)))))

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

(defun kiss--get-pkg-order (pkgs-lst)
  "(I) Get the proper build order for the packages in PKGS-LST."
  (seq-filter
   (lambda (pkg) (member pkg pkgs-lst))
   (kiss--get-pkg-dependency-order pkgs-lst)))

;; This returns the proper order to build all of the out of date packages.
;; (kiss--get-pkg-order (kiss--get-out-of-date-pkgs))
;; (benchmark-elapse
;;   (kiss--get-pkg-order '("blender" "rust" "llvm" "ghc" "zig" "lld")))

;; Little snippet to get a list of all of the installed git-version packages.
;; (benchmark-elapse
;;   (seq-filter
;;    (lambda (pair)
;;      (let ((ver (car (cdr pair))))
;;        (string-match "git" ver)))
;;    (kiss-list)))

;; -> version      Package manager version
;; SEE const.

;; Run "kiss [H|help-ext]" to see all actions

(provide 'kiss)

;;; kiss.el ends here.
