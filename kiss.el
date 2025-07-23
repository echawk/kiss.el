;;; kiss.el --- KISS package manager in ELisp -*- lexical-binding: t -*-
;; SPDX-License-Identifier: MIT

;; Author: Ethan Hawk <ethan.hawk@valpo.edu>
;; Maintainer: Ethan Hawk <ethan.hawk@valpo.edu>
;; URL: https://github.com/ehawkvu/kiss.el
;; Keywords: package-manager, tools
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.2") (tsort "1.0"))
;; Version: 0

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
  (require 'rx)
  (require 'pcase))
(progn
  (require 'cl-lib)
  (require 'eieio)
  (require 'seq)
  (require 'subr-x)
  (require 'tsort))

(require 'kiss-env)
(require 'kiss-os)
(require 'kiss-file)
(require 'kiss-source)
(require 'kiss-package)


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

;; EIEIO Classes

(defclass kiss-build-env ()
  ((proc-dir
    :initarg :proc-dir
    :initform ""
    :type string
    :custom string
    :documentation "The directory where all the build files are located.")
   (build-dir
    :initarg :build-dir
    :initform ""
    :type string
    :custom string
    :documentation "The directory where the sources are extracted to, and where the build takes place.")
   (build-script
    :initarg :build-script
    :initform ""
    :type string
    :custom string
    :documentation "The build script.")
   (install-dir
    :initarg :install-dir
    :initform ""
    :type string
    :custom string
    :documentation "The directory where the built package will be installed to.")
   (kiss-el-build
    :initarg :kiss-el-build
    :initform ""
    :type string
    :custom string
    :documentation "The path to the script resulting from 'kiss--build-make-script'.")
   (log-dir
    :initarg :log-dir
    :initform ""
    :type string
    :custom string
    :documentation "The directory where the log file will be stored.")
   (log-file
    :initarg :log-file
    :initform ""
    :type string
    :custom string
    :documentation "The path to the log file.")))

(defun kiss--build-env-for-package (pkg-obj)
  ;; TODO: when/if i implement the ability for build-files to be
  ;; text of the commands to run, this is where the logic will be at.
  (with-slots
      ((build-file :build-file)
       (name       :name)
       (version    :version))
      pkg-obj
    (let*
        ((proc-dir        (kiss--get-tmp-destdir))
         (build-dir       (concat proc-dir "/build/" name "/"))
         (install-dir     (concat proc-dir "/pkg/" name ))
         (k-el-build      (concat proc-dir "/build-" name "-kiss-el"))
         (log-dir
          (concat kiss-logdir
                  (format-time-string "%Y-%m-%d" (current-time)) "/"))
         (log-file
          (concat log-dir name "-"
                  (format-time-string "%Y-%m-%d-%H:%M" (current-time)))))

      (make-directory install-dir t)
      (make-directory log-dir t)
      (kiss--build-make-script k-el-build
                               build-file
                               build-dir
                               install-dir
                               version
                               log-file)

      (make-instance
       'kiss-build-env
       :proc-dir      proc-dir
       :build-dir     build-dir
       :build-script  build-file
       :install-dir   install-dir
       :kiss-el-build k-el-build
       :log-dir       log-dir
       :log-file      log-file))))

;;(slot-value (kiss--dir-to-kiss-package (car (kiss-search "kiss"))) :sources)

;; ===========================================================================

;; Internal function definitions, these are considered to not be stable.
;; It's best not to rely on them outside of this file.




(defun kiss--lst-to-str (lst)
  "(I) Convert LST to a string."
  (declare (pure t) (side-effect-free t))
  (mapconcat (lambda (s) (format "%s" s)) lst " "))

(defun kiss--sanitize-ver-str (str)
  "(I) Sanitize a version string STR to be correctly compared against others."
  (declare (pure t) (side-effect-free t))
  (thread-last
    str
    (replace-regexp-in-string " " "")
    (replace-regexp-in-string "\n$" "")))


;; (defun kiss--shell-command (command)
;;   ;; Wrapper over 'shell-command' that prevents a ton of messages being
;;   ;; printed.
;;   (let ((inhibit-message t)
;;         (message-log-max nil))
;;     (shell-command command)))


;; FIXME: potentially change this function
;; to simply remove the final newline at the end of the text
;; and to then split on the newlines - it should result in the same
;; code as the following for kiss-manifest, but would allow this
;; code to be used other places too


(defun kiss--get-decompression-command (file-path)
  (declare (pure t) (side-effect-free t))
  (let ((matched-rgx
         (thread-last
           kiss-decompress-alist
           (mapcar #'car)
           (seq-filter (lambda (rgx) (string-match-p rgx file-path)))
           (car))))
    (cdr (assoc matched-rgx kiss-decompress-alist))))


(defun kiss--decompress (file-path out-path)
  "(I) Decompress FILE-PATH to OUT-PATH based on the file name."
  (let ((cmd (kiss--get-decompression-command file-path)))
    (when cmd
      (kiss--silent-shell-command (concat cmd file-path " > " out-path)))))

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
      (kiss--silent-shell-command (format "%s %s %s %s %s" kh hook arg2 arg3 arg4)))))

(defun kiss--run-hook-pkg (hook pkg)
  "(I) Run PKG's HOOK."
  (let ((hook-fp (concat kiss-installed-db-dir pkg "/" hook)))
    (when (kiss--file-is-executable-p hook-fp)
      ;; FIXME: need to expose the proper environment to this shell
      (with-environment-variables
          (("KISS_ROOT" kiss-root))
        (kiss--shell-command-as-user
         hook-fp (kiss--file-get-owner-name kiss-root))))))

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



(defun kiss--single-quote-string (str)
  "(I) Add quotes around STR.  Useful when interacting with the cli."
  (declare (pure t) (side-effect-free t))
  (concat "'" str "'"))


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
                     (kiss--file-get-owner-name path))

                    ;; Only preserve file timestamps when the file is not a link.
                    (unless (eq (kiss--file-identify path) 'symlink)
                      ;; Also preserve the timestamp of the file aswell.
                      (kiss--shell-command-as-user
                       (concat "touch -r " path " "
                               (kiss--single-quote-string
                                (concat kiss-choices-db-dir path-own alt)))
                       (kiss--file-get-owner-name path)))

                    ;; Update the manifest file to reflect the new version.
                    (kiss--pkg-manifest-replace
                     path-own path (concat kiss-choices-db-dir path-own alt))))
              ;; Move over our new desired alternative to the real file.
              (kiss--shell-command-as-user
               (concat "mv -f " (kiss--single-quote-string alt-path)
                       " " path)
               (kiss--file-get-owner-name path))
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
  (concat (string-join pkg-manifest "\n") "\n"))

;; (kiss--manifest-to-string (kiss-manifest "xdo"))

;;;###autoload
(defun kiss-manifest (pkg)
  "Return a list of all files owned by PKG."
  (kiss--file-read-file
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
        (string-join
         (seq-uniq
          (mapcar
           (lambda (file-str)
             (concat
              "/"
              (string-join
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
    "\n" t)))

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
  (let ((res (tsort
              (if installed-p
                  (kiss--get-pkg-dependency-graph pkg-lst t)
                (kiss--get-pkg-dependency-graph pkg-lst)))))
    (if res res (error "Circular dependency detected!"))))

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
         (kiss--file-read-file manifest-file)))

(defun kiss--get-compression-command ()
  "(I) Return the proper command based on `kiss-compress'."
  (cdr (assoc kiss-compress kiss-compress-alist)))

(defun kiss--get-random-number (&optional upper-bound)
  "(I) Number from 1 to UPPER-BOUND, exclusive. Default UPPER-BOUND is 30000."
  (message "%s" (mod (abs (random)) (if upper-bound upper-bound 30000))))

;; TODO: rename ?
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
    (when kiss-build-env-hook
      (mapcar
       (lambda (hook)
         (concat
          "eval $(" hook " " (last (string-split build-dir "/" t)) ")\n"))
       kiss-build-env-hook))

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

;; FIXME: support other kiss-elf commands
(defun kiss--build-get-missing-dependencies (dir file-path-lst pkg-obj)
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
     (seq-remove (lambda (p) (string= p (slot-value pkg-obj :name)))))
   (slot-value pkg-obj :depends)))

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
      (kiss--silent-shell-command (concat "strip -g -R .comment -R .note " file)))

    ;; .so & executables
    (when (and
           (string-match-p elf-rx (nth 0 od-cmd-output))
           (string-match-p (rx "0000020 00" (or "2" "3")) (nth 1 od-cmd-output)))
      (message (concat "strip -s -R .comment -R .note " file))
      (kiss--silent-shell-command (concat "strip -s -R .comment -R .note " file)))))

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
(defun kiss--make-chroot-dir-for-pkg (chroot-dir package &optional strategy)
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
      (let ((dir-install-db (kiss--file-normalize-file-path
                             (concat chroot-dir kiss-installed-db-dir))))
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

        ;; FIXME: figure out why these two expression fail.
        ;; They will create the chroot directory properly, however when
        ;; the build actually goes to execute, the build fails, saying
        ;; that it is unable to find the kiss-el-build file.
        ;; (make-directory chroot-dir t)
        ;; (kiss--install-files
        ;;  kiss-root chroot-dir
        ;;  (seq-remove (lambda (s) (string= "/tmp/" s)) needed-files)
        ;;  package nil)

        ;; We have to run the below code *twice* since it is possible for
        ;; the installation of symlinks to potentially fail.
        ;; This isn't ideal, but it works.
        (dotimes (_ 2)
          (dolist (file needed-files)
            (let ((normalized-file (kiss--file-normalize-file-path
                                    (concat chroot-dir file))))
              ;; todo: make this also take in the validate argument?
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
                    (kiss--file-normalize-file-path
                     (concat chroot-dir kiss-choices-db-dir pkg))
                    (concat ">" (string-join (string-split file "/" t) ">"))))
                  " "
                  (kiss--single-quote-string
                   (kiss--file-normalize-file-path
                    (concat chroot-dir file)))))))
            package-needs-to-provide-lst)))))))

(cl-defmethod kiss--package-build ((pkg-obj kiss-package))
  (with-slots
      ((name         :name)
       (version      :version)
       (release      :release)
       (build-file   :build-file)
       (depends      :depends)
       (make-depends :make-depends)
       (sources      :sources))
      pkg-obj

    ;; Check for missing deps
    (unless kiss-force
      (let ((missing-deps (kiss--package-get-missing-dependencies pkg-obj)))
        (when missing-deps (mapc #'kiss--try-install-build missing-deps))
        (setq missing-deps (kiss--package-get-missing-dependencies pkg-obj))
        (when missing-deps (error (concat "Missing Dependencies: "
                                          (kiss--lst-to-str missing-deps))))))

    (let ((build-env-obj (kiss--build-env-for-package pkg-obj)))
      (with-slots
          ((proc-dir     :proc-dir)
           (build-dir    :build-dir)
           (build-script :build-script)
           (install-dir  :install-dir)
           (k-el-build   :kiss-el-build)
           (log-dir      :log-dir)
           (log-file     :log-file))
          build-env-obj

        (kiss--run-hook "pre-extract" name install-dir)
        (kiss--package-extract-sources pkg-obj build-dir)

        (kiss--run-hook "pre-build" name build-dir)
        (message (concat "Building " name " at version: " version))

        (when (> (shell-command
                  (kiss--build-determine-build-cmd build-env-obj pkg-obj))
                 0)
          (kiss--run-hook "build-fail" name build-dir)
          (error (concat "Build of " name " at " version " failed!")))
        (kiss--run-hook "post-build" name install-dir)

        ;; NOTE: there is definitely still some work & cleanup stuff that
        ;; needs to happen to the below code, but I think this is pretty
        ;; close to replacing the current build function
        (make-directory (concat install-dir kiss-installed-db-dir) t)
        ;; FIXME: make this fork happen at the end of this funciton ~ easier
        ;; to fix the dependencies and whatnot.
        (kiss--package-to-dir pkg-obj
                              (concat install-dir kiss-installed-db-dir))

        ;; FIXME: look over kiss code & implement /dev/null
        ;; for symlinks
        ;; Need to compute etcsums if they exist.
        (let* ((manifest-lst
                (kiss--get-manifest-for-dir install-dir))
               (etc-files
                (seq-filter
                 (lambda (s)
                   (and (string-match-p      (rx bol "/etc") s)
                        (kiss--file-exists-p (concat install-dir "/" s))))
                 manifest-lst))
               (potential-binary-files
                (kiss--get-potential-binary-files manifest-lst)))

          ;; If we have any etcfiles, create etcsums
          (when etc-files
            (kiss--write-text
             (mapconcat #'identity (mapcar #'kiss--b3 etc-files) "\n")
             'utf-8
             (concat install-dir kiss-installed-db-dir name "/etcsums")))

          ;; Next, create the manifest
          (kiss--write-text
           ;; FIXME: I don't think this should be needed,
           ;; since, *technically* kiss--get-manifest-for-dir
           ;; should have already taken care of this...
           (kiss--manifest-to-string (kiss--get-manifest-for-dir install-dir))
           'utf-8
           (concat install-dir kiss-installed-db-dir name "/manifest"))

          ;; Potentially strip the binaries.
          (when (and (eq 1 kiss-strip)
                     (not (kiss--file-exists-p (concat build-dir "nostrip"))))
            (kiss--build-strip-files
             install-dir potential-binary-files))

          ;; TODO: finish up this impl.
          ;; FIXME: also need to do dependency fixing
          (kiss--build-get-missing-dependencies
           install-dir potential-binary-files pkg-obj))

        ;; FIXME; need to refork the package here, since we could have
        ;; picked up some extra dependencies
        ;; Create the packge in the install-dir
        ;;(kiss--package-to-dir pkg-obj install-dir)

        ;; Finally create the tarball
        (unless  (kiss--make-tarball-of-dir
                  install-dir
                  (concat kiss-bindir (kiss--package-bin-name pkg-obj)))
          (error (format "Failed to create a tarball for %s" name)))

        (kiss--run-hook "post-package" name
                        (concat kiss-bindir (kiss--package-bin-name pkg-obj)))

        ;; rm the build directory
        (message (concat "Removing the build directory (" proc-dir ")"))
        (shell-command
         (concat "rm -rf -- " (kiss--single-quote-string proc-dir)))
        t))))

(defun kiss--build-determine-build-cmd (build-env-obj pkg-obj)
  (with-slots
      ((proc-dir    :proc-dir)
       (build-dir   :build-dir)
       (build-script :build-script)
       (install-dir :install-dir)
       (k-el-build  :kiss-el-build)
       (log-dir     :log-dir)
       (log-file    :log-file))
      build-env-obj
    (if kiss-perfom-build-in-sandbox
        ;; TODO: make these variables user configurable
        (let ((fake-chroot-dir "/tmp/kiss-fake-chroot/")
              (fake-home-dir "/tmp/kiss-fake-home/"))

          ;; TODO: make this user-configurable? making chroots is
          ;; expensive...
          ;; (when (kiss--file-is-directory-p fake-chroot-dir)
          ;;   (shell-command (concat "/usr/bin/rm -rvf " fake-chroot-dir)))

          (kiss--make-chroot-dir-for-pkg fake-chroot-dir (slot-value pkg-obj :name))
          (make-directory fake-home-dir t)
          (pcase kiss-sandbox-utility
            ("proot"
             (concat
              "proot "
              " -r " fake-chroot-dir " "
              " -b " fake-home-dir ":" "/home" " "
              " -b " (kiss--dirname k-el-build) ":" (kiss--dirname k-el-build) " "
              " -b " (kiss--dirname (slot-value pkg-obj :build-file)) ":" (kiss--dirname (slot-value pkg-obj :build-file)) " "
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
              k-el-build))))
      k-el-build)))

;; FIXME: add in checks to the appropriate places.
(defun kiss--build-install (pkg)
  "(I) Attempt to build and install PKG, nil if unsuccessful."
  (when (kiss-build pkg)
    (kiss-install pkg)))

(defun kiss--try-install-build (pkg)
  "(I) Attempt to install a binary of PKG, else build and install PKG."
  (if (thread-last
        pkg
        (kiss--search-pkg-obj)
        (kiss--package-bin-name)
        (concat kiss-bindir)
        (kiss--file-exists-p))
      (kiss-install pkg)
    (kiss--build-install pkg)))

;;;###autoload
(defun kiss-build (pkgs-l)
  (interactive)
  (cond ((listp pkgs-l)
         (progn
           ;; Download the package sources now.
           (when (kiss-download pkgs-l)
             (thread-last
               pkgs-l
               (kiss--get-pkg-order)
               (mapcar #'kiss--search-pkg-obj)
               (mapcar #'kiss--package-build)))))
        ((atom pkgs-l)
         (progn
           (when (kiss-download pkgs-l)
             (kiss--package-build
              (kiss--search-pkg-obj pkgs-l)))))))

;; -> checksum     Generate checksums
;; ===========================================================================

;; Initial working impl of kiss-checksum below; need to refactor some of
;; the functionality since kiss-download has similar needs.

(defun kiss--get-pkg-local-checksums (pkg)
  "(I) Return the list of checksums for PKG from the files on disk, or nil."
  (thread-last
    (slot-value (kiss--search-pkg-obj pkg) :sources)
    (mapcar #'kiss--source-get-local-checksum)
    (seq-remove #'string-empty-p)))

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
      (when (and (kiss--file-am-owner-p chk-path)
                 (not (string-empty-p chk-sums)))
        (kiss--write-text chk-sums 'utf-8 chk-path))))))

;; (kiss--pkg-verify-local-checksums "chromium")

;; -> download     Download sources
;; ===========================================================================

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

;; FIXME: remove this & use (kiss--file-is-tarball-p file-path)
(defun kiss--str-tarball-p (str)
  "(I) Predicate to determine if STR matches the regex for a tarball."
  (string-match-p
   (rx
    (or (: "t" any "z")
        (: "tar." any any)
        (: "tar." any any any)
        (: "tar." any any any any)) eol) str))


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

    (cl-assert (kiss--file-exists-p manifest-file))

    ;; TODO: would like to investigate the penalty of using a pure
    ;; Emacs lisp based solution for this.
    (let ((current-installed-files
           (thread-last
             (kiss--get-installed-manifest-files)
             (seq-remove (lambda (fp) (string-match-p pkg fp)))
             (mapcar #'kiss--file-read-file)
             (flatten-list)
             (seq-remove (lambda (fp) (string-match-p (rx "/" eol) fp))))))
      (seq-filter
       (lambda (fp) (member fp current-installed-files))
       (kiss--file-read-file manifest-file)))))


(defun kiss--pkg-conflicts (pkg extr-dir)
  "(I) Fix up DIR for PKG so as to allow for alternatives."
  (let ((conf-files (kiss--get-pkg-conflict-files pkg extr-dir)))
    (when conf-files
      ;; Make the choices dir in the extracted tarball.
      (make-directory (concat extr-dir kiss-choices-db-dir) t)

      ;; Move all of the files to the appropriate place.
      (dolist (path conf-files)
        (let* ((alt      (string-replace "/" ">" path))
               (alt-path (concat kiss-choices-db-dir pkg alt)))

          ;; Move the file to the choices directory.
          (kiss--silent-shell-command
           (concat
            "mv -f " (kiss--single-quote-string (concat extr-dir path))
            " " (kiss--single-quote-string (concat extr-dir alt-path))))))

      ;; Regenerate the manifest for the directory.
      (kiss--write-text
       (kiss--manifest-to-string (kiss--get-manifest-for-dir extr-dir))
       'utf-8 (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest")))))

(defun kiss--install-files (source-dir target-dir file-path-lst pkg verify-p)
  ;; Copy files and create directories (while preserving permissions)
  ;; The 'test $1' will run w/ '-z' for overwrite and '-e' for verify.
  (let ((rn (kiss--get-random-number)))
    (dolist (file file-path-lst)
      (let ((actual-file (kiss--file-normalize-file-path (concat target-dir file)))
            (source-file (concat source-dir file)))
        (pcase (kiss--file-identify source-file)

          ('directory
           (unless (kiss--file-is-directory-p actual-file)
             (kiss--shell-command-as-user
              (concat
               "mkdir -m " (kiss--file-rwx source-file) " "
               (kiss--single-quote-string actual-file))
              (kiss--file-get-owner-name target-dir))))

          ('symlink
           (kiss--shell-command-as-user
            (concat "cp -fP " (kiss--single-quote-string source-file)
                    " " (kiss--single-quote-string
                         (concat (kiss--dirname actual-file) "/.")))
            (kiss--file-get-owner-name target-dir)))

          ('file
           (let ((tmp
                  (kiss--single-quote-string
                   (concat
                    (kiss--dirname actual-file)
                    "__kiss-el-tmp-" pkg
                    "-" (kiss--basename actual-file)
                    "-" rn))))

             ;; TODO: check to see if we can add the '-p' flag here to avoid
             ;; calling touch. My hunch is no, since IIRC, -p preserves
             ;; user and group information.
             (kiss--shell-command-as-user
              (concat "cp -fP " (kiss--single-quote-string source-file)
                      " " tmp)
              (kiss--file-get-owner-name target-dir))

             ;; Ensure that the timestamps are going to be the same.
             (kiss--shell-command-as-user
              (concat "touch -r " (kiss--single-quote-string source-file)
                      " " tmp)
              (kiss--file-get-owner-name target-dir))

             ;; mv already preserves timestamps, hence why we do not need to
             ;; do the touch after this mv.
             (kiss--shell-command-as-user
              (concat "mv -f " tmp
                      " " (kiss--single-quote-string actual-file))
              (kiss--file-get-owner-name target-dir))))))))
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


(defun kiss--install-tarball (tarball)
  "(I) Install TARBALL if it is a valid kiss package."
  (unless (kiss--file-exists-p tarball)
    (error (concat "kiss/install: " tarball " doesn't exist!")))

  (let* ((proc-dir       (kiss--get-tmp-destdir))
         (extr-dir       (concat proc-dir "/extracted"))
         (decomp-tarball (kiss--file-make-temp-file)))

    (make-directory extr-dir t)

    (kiss--decompress tarball decomp-tarball)
    (kiss--with-dir
     extr-dir
     (shell-command
      (concat "tar xf " decomp-tarball)))
    (kiss--file-remove-file decomp-tarball)

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
          (progn
            (message
             (concat "Fixing package conflicts for " pkg " in " extr-dir))
            (kiss--pkg-conflicts pkg extr-dir)
            (message
             (concat "Done fixing package conflicts.")))))

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
      (let* ((new-manifest (kiss--file-read-file (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest")))
             (old-manifest (kiss--file-read-file (concat kiss-installed-db-dir pkg "/manifest")))
             (files-not-present-in-new-manifest
              ;; NOTE: the order here is backwards from upstream.
              (seq-difference old-manifest new-manifest)))
        ;; Reverse the manifest file so that we start shallow, and go deeper
        ;; as we iterate through each item. This is needed so that directories
        ;; are created in the proper order

        (message (concat "kiss/install: Installing " pkg "..."))

        ;; Install the packages files.
        (kiss--install-files extr-dir kiss-root (reverse new-manifest) pkg nil)

        ;; Remove any files that were in the old manifest that aren't
        ;; in the new one.
        (kiss--file-remove-files files-not-present-in-new-manifest)

        ;; Install the packages files for a second time to fix
        ;; any potential mess that could have been made from the
        ;; previous rm.
        (kiss--install-files extr-dir kiss-root (reverse new-manifest) pkg t)

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
                  (t
                   (concat
                    kiss-bindir
                    (kiss--package-bin-name
                     (kiss--search-pkg-obj pkgs-l)))))))
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


;; -> remove       Remove packages
;; ===========================================================================

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

               (kiss--file-remove-files
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

(defun kiss--search-pkg-obj (q)
  (let ((res (kiss-search q)))
    (when res
      (kiss--dir-to-kiss-package (car res)))))

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
  (seq-intersection (kiss--get-pkg-dependency-order pkgs-lst) pkgs-lst))

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
