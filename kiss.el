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
(require 'kiss-manifest)
(require 'kiss-preferred)
(require 'kiss-hook)
(require 'kiss-source)
(require 'kiss-package)
(require 'kiss-install)
(require 'kiss-search)
(require 'kiss-download)
(require 'kiss-build)
(require 'kiss-update)



;;(slot-value (kiss--dir-to-kiss-package (car (kiss-search "kiss"))) :sources)

;; ===========================================================================

;; Internal function definitions, these are considered to not be stable.
;; It's best not to rely on them outside of this file.


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




;; Public code below.


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
                     (format "cp -Pf '%s' '%s%s%s'" path kiss-choices-db-dir path-own alt)
                     (kiss--file-get-owner-name path))

                    ;; Only preserve file timestamps when the file is not a link.
                    (unless (eq (kiss--file-identify path) 'symlink)
                      ;; Also preserve the timestamp of the file aswell.
                      (kiss--shell-command-as-user
                       (format "touch -r '%s' '%s%s%s'" path kiss-choices-db-dir path-own alt)
                       (kiss--file-get-owner-name path)))

                    ;; Update the manifest file to reflect the new version.
                    (kiss--package-manifest-replace
                     path-own path (concat kiss-choices-db-dir path-own alt))))
              ;; Move over our new desired alternative to the real file.
              (kiss--shell-command-as-user
               (format "mv -f '%s' '%s'" alt-path path)
               (kiss--file-get-owner-name path))
              (kiss--package-manifest-replace pkg alt-path path))))))

;; (f-symlink-p "/var/db/kiss/choices/gawk\\>usr\\>bin\\>awk")
;; (f-exists?  "/var/db/kiss/choices/busybox\\>usr\\>bin\\>sh")
;; (kiss--file-exists-p "/var/db/kiss/choices/gawk\\>usr\\>bin\\>awk")
;; (if (kiss-owns "/usr/bin/awk") 1)
;; (cl-remove-if-not
;;  (lambda (s) (string= "gawk" s))
;;  (cl-mapcar #'car (kiss-alternatives)))
;; (concat "mawk" (string-replace "/" ">" "/usr/bin/awk"))



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




;; (benchmark-elapse (kiss--package-get-dependency-graph (mapcar #'car (kiss-list))))
;; (benchmark-elapse (kiss--package-get-dependency-graph-rec (mapcar #'car (kiss-list))))


(defun kiss--remove-redundant-dependencies (dep-lst)
  "(I) Remove redundant dependencies from DEP-LST."
  (seq-remove
   (lambda (pkg)
     (member pkg
             (thread-last
               dep-lst
               (mapcar #'kiss--package-get-dependencies)
               (flatten-list)
               (seq-uniq))))
   dep-lst))


;; (kiss--package-get-make-dependents "ant")
;; (kiss--package-get-make-orphans)
;; (length (kiss--package-get-make-orphans))


;; (kiss--package-get-hard-dependents "mpfr")


;; (benchmark-elapse (kiss--package-get-make-dependents "meson"))


;; -> build        Build packages
;; ===========================================================================

;; TODO: rename ?

;;;###autoload
(defun kiss-fork (pkg dir)
  "Fork PKG to DIR."
  (eq 0 (shell-command
         (concat "cp -Lrf " (car (kiss-search pkg)) " " dir))))

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
               (kiss--package-get-order)
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
                      (kiss--package-get-local-checksums pkgs-l) "\n")))
      (when (and (kiss--file-am-owner-p chk-path)
                 (not (string-empty-p chk-sums)))
        (kiss--write-text chk-sums 'utf-8 chk-path))))))

;; (kiss--pkg-verify-local-checksums "chromium")

;; -> install      Install packages
;; ===========================================================================

;; (defun kiss--pkg-is-installable-p (pkg)
;;   "(I) Return t if PKG is installable, nil otherwise."
;;   ;; A package is installable when the following conditions are met:
;;   ;; * all of the dependencies for the packge are installed
;;   (and
;;    (kiss--pkg-is-installed-p pkg)
;;    (eq (kiss--package-get-hard-dependents pkg) nil)
;;    (eq (kiss--package-get-orphan-alternatives pkg) nil)))



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
              (format "mkdir -m %s '%s'" (kiss--file-rwx source-file) actual-file)
              (kiss--file-get-owner-name target-dir))))

          ('symlink
           (kiss--shell-command-as-user
            (format "cp -fP '%s' '%s'" source-file
                    (concat (kiss--dirname actual-file) "/."))
            (kiss--file-get-owner-name target-dir)))

          ('file
           (let ((tmp
                  (format "'%s__kiss-el-tmp-%s-%s-%s'"
                          (kiss--dirname actual-file)
                          pkg
                          (kiss--basename actual-file)
                          rn)))

             ;; TODO: check to see if we can add the '-p' flag here to avoid
             ;; calling touch. My hunch is no, since IIRC, -p preserves
             ;; user and group information.
             (kiss--shell-command-as-user
              (format "cp -fP '%s' '%s'" source-file tmp)
              (kiss--file-get-owner-name target-dir))

             ;; Ensure that the timestamps are going to be the same.
             (kiss--shell-command-as-user
              (format "touch -r '%s' '%s'" source-file tmp)
              (kiss--file-get-owner-name target-dir))

             ;; mv already preserves timestamps, hence why we do not need to
             ;; do the touch after this mv.
             (kiss--shell-command-as-user
              (format "mv -f '%s' '%s'" tmp actual-file)
              (kiss--file-get-owner-name target-dir))))))))
  ;; FIXME: have a better return than nil
  nil)


(defun kiss--install-if-not-installed (pkgs-l)
  "Only install packages in PKGS-L if they are not already installed."
  (kiss-install (seq-remove #'kiss--pkg-is-installed-p pkgs-l)))

;; -> list         List installed packages
;; ===========================================================================



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
                  (kiss--package-get-order pkgs-l))))
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


;; -> update       Update the repositories
;; ===========================================================================

;; TODO: see if Emacs has something built-in to do most of this (vc.el).

;; -> upgrade      Update the system
;; ===========================================================================


;; TODO: consider making this *not* internal.
(defun kiss--get-out-of-date-pkgs ()
  "(I) Return a list of PKGS that are out of date."
  (seq-remove 'kiss--package-remote-eq-pkg-local-p
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
      (dolist (pkg (kiss--package-get-order oodpkgs))
        (kiss--try-install-build pkg)))))




;; This returns the proper order to build all of the out of date packages.
;; (kiss--package-get-order (kiss--get-out-of-date-pkgs))
;; (benchmark-elapse
;;   (kiss--package-get-order '("blender" "rust" "llvm" "ghc" "zig" "lld")))

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
