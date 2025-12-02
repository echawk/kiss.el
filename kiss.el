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
(require 'kiss-hook)
(require 'kiss-source)
(require 'kiss-package)
(require 'kiss-build)
(require 'kiss-update)



;;(slot-value (kiss--dir-to-kiss-package (car (kiss-search "kiss"))) :sources)

;; ===========================================================================

;; Internal function definitions, these are considered to not be stable.
;; It's best not to rely on them outside of this file.




(defun kiss--lst-to-str (lst)
  "(I) Convert LST to a string."
  (declare (pure t) (side-effect-free t))
  (mapconcat (lambda (s) (format "%s" s)) lst " "))


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

(defun kiss--package-get-dependencies (pkg &optional installed-p)
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

;; FIXME: implement installed-p argument here.
(defun kiss--package-get-dependency-graph-rec (pkg-lst)
  "(I) A Recursive & TCO-ed implementation of `kiss--package-get-dependency-graph'.

This version of the function is meant primarily as a resource for people
looking to implement kiss in other functional programming languages.
It is also important to note that there is no meaninful decrease in speed
when using this function compared with the iterative version."
  (let* ((pkgs (cond
                ((atom pkg-lst) `(,pkg-lst))
                ((listp pkg-lst) pkg-lst))))

    (named-let kiss--package-get-dependency-graph-rec-impl
        ((queue pkgs) (seen '()) (res '()))
      (let* ((new-queue (seq-remove (lambda (e) (member e seen)) queue))
             (dep (car new-queue)))
        (if dep
            (let* ((new-seen (cons dep seen))
                   (dep-deps (kiss--package-get-dependencies dep))
                   (item `(,dep ,dep-deps))
                   (new-res (if (not (member item res)) (cons item res) res)))
              (kiss--package-get-dependency-graph-rec-impl
               (append dep-deps (cdr new-queue)) new-seen new-res))
          res)))))

;; (benchmark-elapse (kiss--package-get-dependency-graph (mapcar #'car (kiss-list))))
;; (benchmark-elapse (kiss--package-get-dependency-graph-rec (mapcar #'car (kiss-list))))

(defun kiss--package-get-dependency-order (pkg-lst &optional installed-p)
  "(I) Return the proper build order of the dependencies for each pkg in PKG-LST."
  (let ((res (tsort
              (if installed-p
                  (kiss--package-get-dependency-graph pkg-lst t)
                (kiss--package-get-dependency-graph pkg-lst)))))
    (if res res (error "Circular dependency detected!"))))

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
;; (kiss--package-get-make-dependents "ant")

(defun kiss--package-get-make-orphans ()
  "(I) Return a list of packages which only have make dependents."
  ;; NOTE: This function is pretty slow at the moment.
  (seq-filter
   (lambda (pkg)
     (and (eq (kiss--package-get-hard-dependents pkg) nil)
          (not
           (eq (kiss--package-get-make-dependents pkg) nil))))
   (mapcar #'car (kiss-list))))

;; (kiss--package-get-make-orphans)
;; (length (kiss--package-get-make-orphans))

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

;; (kiss--package-get-hard-dependents "mpfr")

(defun kiss--package-get-orphan-alternatives (pkg)
  "(I) Return a list of orphaned alternatives that would result from removing PKG."
  (let ((orphaned-alternatives
         (seq-filter
          (lambda (pair) (seq-contains-p pair pkg))
          (kiss-preferred))))
    ;; Return the files associated with PKG.
    (if orphaned-alternatives
        (mapcar #'cadr orphaned-alternatives))))

;; (benchmark-elapse (kiss--package-get-make-dependents "meson"))

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

(defun kiss--package-get-local-checksums (pkg)
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

(defun kiss--package-get-conflict-files (pkg dir)
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
  (let ((conf-files (kiss--package-get-conflict-files pkg extr-dir)))
    (when conf-files
      ;; Make the choices dir in the extracted tarball.
      (make-directory (concat extr-dir kiss-choices-db-dir) t)

      ;; Move all of the files to the appropriate place.
      (dolist (path conf-files)
        (let* ((alt      (string-replace "/" ">" path))
               (alt-path (concat kiss-choices-db-dir pkg alt)))

          ;; Move the file to the choices directory.
          (kiss--silent-shell-command
           (format
            "mv -f '%s' '%s'" (concat extr-dir path) (concat extr-dir alt-path)))))

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

(defun kiss--package-get-from-manifest (file-path-lst)
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

    (let ((pkg (kiss--package-get-from-manifest
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

      (when (kiss--package-get-conflict-files pkg extr-dir)
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
             (format "cp '%s%s/manifest' '%s/manifest-copy'"
                     kiss-installed-db-dir pkg proc-dir))
            (if (file-exists-p (concat kiss-installed-db-dir pkg "/etcsums"))
                (shell-command
                 (format "cp '%s%s/etcsums' '%s/etcsums-copy'"
                         kiss-installed-db-dir pkg proc-dir)))))

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
    (mapcar #'kiss-install (kiss--package-get-order pkgs-l)))
   ((atom pkgs-l)
    (let* ((tarball
            (cond ((and (file-exists-p pkgs-l)
                        (kiss--file-is-tarball-p pkgs-l))
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
