;;; kiss.el --- KISS package manager in ELisp -*- lexical-binding: t -*-

;; Author: Ethan Hawk <ethan.hawk@valpo.edu>
;; Maintainer: Ethan Hawk <ethan.hawk@valpo.edu>
;; URL: https://github.com/ehawkvu/kiss.el
;; Keywords: package-manager, tools
;; Package-Requires: ((dash) (emacs "29.1") (f) (tsort))
;; Version: 0.0.1

;; This file is under the MIT license.

;;; Commentary:

;; TODO: need to replace all slashes with something else, likely
;; a hyphen?
;; or could rename kiss/internal to kiss-i or just kiss--

;; TODO: see if I can reduce the required Emacs version.

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

;; Hooks are currently on the back burner until a POC can be fleshed out.
;; However, I don't think that it will actually be all that difficult, since
;; Emacs already has built-in functionality for them...

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

;; Also, need to go through this code once I get a fully working POC done
;; and ruthlessly remove all duplicated code, since rn there are many
;; little redundancies spread about the current source.

;; https://www.emacswiki.org/emacs/UnitTesting

;; Fun little piece of trivia for the KISS Linux veterans out there:
;; https://github.com/dylanaraps/community/commit/e370a224520d07e6e42ba045845674b39dea03a4

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'f)
  (require 'rx)
  (require 'seq)
  (require 'subp)
  (require 'tsort))

;; FIXME: Find out what the containing group should be...
(defgroup kiss nil
  "The KISS package manager, in ELisp.")

;; TODO: cleanup these names, they're not consistent...
(defconst kiss/root "/")
(defconst kiss/installed-db-dir (concat kiss/root "var/db/kiss/installed/"))
(defconst kiss/choices-db-dir (concat kiss/root "var/db/kiss/choices/"))

(defconst kiss/KISS_TMPDIR
  (concat (getenv "HOME") "/.cache/kiss/proc/"))

(defconst kiss/KISS_SRCDIR
  (concat (getenv "HOME") "/.cache/kiss/sources/"))

(defconst kiss/KISS_BINDIR
  (concat (getenv "HOME") "/.cache/kiss/bin/"))

(defconst kiss/version "0.0.1"
  "The version of kiss.el.")
(defconst kiss/compat-version "5.6.4"
  "The version of kiss that kiss.el is compatible with.")

(defcustom kiss/KISS_GET
  (car (seq-filter 'executable-find
                   '("aria2c" "axel" "curl" "wget" "wget2")))
  "The utility for downloading http sources."
  :type 'string)

(defcustom kiss/KISS_CHK
  (car (seq-filter 'executable-find
                   '("openssl" "sha256sum" "sha256" "shasum" "digest")))
  "The utility for computing SHA256 checksums."
  :type 'string)

;; FIXME: Using 'su' is currently not supported by this package manager.
(defcustom kiss/KISS_SU
  (car (seq-filter 'executable-find
                   '("ssu" "sudo" "doas" "su")))
  "The utility that will be used to elevate priviledges."
  :type 'string)

;; Valid options:
;; bz2, gz, lz, lzma, xz, zst
(defcustom kiss/KISS_COMPRESS
  "gz"
  "The compression algorithm that should be used when making packages."
  :type 'string)

(defcustom kiss/KISS_PATH
  (split-string (getenv "KISS_PATH") ":")
  "A list of directories in decreasing precedence to look for packages in."
  :type '(string))

;; ===========================================================================

;; Internal function definitions, these are considered to not be stable.
;; It's best not to rely on them outside of this file.

(defun kiss--lst-to-str (lst)
  "(I) Convert LST to a string."
  (mapconcat (lambda (s) (format "%s" s)) lst " "))

(defun kiss--sanitize-ver-str (str)
  "(I) Sanitize a version string STR to be correctly compared against others."
  (replace-regexp-in-string
   "\n$" ""
   (replace-regexp-in-string
    " " ""
    str)))

;; FIXME: potentially change this function
;; to simply remove the final newline at the end of the text
;; and to then split on the newlines - it should result in the same
;; code as the following for kiss/manifest, but would allow this
;; code to be used other places too
(defun kiss--read-file (file-path)
  "(I) Read FILE-PATH as a list of lines, with empty newlines removed."
  (seq-remove
   (lambda (s) (string= "" s))
   (string-split (f-read-text file-path) "\n")))

(defun kiss--get-user-from-uid (uid)
  "(I) Return the name for UID.  `$ getent passwd' is parsed for the information."
  (car
   (remove
    nil
    (cl-mapcar
     (lambda (str)
       (if (string-match
            (rx bol
                (1+ (not ":")) ":"
                (0+ (not ":")) ":"
                (literal (number-to-string uid)) ":")
            str)
           (car (string-split str ":"))))
     ;; NOTE: there is a portability penalty here for using getent(1).
     ;; This will work fine on Linux and the *BSDs, but not on macOS.
     (string-split
      (shell-command-to-string "getent passwd") "\n" t)))))

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
  "(I) Run COMMAND as USER using `kiss/KISS_SU'."
  (shell-command (concat kiss/KISS_SU " -u " user " -- " command)))

(defun kiss--decompress (file-path out-path)
  "(I) Decompress FILE-PATH to OUT-PATH based on the file name."
  (let ((cmd
         (cond
          ((string-match-p (rx ".tar" eol)             file-path) "cat ")
          ((string-match-p (rx (or ".tbz" ".bz2") eol) file-path) "bzip2 -dc ")
          ((string-match-p (rx ".lz" eol)              file-path) "lzip -dc")
          ((string-match-p (rx (or ".tgz" ".gz") eol)  file-path) "gzip -dc ")
          ((string-match-p (rx ".lzma" eol)            file-path) "lzma -dcT0 ")
          ((string-match-p (rx (or ".txz" ".xz") eol)  file-path) "xz -dcT0 ")
          ((string-match-p (rx ".zst" eol)             file-path) "zstd -dcT0 "))))
    (if cmd
        (shell-command (concat cmd file-path " > " out-path)))))

(defun kiss--b3 (file-path)
  "(I) Run b3sum on FILE-PATH."
  (car
   (string-split
    (replace-regexp-in-string
     "\n$" ""
     ;; NOTE: b3sum is the only supported BLAKE3 utility at this time.
     (shell-command-to-string (concat "b3sum -l 33 " file-path)))
    " ")))

(defun kiss--sh256 (file-path)
  "(I) Run `kiss/KISS_CHK' with proper arguments on FILE-PATH."
  (let ((args
         (cond
          ((string-match-p "openssl"   kiss/KISS_CHK) " dgst -sha256 -r ")
          ((string-match-p "sha256sum" kiss/KISS_CHK) "")
          ((string-match-p "sha256"    kiss/KISS_CHK) " -r ")
          ((string-match-p "shasum"    kiss/KISS_CHK) " -a 256 ")
          ((string-match-p "digest"    kiss/KISS_CHK) " -a sha256 "))))
    (car
     (string-split
      (replace-regexp-in-string
       "\n$" ""
       (shell-command-to-string
        (concat kiss/KISS_CHK args file-path)))))))

;; Public code below.

;; -> kiss [a|b|c|d|i|l|p|r|s|u|U|v] [pkg]...
;; -> alternatives List and swap alternatives
;; ===========================================================================

(defun kiss/alternatives (&optional pkg path)
  (interactive)
  (if (or (eq nil pkg) (eq nil path))
      (mapcar
       (lambda (s)
         (let ((d (split-string s ">")))
           (list (car d)
                 (concat "/" (string-join (cdr d) "/"))
                 s)))
       (nthcdr 2 (directory-files kiss/choices-db-dir)))
    (kiss--pkg-swap pkg path)))

;; (benchmark-elapse (kiss/alternatives))
;; (kiss/alternatives "util-linux" "/usr/bin/mkswap")
;; (kiss/alternatives "busybox" "/usr/bin/mkswap")
;; (kiss/alternatives)

;; FIXME?: may need to have this code take in a general path
;; for pkg - this is so that this code can be reused to implement
;; the conflicts system.
;; pkg_manifest_replace() in kiss
(defun kiss--pkg-manifest-replace (pkg old new)
  "(I) Replace the line matching OLD in the manifest of PKG with NEW."
  ;; Replace the matching line in the manifest w/ the desired
  ;; replacement.
  ;; TODO: test this to make sure it is correct.
  (let* ((manifest-f (concat kiss/installed-db-dir pkg "/manifest"))
         (temp-f     (kiss--make-temp-file))
         (owner      (kiss--get-owner-name manifest-f))
         (manifest-t (kiss--manifest-to-string
                      (reverse
                       (cl-sort
                        (cl-mapcar (lambda (s) (if (string= s old) new s))
                                   (kiss/manifest pkg))
                        'string-lessp)))))

    (f-write-text manifest-t 'utf-8 temp-f)

    ;; TODO: see if this can be avoided.
    ;; Ensure the ownership is preserved.
    ;; NOTE: chown can work with uids instead of names
    (kiss--shell-command-as-user
     (concat "chown " owner ":" owner " " temp-f) owner)
    ;; Ensure the permissions are set correctly.
    (kiss--shell-command-as-user
     (concat "chmod 644 " temp-f) owner)
    ;; Move it into place.
    (kiss--shell-command-as-user
     (concat "mv -f " temp-f " " manifest-f)
     owner)))

;; (kiss--manifest-to-string (kiss/manifest "xdo"))

;; FIXME: Either fix upstream Emacs/f.el or keep using this.
;; NOTE: DO NOT USE THIS ANYWHERE THAT ISN'T ABSOLUTELY NECESSARY.
(defun kiss--file-exists-p (file-path)
  "(I) This function should NOT exist.
However, `file-exists-p' and `file-symlink-p' are fundamentally broken when it
comes to broken symlinks.  Hence the need for this function.
This function returns t if FILE-PATH exists and nil if it doesn't."
  (or
   (eq 0
       (shell-command (concat "test -f " file-path)))
   (eq 0
       (shell-command (concat "test -h " file-path)))))

(defun kiss--single-quote-string (str)
  "(I) Add quotes around STR.  Useful when interacting with the cli."
  (concat "'" str "'"))

(defun kiss--pkg-swap (pkg path)
  "(I) Swap the owner of PATH to PKG, modifying the relevant package manifests."
  (if (kiss--pkg-is-installed-p pkg)
      ;; NOTE: The quotes surrounding the string are very important.
      ;; This is because this string is only interpreted as a command argument.
      ;; This means that the shell can mangle it if it is not properly
      ;; esacped.
      (let* ((alt      (string-replace "/" ">" path))
             (alt-path (concat kiss/choices-db-dir pkg alt))
             (path-own (kiss/owns path)))
        (if (kiss--file-exists-p
             (kiss--single-quote-string alt-path))
            (progn
              ;; If the file is owned by a package in the database.
              (if path-own
                  (progn
                    (message (concat "Swapping " path
                                     " from " path-own
                                     " to " pkg))
                    ;; Save the path into kiss/choices-db-dir
                    (kiss--shell-command-as-user
                     (concat "cp -Pf " path " "
                             (kiss--single-quote-string
                              (concat kiss/choices-db-dir path-own alt)))
                     (kiss--get-owner-name path))

                    ;; Update the manifest file to reflect the new version.
                    (kiss--pkg-manifest-replace
                     path-own path (concat kiss/choices-db-dir path-own alt))))
              ;; Move over our new desired alternative to the real file.
              (kiss--shell-command-as-user
               (concat "mv -f " (kiss--single-quote-string alt-path)
                       " " path)
               (kiss--get-owner-name path))
              (kiss--pkg-manifest-replace pkg alt-path path))))))



;; (f-symlink-p "/var/db/kiss/choices/gawk\\>usr\\>bin\\>awk")
;; (f-exists?  "/var/db/kiss/choices/busybox\\>usr\\>bin\\>sh")
;; (kiss--file-exists-p "/var/db/kiss/choices/gawk\\>usr\\>bin\\>awk")
;; (if (kiss/owns "/usr/bin/awk") 1)
;; (cl-remove-if-not
;;  (lambda (s) (string= "gawk" s))
;;  (cl-mapcar #'car (kiss/alternatives)))
;; (concat "mawk" (string-replace "/" ">" "/usr/bin/awk"))

(defun kiss--manifest-to-string (pkg-manifest)
  "(I) Convert our internal representation of PKG-MANIFEST into a string."
  (concat (mapconcat #'identity pkg-manifest "\n") "\n"))

;; (kiss--manifest-to-string (kiss/manifest "xdo"))

(defun kiss/manifest (pkg)
  "Return a list of all files owned by PKG."
  (kiss--read-file
   (concat kiss/installed-db-dir pkg "/manifest")))

;; (cl-remove-if
;;  (lambda (s) (string= "" s))
;;  (string-split
;;   (f-read-text
;;    (concat kiss/installed-db-dir pkg "/manifest"))
;;   "\n")))

;; (benchmark-elapse (kiss/manifest "kiss"))

(defun kiss--get-installed-manifest-files ()
  "(I) Return a list of all of the installed manifest files."
  (mapcar
   '(lambda (pkg) (concat kiss/installed-db-dir pkg "/manifest"))
   (mapcar 'car (kiss/list))))

(defun kiss/owns (file-path)
  ;; TODO: See if this can be made a little less ugly.
  (let* ((cmd (concat "grep " (rx bol (literal file-path) eol) " "
                      (kiss--lst-to-str
                       (kiss--get-installed-manifest-files))))
         (cmd-out (shell-command-to-string cmd)))
    (if (not (string-empty-p cmd-out))
        (car
         (string-split
          (replace-regexp-in-string kiss/installed-db-dir "" cmd-out) "/")))))

;; (cl-mapcar
;;  (lambda (file)
;;    (list (kiss/owns file) file))
;;  (delete-dups (cl-mapcar #'cadr (kiss/alternatives))))

;; (rgrep "/usr/bin/awk$" "manifest" "/var/db/kiss/installed/")

(defun kiss/preferred ()
  (mapcar
   ;; NOTE: this may split files with ':' in the name...
   ;; Do the final split of package and file.
   (lambda (s) (string-split s ":"))
   ;; Split up each line from each other.
   (string-split
    ;; Clean up the string to just contain the package name
    ;; on the left and the file on the right.
    (replace-regexp-in-string
     kiss/installed-db-dir ""
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
         (delete-dups
          (mapcar
           (lambda (file-str)
             (concat
              "/"
              (mapconcat #'identity
                         (cdr (split-string file-str ">"))
                         "/")))
           (nthcdr 2 (directory-files kiss/choices-db-dir))))
         "\\n")
        "'"
        ;; Now for the piping into grep.
        " | "
        "grep -Fxf - "
        (kiss--lst-to-str
         (kiss--get-installed-manifest-files))
        " /dev/null"))))
    "\n")))

(defun kiss--get-pkg-dependencies (pkg)
  "(I) Get the dependencies of PKG as a list, nil if PKG has no dependencies."
  (let ((depends-file (concat (car (kiss/search pkg)) "/depends")))
    (if (file-exists-p depends-file)
        (seq-remove
         #'string-empty-p
         ;; All of this below is to emulate `awk '{print $1}' < file'
         (mapcar (lambda (s) (car (string-split s " ")))
                 (string-split
                  (replace-regexp-in-string
                   "#.*$" ""
                   (f-read-text depends-file))
                  "\n"))))))

;; NOTE: I would like to revisit this function to see if it
;; could be written in a more functional style, so as to allow
;; eaiser porting of the logic to other functional programming
;; languages.
(defun kiss--get-pkg-dependency-graph (pkg-lst)
  "(I) Generate a graph of the dependencies for PKG-LST."
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
                   (dep-deps (kiss--get-pkg-dependencies dep))
                   (item `(,dep ,dep-deps)))
              (if (not (member item res))
                  (setq res (cons item res)))
              (setq queue (append dep-deps (cdr queue)))))))
    res))

;; (length  (kiss--get-pkg-dependency-graph '("gcc")))

;; (equal
;;  (kiss--get-pkg-dependency-graph '("gcc" "llvm"))
;;  (kiss--get-pkg-dependency-graph-rec '("gcc" "llvm")))

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

;; (benchmark-elapse (kiss--get-pkg-dependency-graph (mapcar #'car (kiss/list))))
;; (benchmark-elapse (kiss--get-pkg-dependency-graph-rec (mapcar #'car (kiss/list))))

;; (kiss--get-pkg-dependency-graph '("kiss" "cmake"))
;; (kiss--get-pkg-dependency-order "cmake")
;; TODO: consider moving invert-pkg-dependency-graph to tsort.el

(defun kiss--get-pkg-dependency-order (pkg-lst)
  "(I) Return the proper build order of the dependencies for each pkg in PKG-LST."
  (tsort (kiss--get-pkg-dependency-graph pkg-lst)))

;; TODO: make the output list prettier (ie, should be a list of pkgs,
;; not depends files)
(defun kiss--get-pkg-make-dependents (pkg)
  "(I) Return a list of installed packages that have a make dependency on PKG, nil if there are no dependents."
  (mapcar
   (lambda (dep-file)
     (replace-regexp-in-string
      "/depends" ""
      (replace-regexp-in-string
       kiss/installed-db-dir ""
       dep-file)))
   (seq-filter
    (lambda (depfile)
      (string-match (rx bol (literal pkg) (one-or-more " ") (literal "make"))
                    (f-read-text depfile)))
    (seq-filter
     #'file-exists-p
     (mapcar (lambda (p) (concat kiss/installed-db-dir p "/depends"))
             (nthcdr 2 (directory-files kiss/installed-db-dir)))))))
;; (kiss--get-pkg-make-dependents "ant")

(defun kiss--get-pkg-make-orphans ()
  "(I) Return a list of installed packages that were only required as a make dependency."
  ;; NOTE: This function is pretty slow at the moment.
  (cl-remove-if-not
   (lambda (pkg)
     (and (eq (kiss--get-pkg-hard-dependents pkg) nil)
          (not
           (eq (kiss--get-pkg-make-dependents pkg) nil))))
   (mapcar #'car (kiss/list))))

;; (kiss--get-pkg-make-orphans)
;; (length (kiss--get-pkg-make-orphans))

;; TODO: make the output list prettier (ie, should be a list of pkgs,
;; not depends files)
(defun kiss--get-pkg-hard-dependents (pkg)
  "(I) Return a list of installed packages that have a runtime dependency on PKG, nil if there are no dependents."
  (mapcar
   (lambda (dep-file)
     (replace-regexp-in-string
      "/depends" ""
      (replace-regexp-in-string
       kiss/installed-db-dir ""
       dep-file)))
   (seq-filter
    (lambda (depfile)
      (string-match (rx bol (literal pkg) (zero-or-more " ") eol)
                    (f-read-text depfile)))
    (seq-filter
     #'file-exists-p
     (mapcar (lambda (p) (concat kiss/installed-db-dir p "/depends"))
             (nthcdr 2 (directory-files kiss/installed-db-dir)))))))

;; (kiss--get-pkg-hard-dependents "mpfr")

(defun kiss--get-pkg-missing-dependencies (pkg)
  "(I) Return a list of dependencies that are missing for PKG, nil otherwise."
  (seq-remove
   #'kiss--pkg-is-installed-p
   (delete-dups
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
          (kiss/preferred))))
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

;; (length
;;  (cl-remove-if-not
;;   #'f-dir?
;;   (string-split
;;    (f-read-text (concat kiss/installed-db-dir "gcc" "/manifest"))
;;    "\n")))

;; -> build        Build packages
;; ===========================================================================

;; Just some testing code to get all of the missing dependencies for the
;; pkgs to be updated.
;;(kiss--get-pkg-order
;; (flatten-list
;; (cl-remove-if
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
               ((f-dir? file-path) (concat cfp "/"))
               (t cfp ))))

          ;; Filter out libtool .la files and charset.alias
          (cl-remove-if
           (lambda (fp)
             (string-match-p
              (rx (or (literal "charset.alias") (literal ".la")) eol)
              fp))
           (directory-files-recursively dir "." t)))))

    ;; Admittedly I'm not a huge fan of the below code, but it does work.
    (if (member "/var/db/kiss/installed/" files-and-dirs)
        (let ((pkg-db-dir
               (car (cl-remove-if-not
                     (lambda (s)
                       (string-match-p
                        (rx "/var/db/kiss/installed/" (1+ (not "/")) "/" eol)
                        s))
                     files-and-dirs))))
          ;; add /var/db/kiss/installed/<pkg>/manifest
          ;; and /var/db/kiss/isntalled/<pkg>/etcsums
          (if (member "/etc" files-and-dirs)
              (setq files-and-dirs
                    (cons (concat pkg-db-dir "etcsums") files-and-dirs)))
          (setq files-and-dirs
                (cons (concat pkg-db-dir "manifest") files-and-dirs))))

    ;; sort -ur
    (reverse
     (cl-sort (delete-dups files-and-dirs) 'string-lessp))))

(defun kiss--get-pkg-version (pkg)
  "(I) Get the version for PKG using the car of `kiss/search'."
  (let ((ks (kiss/search pkg)))
    (if ks
        (let ((pdir (car ks)))
          (replace-regexp-in-string
           "\n$" ""
           ;; TODO: see if there is a way to avoid
           ;; depending on f.el
           (f-read-text (concat pdir "/version")))))))

(defun kiss--get-pkg-bin-name (pkg version)
  "(I) Return the proper name for the binary for PKG at VERSION."
  (concat pkg "@"
          (replace-regexp-in-string " " "-" version)
          ".tar." kiss/KISS_COMPRESS ))

(defun kiss--get-compression-command ()
  "(I) Return the proper command for the compression specified by `kiss/KISS_COMPRESS'."
  (cond ((string= "bz2" kiss/KISS_COMPRESS) "bzip2 -z")
        ((string= "gz" kiss/KISS_COMPRESS) "gzip -6")
        ((string= "lzma" kiss/KISS_COMPRESS) "lzma -z")
        ((string= "lz" kiss/KISS_COMPRESS) "lzip -z")
        ((string= "xz" kiss/KISS_COMPRESS) "xz -z")
        ((string= "zstd" kiss/KISS_COMPRESS) "zstd -z")
        (t nil)))

(defun kiss--get-pkg-cached-bin (pkg)
  "(I) Return the path of the binary for PKG, nil if PKG has no binary in the cache."
  (let ((ver (kiss--get-pkg-version pkg)))
    (if ver
        (let ((bin (concat kiss/KISS_BINDIR
                           (kiss--get-pkg-bin-name pkg ver))))
          (if (file-exists-p bin) bin)))))

(defun kiss--get-tmp-destdir ()
  "(I) Return a directory that can be used as a temporary destdir."
  ;; NOTE: This is not a *perfect* system, however, it is not as easy to
  ;; do the pid trick that the shell implementation of kiss does.
  ;; So the compromise is to pick a random number from 1 to 30000.
  (let ((rn (message "%s" (mod (abs (random)) 30000))))
    (while (file-exists-p (concat kiss/KISS_TMPDIR rn))
      (setq rn (message "%s" (mod (abs (random)) 30000))))
    (make-directory (concat kiss/KISS_TMPDIR rn) t)
    (concat kiss/KISS_TMPDIR rn)))

(defun kiss/fork (pkg dir)
  "Fork PKG to DIR."
  (eq 0 (shell-command
         (concat "cp -Lrf " (car (kiss/search pkg)) " " dir))))


(defun kiss--make-tarball-of-dir (dir file-path)
  "(I) Make a compressed tarball of DIR saved into FILE-PATH."
  ;; FIXME: need to remove the reliance on tar's -C flag, since it
  ;; is noted in upstream kiss as being a source of portability issues.
  (eq 0
      (shell-command
       (concat "tar -cf - -C " dir " . | "
               (kiss--get-compression-command)
               " > " file-path))))

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
         (delete-dups
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
                      mani))))))))
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

;; FIXME: rm missing-deps check here and move that up to the caller.
;; FIXME: should try to see what functionality I can move out of this
;; function
(defun kiss--build-pkg (pkg)
  "(I) Build PKG, return t if PKG was built successfully, nil otherwise."
  (let ((missing-deps (kiss--get-pkg-missing-dependencies pkg))
        (pkg-ver (replace-regexp-in-string
                  " " "-"
                  (kiss--get-pkg-version pkg))))
    ;; Install/build missing dependencies
    (if missing-deps
        (mapcar #'kiss--try-install-build missing-deps))

    ;; Recheck to make sure that we aren't missing any deps.
    (setq missing-deps (kiss--get-pkg-missing-dependencies pkg))

    (if (not missing-deps)
        (let* ((build-script (concat (car (kiss/search pkg)) "/build"))
               (proc-dir     (kiss--get-tmp-destdir))
               (build-dir    (concat proc-dir "/build/" pkg "/"))
               (install-dir  (concat proc-dir "/pkg/" pkg))
               (k-el-build   (concat proc-dir "/build-" pkg "-kiss-el")))

          ;; Extract pkg's sources to the build directory.
          (kiss--extract-pkg-sources pkg build-dir)
          ;; Essentially, we want to build out a script that contains
          ;; all of the info that we need.

          ;; * Environment to build the package in
          ;; * The package build script
          ;; * The proper arguments to said pkg build script

          ;; FIXME: need to also save a log of the build...
          ;; ^^ do this using 'tee(1)' or just piping directly into the
          ;; log itself
          ;; ~/.cache/kiss/logs/$(date +%Y-%m-%d)/<pkg>-$(date +%Y-%m-%d-%H:%M)-<procnum>
          (f-write-text
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

            build-script " " install-dir " " pkg-ver)
           ;; Write this script to a temporary location.
           'utf-8 k-el-build)

          ;; Mark the script as executable.
          (shell-command (concat "chmod +x " k-el-build))

          ;; NOTE: will need to be somewhat more clever when
          ;; executing the build script, since I would like to be able
          ;; to see the build as it is occurring, like in
          ;; async-shell-command

          ;; Now actually execute the script.
          (message (concat "Building " pkg " at version: " pkg-ver))
          (if (eq 0 (shell-command (concat "sh -xe " k-el-build)))
              ;; Success
              (progn
                (message (concat "Successful Build!"))
                ;; Now we have to fork over the package files that we
                ;; used to the repo dir.
                (let ((pkg-install-db
                       (concat install-dir "/var/db/kiss/installed/")))
                  (make-directory pkg-install-db t)
                  (kiss/fork pkg pkg-install-db)

                  ;; FIXME: look over kiss code & implement /dev/null
                  ;; for symlinks
                  ;; Need to compute etcsums if they exist.
                  (let* ((manifest-lst
                          (kiss--get-manifest-for-dir install-dir))
                         (etc-files
                          (cl-remove-if-not
                           (lambda (s)
                             (and (string-match-p (rx bol "/etc")s)
                                  (f-file? (concat install-dir "/" s))))
                           manifest-lst)))

                    ;; If we have any etcfiles, create etcsums
                    (if etc-files
                        (f-write-text
                         (mapconcat
                          #'identity
                          (mapcar #'kiss--b3 etc-files)
                          "\n")
                         'utf-8 (concat pkg-install-db pkg "/etcsums")))

                    ;; Next, create the manifest
                    (f-write-text
                     (kiss--manifest-to-string manifest-lst)
                     'utf-8 (concat pkg-install-db pkg "/manifest")))

                  ;; FIXME: need to optionally strip the binaries based off
                  ;; of the KISS_STRIP env variable.

                  ;; TODO: finish up this impl.
                  ;; FIXME: also need to do dependency fixing
                  (kiss--get-potential-binary-files
                   (kiss--read-file (concat pkg-install-db pkg "/manifest"))))

                ;; Finally create the tarball
                (message (concat "Creating tarball for " pkg))
                (kiss--make-tarball-of-dir
                 install-dir
                 (concat kiss/KISS_BINDIR
                         (kiss--get-pkg-bin-name pkg pkg-ver)))

                ;; rm the build directory
                (message (concat "Removing the build directory (" proc-dir ")"))
                (shell-command (concat "rm -rf -- " proc-dir))
                ;; Have the expr eval to t.
                t)
            ;; Failure
            (progn
              (message "Build failed")
              ;; Have the expr eval to nil.
              nil))))))

;; (kiss--build-pkg "xdo")

;; FIXME: add in checks to the appropriate places.
(defun kiss--build-install (pkg)
  "(I) Attempt to build and install PKG, nil if unsuccessful."
  (if (kiss/build pkg)
      (kiss/install pkg)))

(defun kiss--try-install-build (pkg)
  "(I) Attempt to install a binary of PKG, else build and install PKG."
  (if (kiss--get-pkg-cached-bin pkg)
      (kiss/install pkg)
    (kiss--build-install pkg)))

(defun kiss/build (pkgs-l)
  (interactive)
  (cond ((listp pkgs-l)
         (progn
           (mapcar #'kiss--build-pkg
                   (kiss--get-pkg-order pkgs-l))))
        ((atom pkgs-l)
         (progn
           (kiss/download pkgs-l)
           (kiss--build-pkg pkgs-l)))))

;; -> checksum     Generate checksums
;; ===========================================================================

;; Initial working impl of kiss/checksum below; need to refactor some of
;; the functionality since kiss/download has similar needs.

(defun kiss--get-pkg-repo-checksums (pkg)
  "(I) Return the list of checksums for PKG from a repo or nil if checksums don't exist."
  (let ((checksums-file (concat (car (kiss/search pkg)) "/checksums")))
    (if (file-exists-p checksums-file)
        (cl-remove-if
         #'string-empty-p
         (string-split
          (f-read-text checksums-file) "\n")))))

(defmacro kiss--tps-env (pkg tps expr)
  ;; "(I) Macro to aide in parsing TPS, and using the values in EXPR."
  ;; Extract out each of the variables.
  `(let* ((pkg-source-cache-dir (concat kiss/KISS_SRCDIR ,pkg "/"))
          (type                 (car  ,tps))
          (uri                  (car  (cdr ,tps)))
          (sub-dir              (cadr (cdr ,tps)))
          (dest-dir             (concat pkg-source-cache-dir sub-dir)))
     ,expr))

(defun kiss--get-pkg-local-checksums (pkg)
  "(I) Return the list of checksums for PKG from the files on disk, or nil."
  (cl-mapcar
   #'kiss--b3
   (cl-mapcar
    #'cdr
    (cl-remove-if
     ;; Filter out 'git' sources.
     ;; TODO: implement a lookup for future mercurial sources.
     (lambda (tps-cache)
       (string= "git" (car (car tps-cache))))
     (-zip
      (kiss--get-type-pkg-sources pkg)
      (kiss--get-pkg-sources-cache-path pkg))))))

(defun kiss--pkg-verify-local-checksums (pkg)
  "(I) Return t if local checksums match up with the repo checksums for PKG, nil otherwise."
  (eq nil
      (cl-remove-if
       (lambda (pair) (string= (car pair) (cdr pair)))
       (-zip
        (kiss--get-pkg-repo-checksums  pkg)
        (kiss--get-pkg-local-checksums pkg)))))

(defun kiss/checksum (pkgs-l)
  (cond
   ((listp pkgs-l)
    (cl-mapcar #'kiss/checksum pkgs-l))
   ((atom pkgs-l)
    (let* ((pkg-path (car (kiss/search pkgs-l)))
           (chk-path (concat pkg-path "/checksums"))
           (chk-sums (mapconcat
                      #'identity
                      (kiss--get-pkg-local-checksums pkgs-l) "\n")))
      (if (and (kiss--am-owner-p chk-path)
               (not (string-empty-p chk-sums)))
          (f-write-text
           chk-sums
           'utf-8 chk-path))))))

;; (kiss--pkg-verify-local-checksums "chromium")

;; -> download     Download sources
;; ===========================================================================

(defun kiss/download (pkgs-l)
  (interactive "sQuery: ")
  (cond ((listp pkgs-l)
         (cl-mapcar #'kiss/download pkgs-l))
        ((atom pkgs-l)
         (kiss--download-pkg-sources pkgs-l))
        (t nil)))

;; (kiss/download '("kiss" "gdb"))
;; (kiss/download '("hugs"))

(defun kiss--get-pkg-sources (pkg)
  "(I) Return a list of sources for PKG, or nil if PKG has no sources file."
  (let* ((pkg-repo    (car (kiss/search pkg)))
         (pkg-sources (concat pkg-repo "/sources")))
    (if (file-exists-p pkg-sources)
        (progn
          ;; Remove any non source lines.
          (cl-remove-if
           (lambda (lst) (string-empty-p (car lst)))
           (mapcar
            (lambda (pkg-dest-line)
              ;; Sanitize each line
              (string-split
               (replace-regexp-in-string
                (rx (one-or-more " ")) " "
                pkg-dest-line)
               " "))
            (string-split
             (f-read-text (concat pkg-repo "/sources"))
             "\n"))))
      ;; Return nil if there are no sources for the package.
      ;; NOTE: This nil does not mean failure.
      nil)))

(defun kiss--get-pkg-sources-type (pkg-source)
  "(I) Return the type of PKG-SOURCE."
  (let ((pkg-url (car pkg-source)))
    ;; TODO: need to ensure that this is the same expected behavior as
    ;; upstream.
    (cond
     ((string-match-p (rx bol "git+") pkg-url) "git")
     ((string-match-p (rx "://") pkg-url) "remote")
     (t "local"))))

;; FIXME: make a macro for parsing out the clean url, the dest folder & commit
;; since that information would also be useful for other vc systems like
;; fossil and hg.
(defun kiss--download-git-source (url dest-dir)
  "(I) Download git URL to `kiss/KISS_SRCDIR' in the folder DEST-DIR."
  ;; NOTE: This currently does not support sources like the following:
  ;; git+https://github.com/user/project@somebranch#somecommit
  ;; However, I have yet to see this combo out in the wild in kiss linux.
  ;; So... It's not a bug (yet). Also, it just doesn't make sense to do anyways.
  ;; TODO: consider moving this code out?
  (let* ((u (replace-regexp-in-string
             (rx bol "git+") ""
             url))
         (clean-url (car (string-split u (rx (or "#" "@")))))
         (dest-folder
          (concat dest-dir "/"
                  (car (reverse (string-split clean-url "/")))))
         ;; Set branch/commit if it's specified.
         (com (nth 1 (string-split u (rx (or "#" "@"))))))

    ;; If a specific branch/commit isn't specified, default to FETCH_HEAD.
    (if (eq com nil)
        (setq com "HEAD"))
    ;; Only make the directory if it doesn't exist.
    (if (not (file-exists-p dest-folder))
        (make-directory dest-folder))
    ;; Initialize the git repo if it doesn't exist.
    (if (not (file-exists-p (concat dest-folder ".git")))
        (shell-command (concat "git init " dest-folder)))
    ;; Save our current working directory.
    (let ((opwd (getenv "PWD")))
      (cd dest-folder)
      (if (not
           (eq 0
               (shell-command
                (concat "git remote set-url origin " clean-url " 2> /dev/null"))))
          (shell-command
           (concat "git remote add origin " clean-url)))
      (shell-command (concat "git fetch --depth=1 origin " com))
      (shell-command (concat "git reset --hard FETCH_HEAD"))
      ;; Change back to our old working directory
      (cd opwd))))

(defun kiss--make-temp-file ()
  "(I) Make a temporary file using the `mktemp' utility."
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "mktemp")))

(defun kiss--get-download-utility-arguments ()
  "(I) Get the proper arguments for the `kiss/KISS_GET' utility."
  (cond
   ((string= kiss/KISS_GET "aria2c") " -d / -o ")
   ((string= kiss/KISS_GET "axel")   " -o ")
   ((string= kiss/KISS_GET "curl")   " -fLo ")
   ((or (string= kiss/KISS_GET "wget") (string= kiss/KISS_GET "wget2")) " -O ")))

(defun kiss--download-remote-source (url dest-dir)
  "(I) Download URL to DEST-DIR using `kiss/KISS_GET'."
  ;; TODO: check and make sure this is the right way to create this file name.
  (let* ((file-name (car (reverse (string-split url "/"))))
         (dest-path (concat dest-dir "/" file-name)))
    ;; Only download if the file doesn't already exist.
    (if (not (file-exists-p dest-path))
        (shell-command
         (concat kiss/KISS_GET " "
                 url
                 (kiss--get-download-utility-arguments)
                 dest-path)))))

(defun kiss--download-local-source (uri dest-dir)
  "(I) Copy URI to DEST-DIR using cp(1)."
  (let ((file-name (car (reverse (string-split uri "/")))))
    ;; FIXME: double check to make sure this is correct - I have a hunch
    ;; that it's not.
    (if (not (file-exists-p uri))
        (shell-command
         (concat "cp " uri
                 " " (concat dest-dir file-name))))))

(defun kiss--get-pkg-sources-cache-path (pkg)
  "(I) Return the cache path in `kiss/KISS_SRCDIR' for each of PKG's sources."
  (let* ((pkg-source-cache-dir (concat kiss/KISS_SRCDIR pkg "/"))
         (type-pkg-sources (kiss--get-type-pkg-sources pkg)))
    (cl-mapcar
     (lambda (tps)
       (kiss--tps-env pkg tps
                      (progn
                        (cond
                         ;; This one is a bit messy since we have to be able to parse
                         ;; out the useful information in a git source.
                         ((string= "git" type)
                          (let ((u (replace-regexp-in-string
                                    (rx bol "git+") ""
                                    uri)))
                            (concat
                             dest-dir "/"
                             (car
                              (reverse
                               (string-split
                                (car (string-split u (rx (or "#" "@")))) "/"))))))

                         ((string= "remote" type)
                          (concat dest-dir "/" (car (reverse (string-split uri "/")))))
                         ((string= "local" type)
                          ;; (if (string= (rx bol "/" (regexp ".*")) "/asdf")
                          (if (string-match (rx bol "/") uri)
                              ;; Absolute path.
                              uri
                            ;; Relative path.
                            (concat (car (kiss/search pkg)) "/" uri)))))))
     type-pkg-sources)))

(defun kiss--pkg-sources-available-p (pkg)
  "(I) Return t if all of the sources for PKG are available locally, nil otherwise."
  (not (member nil (cl-mapcar
                    #'file-exists-p
                    (kiss--get-pkg-sources-cache-path pkg)))))

(defun kiss--get-type-pkg-sources (pkg)
  "(I) Return a list containing the source type, followed by the source for PKG."
  (let ((pkg-sources (kiss--get-pkg-sources pkg)))
    (-zip
     (mapcar 'kiss--get-pkg-sources-type pkg-sources)
     pkg-sources)))

(defun kiss--download-pkg-sources (pkg)
  "(I) Download the sources for PKG into `kiss/KISS_SRCDIR'."
  (let* ((pkg-source-cache-dir (concat kiss/KISS_SRCDIR pkg "/"))
         (type-pkg-sources (kiss--get-type-pkg-sources pkg)))
    (cl-mapcar
     (lambda (tps)
       (kiss--tps-env pkg tps
                      (progn
                        ;; Make the cache directory if it doesn't already exist.
                        (if (not (file-exists-p dest-dir))
                            (make-directory dest-dir))

                        ;; Switch based on the type of source that it is.
                        (cond
                         ((string= type "remote")
                          (kiss--download-remote-source uri dest-dir))
                         ((string= type "git")
                          (kiss--download-git-source uri dest-dir))
                         ((string= type "local")
                          (if (string-match (rx bol "/") uri)
                              ;; Absolute path.
                              (kiss--download-local-source uri dest-dir))
                          ;; Relative path.
                          (kiss--download-local-source
                           (concat (car (kiss/search pkg)) "/" uri) dest-dir))))))
     type-pkg-sources)))

;; (kiss--get-pkg-sources "shen-cl")
;; (concat (car (kiss/search "shen-cl")) "/" "patches/ccl-and-ecl-support.patch"))
;; (kiss--download-pkg-sources "shen-cl")

;; pkg_source_tar()
(defun kiss--extract-tarball (tarball dir)
  "(I) Extract TARBALL to DIR.  Emulates GNU Tar's --strip-components=1."
  (let ((decomp-tarball (kiss--make-temp-file)))
    ;; Decompress the tarball.
    (kiss--decompress tarball decomp-tarball)
    ;; Extract the tarball.
    (shell-command (concat "tar xf " decomp-tarball " -C " dir))
    ;; Get all of the top level directories from the tarball.
    (cl-mapcar
     (lambda (tld)
       (let* ((temp-f (kiss--make-temp-file))
              (temp-d (concat temp-f "-" tld)))
         (message "%s" (eq 0
                           (shell-command
                            (concat "mv -f " (concat dir "/" tld) " " temp-d))))

         ;; NOTE: we need to call directory-files twice here, since
         ;; First do the mv's
         (cl-mapcar
          (lambda (f)
            (shell-command
             (concat "mv -f " (concat temp-d f) " " dir)))
          (nthcdr 2 (directory-files temp-d)))

         ;; Then do the cp's
         (let ((files (nthcdr 2 (directory-files temp-d))))
           (if files
               (cl-mapcar
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

     ;; Get a list of all of the top level directories in the tarball.
     ;; TODO: see if I can remove the pipe into sort.
     (seq-remove
      #'string-empty-p
      (string-split
       (shell-command-to-string
        (concat "tar tf " tarball " | sort -ut / -k1,1"))
       "\n")))

    ;; Remove our decompressed tarball now that we are done with it.
    (kiss--shell-command-as-user
     (concat "rm -f -- " decomp-tarball)
     (kiss--get-owner-name decomp-tarball))))

;; pkg_extract() in kiss
(defun kiss--extract-pkg-sources (pkg dir)
  "(I) Extract the cached sources of PKG to DIR."
  (cl-mapcar
   (lambda (type-path)
     (let* ((type   (car  (car type-path)))
            (subdir (cdr (car type-path)))
            (cache  (cdr  type-path))
            (outdir (concat dir "/" subdir)))
       ;; Make the subdir if it does not exist already.
       (if (not (file-directory-p outdir))
           (make-directory outdir t))
       (cond
        ;; If the source type is a git repo:
        ((string= type "git")
         (shell-command (concat "cp -PRf " cache "/. " outdir)))

        (t (if (kiss--str-tarball-p cache)
               (kiss--extract-tarball cache outdir)
             (shell-command (concat "cp -PRf " cache " " outdir)))))))
   ;; Get the type of each cached pkg source w/ the source itself.
   (-zip
    (cl-mapcar (lambda (tps) (cons (car tps) (nth 2 tps))) (kiss--get-type-pkg-sources pkg))
    (kiss--get-pkg-sources-cache-path pkg))))

(defun kiss--str-tarball-p (str)
  "(I) Predicate to determine if STR matches the regex for a tarball."
  (string-match-p
   (rx
    (or (: "t" any "z")
        (: "t" any "z")
        (: "tar")
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

(defun kiss--install-tarball (tarball)
  "(I) Install TARBALL if it is a valid kiss package."
  ;; FIXME: maybe error out here?
  (if (not (f-exists? tarball))
      nil)

  (let* ((proc-dir       (kiss--get-tmp-destdir))
         (extr-dir       (concat proc-dir "/extracted"))
         (decomp-tarball (kiss--make-temp-file)))

    ;; (split-string
    ;;  (shell-command-to-string (concat "tar tf " tarball)) "\n")


    (make-directory extr-dir t)

    ;; NOTE: I would like to switch to using my already written
    ;; extract-tarball function instead of having to spell it out
    ;; manually here, however when attempting to use it I end
    ;; up getting odd errors likely due to the extra processing that
    ;; occurs.
    ;; (kiss--extract-tarball tarball proc-dir)

    (kiss--decompress tarball decomp-tarball)
    (shell-command
     (concat "tar xf " decomp-tarball " -C " extr-dir))
    (kiss--remove-file decomp-tarball)

    (let ((pkg
           ;; FIXME: this code is ugly
           (car (reverse
                 (cl-remove-if
                  #'string-empty-p
                  (string-split
                   (cl-remove-if-not
                    (lambda (str)
                      (string-match
                       (rx "/var/db/kiss/installed/" (1+ (not "/")) "/" eol) str))
                    (kiss--get-manifest-for-dir extr-dir)) "/"))))))

      (if (not pkg)
          (error "unable to detemine the package"))

      ;; assume that the existence of the manifest file is all that
      ;; makes a valid KISS pkg.
      (if (not (file-exists-p
                (concat "/var/db/kiss/installed/" pkg "/manifest")))
          (message "kiss/install: Not a valid kiss package!"))

      ;; Now that the pkg is verified to be a kiss pkg, we need
      ;; to validate the manifest that was shipped with the pkg.

      (if (not
           ;; Remove all of the t values, since it will result in the
           ;; list being empty if it was successful.
           (cl-remove-if
            #'identity
            (cl-mapcar

             ;; Test if the file is either a directory -or- a file/symlink
             (lambda (fp)
               (or (file-directory-p
                    (concat extr-dir fp))
                   (kiss--file-exists-p
                    (concat extr-dir fp))))

             ;; Yes this code is copied straight from `kiss/manifest',
             ;; I'm hoping to factor it out.
             (kiss--read-file
              (concat extr-dir "/var/db/kiss/installed/" pkg "/manifest")))))
          (message "kiss/install: manifest is valid!"))

      ;; (not (cl-remove-if #'identity '(nil nil nil)))
      ;; (not (cl-remove-if #'identity '(t t t)))

      ;; pkg_conflicts()
      ;; FIXME: impl

      ;; If the pkg is already installed (and this is an upgrade)
      ;; make a backup of the manifest and etcsum files
      (if (kiss--pkg-is-installed-p pkg)
          (progn
            (shell-command
             (concat "cp " kiss/installed-db-dir pkg "/manifest"
                     " " proc-dir "/manifest-copy"))
            (if (file-exists-p kiss/installed-db-dir pkg "/etcsums")
                (shell-command
                 (concat "cp " kiss/installed-db-dir pkg "/etcsums"
                         " " proc-dir "/etcsums-copy")))))

      ;; generate a list of files which exist in the current (installed)
      ;; manifest that do not exist in the new (to be installed) manifest.

      ;; Reverse the manifest file so that we start shallow, and go deeper
      ;; as we iterate through each item. This is needed so that directories
      ;; are created in the proper order

      ;; FIXME: finish this func
      nil)))

(defun kiss/install (pkgs-l)
  (interactive)
  (cond
   ((listp pkgs-l)
    (mapcar #'kiss/install pkgs-l))
   ((atom pkgs-l)
    ((let* ((tarball
             (cond ((file-exists-p pkgs-l) pkgs-l)
                   (t (kiss--get-pkg-cached-bin pkgs-l)))))
       (if (not tarball)
           (error (concat "kiss/install: tarball does not exist for " pkgs-l)))
       (kiss--install-tarball tarball))))))

(defun kiss--pkg-is-installed-p (pkg)
  "(I) Return t if PKG is installed, nil otherwise."
  (file-exists-p (concat kiss/installed-db-dir pkg)))

(defun kiss--install-if-not-installed (pkgs-l)
  "Only install packages in PKGS-L if they are not already installed."
  (kiss/install (seq-remove 'kiss--pkg-is-installed-p pkgs-l)))

;; -> list         List installed packages
;; ===========================================================================

(defun kiss--get-installed-pkg-version (pkg)
  "(I) Return the version string for PKG, nil if PKG is not installed."
  (if (kiss--pkg-is-installed-p pkg)
      (let ((pdir (concat kiss/installed-db-dir pkg)))
        (replace-regexp-in-string
         "\n$" ""
         ;; TODO: see if there is a way to avoid
         ;; depending on f.el
         (f-read-text (concat pdir "/version"))))))

;; TODO: add docstring.
;; FIXME: comply w/ upstream kiss (this can take a list of packages.)
(defun kiss/list (&optional pkg-q)
  (if (eq nil pkg-q)
      (let ((pkgs (nthcdr 2 (directory-files kiss/installed-db-dir))))
        (cl-mapcar (lambda (p)
                     (let ((pdir (concat kiss/installed-db-dir p)))
                       (list p (kiss--get-installed-pkg-version p))))
                   pkgs))
    (list pkg-q (kiss--get-installed-pkg-version pkg-q))))

;; -> remove       Remove packages
;; ===========================================================================

;; TODO: need to account for symlinks w/ (file-symlink-p

;; FIXME: I think the `-f' flag is required to be added to rm(1).
(defun kiss--remove-file (file-path)
  "(I) Remove FILE-PATH as the appropriate user using rm(1), t if successful, nil otherwise."
  (if (file-exists-p file-path)
      (let ((owner (kiss--get-owner-name file-path))
            (rmcmd (concat "rm -- " file-path)))
        (eq 0
            (if (kiss--am-owner-p file-path)
                (shell-command rmcmd)
              (kiss--shell-command-as-user rmcmd owner))))))

(defun kiss--remove-directory (dir-path)
  "(I) Remove DIR-PATH as the appropriate user using rmdir(1), t if successful, nil otherwise."
  (if (and (file-directory-p dir-path)
           (not (file-symlink-p dir-path)))
      (let ((owner (kiss--get-owner-name dir-path))
            (rmcmd (concat "rmdir -- " dir-path)))
        (eq 0
            (if (kiss--am-owner-p dir-path)
                (shell-command rmcmd)
              (kiss--shell-command-as-user rmcmd owner))))))

(defun kiss--remove-files (file-path-lst)
  "(I) Remove all files and empty directories in FILE-PATH-LST."

  ;; FIXME: does *not* take all cases into account yet, do NOT
  ;; use

  ;; FIXME: need to investigate if this function needs my custom 'file-exists-p'
  ;; predicate.

  ;; This will return all of the /etc files/dirs.
  ;; (cl-remove-if-not
  ;;  (lambda (file-path) (string-match-p "/etc/" file-path))
  ;;  file-path-lst)

  ;; NOTE: I'm not entirely sure if it removing the files in the proper
  ;; order since it should be removing all of the files in a dir first,
  ;; then the dir itself, but when testing on 'xdo' it does not remove
  ;; the actual directory in `kiss/installed-db-dir'.

  ;; Make this local variable since we need to rm the symlinks
  ;; separately.
  (let ((symlink-queue '()))
    (cl-mapcar
     (lambda (file-path)
       (cond
        ;; NOTE: This is a cond expression, so it is important that
        ;; the directory check is *first*.

        ((and (file-directory-p file-path)
              (not (file-symlink-p file-path)))
         (kiss--remove-directory file-path))

        ((file-symlink-p file-path)
         (setq symlink-queue (cons file-path symlink-queue)))

        ((file-exists-p file-path)
         (kiss--remove-file file-path))

        (t nil)))
     file-path-lst)
    ;; Now to cleanup broken symlinks.
    (cl-mapcar
     (lambda (sym)
       (if (file-exists-p sym)
           (kiss--remove-file sym)))
     symlink-queue)))

;; (mapconcat #'identity
;;            (cl-mapcar
;;             (lambda (file-path)
;;               (cond
;;                ((and (file-directory-p file-path)
;;                      (not (file-symlink-p file-path)))
;;                 "dir")
;;                ((file-symlink-p file-path)
;;                 "sym")
;;                ((file-exists-p file-path)
;;                 "file")
;;                (t nil)))
;;             (kiss/manifest "xdo"))
;;            " ")

(defun kiss/remove (pkgs-l)
  (interactive)

  (cond ((listp pkgs-l)
         (cl-mapcar #'kiss/remove
                    (reverse
                     (kiss--get-pkg-order pkgs-l))))
        ((atom pkgs-l)
         (if (kiss--pkg-is-removable-p pkgs-l)
             (kiss--remove-files
              (kiss/manifest pkgs-l))))
        (t nil)))

;; -> search       Search for packages
;; ===========================================================================
(defun kiss/search (q)
  (interactive "sQuery: ")
  (seq-filter 'file-exists-p
              (mapcar (lambda (repo) (concat repo "/" q))
                      `(,@kiss/KISS_PATH
                        ,kiss/installed-db-dir))))

;; -> update       Update the repositories
;; ===========================================================================

;; FIXME: see if Emacs has something built-in to do most of this (vc.el).

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
  "(I) Return only the repos in `kiss/KISS_PATH' that are git repos."
  (seq-filter 'kiss--dir-is-git-repo-p kiss/KISS_PATH))

(defun kiss--update-git-repos ()
  "(I) Update all git repos in `kiss/KISS_PATH'."
  (let ((git-repos (delete-dups
                    (cl-mapcar 'kiss--get-git-dir-toplevel
                               (kiss--kiss-path-git-repos)))))
    (dolist (repo git-repos)
      (message (concat "kiss/update: Updating " repo))
      ;; FIXME: prevent this from stalling Emacs.
      (let ((repo-owner   (kiss--get-owner-name repo))
            (am-owner-p   (kiss--am-owner-p repo))
            (git-pull-cmd (concat "git -C " repo " pull" ))
            (git-subm-cmd (concat "git -C " repo " submodule update --remote --init -f")))
        (if am-owner-p
            (progn
              (shell-command git-pull-cmd)
              (shell-command git-subm-cmd))
          (progn
            (kiss--shell-command-as-user git-pull-cmd repo-owner)
            (kiss--shell-command-as-user git-subm-cmd repo-owner)))))))

;; TODO: Rethink how to integrate this.
;; (defun kiss--print-git-repo-MOTD ()
;;   "(I) Print out all of the MOTDs from each git repo."
;;   (let ((git-repos (delete-dups (cl-mapcar 'kiss--get-git-dir-toplevel (kiss--kiss-path-git-repos)))))
;;     (dolist (repo git-repos)
;;       (if (file-exists-p (concat repo "/MOTD"))
;;           (shell-command-to-string (concat "cat " repo "/MOTD"))))))

(defun kiss/update ()
  (interactive)
  (message "kiss/update")
  (kiss--update-git-repos))
;; (async-shell-command "KISS_PROMPT=0 kiss update"))

;; -> upgrade      Update the system
;; ===========================================================================

(defun kiss--pkg-remote-eq-pkg-local-p (pkg)
  "(I) Return t if the version of PKG is the same locally and from the remotes."
  (string=
   (kiss--sanitize-ver-str
    (f-read-text (concat (car (kiss/search pkg)) "/version")))
   (kiss--sanitize-ver-str
    (kiss--get-installed-pkg-version pkg))))

;; TODO: consider making this *not* internal.
(defun kiss--get-out-of-date-pkgs ()
  "(I) Return a list of PKGS that are out of date."
  (seq-remove 'kiss--pkg-remote-eq-pkg-local-p
              (cl-mapcar 'car (kiss/list))))

(defun kiss/upgrade ()
  (interactive)

  (let* ((oodpkgs (kiss--get-out-of-date-pkgs)))
    ;; FIXME: need to move this step to the actual building of the pkg
    ;; Need to check if kiss is being updated, since we need to make sure
    ;; to install it *first*, because it may have bug fixes/breaking changes.

    ;; NOTE: need think about how we will deal with a kiss update from
    ;; kiss.el's perspective.
    ;; (if (member "kiss" oodpkgs)
    ;;     (kiss/build-install "kiss"))

    ;; Now that we potentially have kiss updated, we need to now update
    ;; the packages in oodpkgs in the proper order.
    ;; This should prevent us from having to do any backtracking
    ;; as well, since we are always going to build the packages in the order
    ;; that they are required for.

    ;; (mapcar #'kiss/build-install
    ;;         (kiss--get-pkg-order oodpkgs))
    oodpkgs))

;; (let ((pkgs (kiss--get-out-of-date-pkgs)))
;; ;; If kiss is a member of the pkgs to be updated, make sure to update
;; ;; it first.
;; (if (member "kiss" pkgs)
;;     (progn
;;       (kiss/download "kiss")
;;       (kiss/build    "kiss")
;;       (kiss/install  "kiss")))

(defun kiss--pkgs-without-repo ()
  "(I) Return all packages that are installed that are not in a remote repo."
  (let ((pkgs-l (mapcar 'car (kiss/list))))
    (seq-filter
     (lambda (p)
       ;; Naturally, anything that was only *installed* will have 0 other
       ;; occurances.
       (eq 0
           (length
            ;; Remove the installed-db-dir *repo* from the list.
            (seq-remove
             (lambda (repo) (string-match-p kiss/installed-db-dir repo))
             (kiss/search p)))))
     pkgs-l)))

;; NOTE: it may be possible to make the future version that uses
;; the elisp tsort impl much faster than how it currently is,
;; since we can remove all verticies from the dependency graph
;; which aren't explicitly mentioned
;; TODO: look into this function when it comes time to optimize.
;; FIXME: add in a fast path for lists of 1 - can just return the
;; list
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
;;   (cl-remove-if-not
;;    (lambda (pair)
;;      (let ((ver (car (cdr pair))))
;;        (string-match "git" ver)))
;;    (kiss/list)))

;; -> version      Package manager version
;; SEE const.

;; Run "kiss [H|help-ext]" to see all actions

(provide 'kiss)

;;; kiss.el ends here.
