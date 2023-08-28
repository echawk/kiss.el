;;; kiss.el -- kiss package manager in ELisp

;; Author: Ethan Hawk <ethan.hawk@valpo.edu>
;; Maintainer: Ethan Hawk <ethan.hawk@valpo.edu>
;; URL: https://github.com/ehawkvu/kiss.el
;; Keywords: package-manager, tools
;; Package-Requires: ((f) (emacs "29.1"))
;; Version: 0.0.1

;; This file is under the MIT license.

;;; Commentary:

;; TODO: need to replace all slashes with something else, likely
;; a hyphen?
;; or could rename kiss/internal to kiss-i

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

;; Also, need to go through this code once I get a fully working POC done
;; and ruthlessly remove all duplicated code, since rn there are many
;; little redundancies spread about the current source.

;; https://www.emacswiki.org/emacs/UnitTesting

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'f)
  (require 'rx))

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
  (car (cl-remove-if-not 'executable-find
                         '("aria2c" "axel" "curl" "wget" "wget2")))
  "The utility for downloading http sources."
  :type 'string)

(defcustom kiss/KISS_CHK
  (car (cl-remove-if-not 'executable-find
                         '("openssl" "sha256sum" "sha256" "shasum" "digest")))
  "The utility for computing SHA256 checksums."
  :type 'string)

;; FIXME: Using 'su' is currently not supported by this package manager.
(defcustom kiss/KISS_SU
  (car (cl-remove-if-not 'executable-find
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

(defun kiss/internal--lst-to-str (lst)
  "(I) Convert LST to a string."
  (mapconcat (lambda (s) (format "%s" s)) lst " "))

(defun kiss/internal--sanitize-ver-str (str)
  "(I) Sanitize a version string STR to be correctly compared against others."
  (replace-regexp-in-string
   "\n$" ""
   (replace-regexp-in-string
    " " ""
    str)))

(defun kiss/internal--get-owner (file-path)
  "(I) Return the owner of FILE-PATH."
  (nth 2
       (string-split
        (shell-command-to-string (concat "ls -ld " file-path))
        " ")))

(defun kiss/internal--am-owner-p (file-path)
  "(I) Return t if the current LOGNAME owns the FILE-PATH, nil otherwise."
  (string=
   (getenv "LOGNAME")
   (kiss/internal--get-owner file-path)))
(defun kiss/internal--shell-command-as-user (command user)
  "(I) Run COMMAND as USER using `kiss/KISS_SU'."
  (shell-command (concat kiss/KISS_SU " -u " user " -- " command)))

(defun kiss/internal--decompress (file-path out-path)
  "(I) Decompress FILE-PATH to OUT-PATH based on the file name."
  (let ((cmd
         (cond
          ((string-match-p (rx ".tar" eol)             file-path) "cat ")
          ((string-match-p (rx (or ".tbz" ".bz2") eol) file-path) "bzip2 -dc ")
          ((string-match-p (rx ".lz" eol)              file-path) "lzip -dc")
          ((string-match-p (rx (or ".tgz" ".gz") eol)  file-path) "gzip -dc ")
          ((string-match-p (rx ".lzma" eol)            file-path) "lzma -dcT0 ")
          ((string-match-p (rx (or ".txz" ".xz") eol)  file-path) "xz -dcT0 ")
          ((string-match-p (rx ".zst" eol)             file-path) "zstd -dcT0 ")
          )))
    (if cmd
        (shell-command (concat cmd file-path " > " out-path)))))

(defun kiss/internal--b3 (file-path)
  "(I) Run b3sum on FILE-PATH."
  (car
   (string-split
    (replace-regexp-in-string
     "\n$" ""
     ;; NOTE: b3sum is the only supported BLAKE3 utility at this time.
     (shell-command-to-string (concat "b3sum -l 33 " file-path)))
    " ")))

(defun kiss/internal--sh256 (file-path)
  "(I) Run `kiss/KISS_CHK' with proper arguments on FILE-PATH."
  (let ((args
         (cond
          ((string-match-p "openssl"   kiss/KISS_CHK) " dgst -sha256 -r ")
          ((string-match-p "sha256sum" kiss/KISS_CHK) "")
          ((string-match-p "sha256"    kiss/KISS_CHK) " -r ")
          ((string-match-p "shasum"    kiss/KISS_CHK) " -a 256 ")
          ((string-match-p "digest"    kiss/KISS_CHK) " -a sha256 ")
          )))
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
    (kiss/internal--pkg-swap pkg path)))

;; pkg_manifest_replace() in kiss
(defun kiss/internal--pkg-manifest-replace (pkg old new)
  "(I) Replace the line matching OLD in the manifest of PKG with NEW."
  ;; Replace the matching line in the manifest w/ the desired
  ;; replacement.
  ;; TODO: test this to make sure it is correct.
  (let* ((manifest-f (concat kiss/installed-db-dir pkg "/manifest"))
         (temp-f     (kiss/internal--make-temp-file))
         (owner      (kiss/internal--get-owner manifest-f))
         (manifest-t (kiss/internal--manifest-to-string
                      (reverse
                       (cl-sort
                        (cl-mapcar (lambda (s) (if (string= s old) new s))
                                   (kiss/manifest pkg))
                        'string-lessp)))))

    (f-write-text manifest-t 'utf-8 temp-f)

    ;; TODO: see if this can be avoided.
    ;; Ensure the ownership is preserved.
    (kiss/internal--shell-command-as-user
     (concat "chown " owner ":" owner " " temp-f) owner)
    ;; Ensure the permissions are set correctly.
    (kiss/internal--shell-command-as-user
     (concat "chmod 644 " temp-f) owner)
    ;; Move it into place.
    (kiss/internal--shell-command-as-user
     (concat "mv -f " temp-f " " manifest-f)
     owner)))


;; FIXME: Either fix upstream Emacs/f.el or keep using this.
;; NOTE: DO NOT USE THIS ANYWHERE THAT ISN'T ABSOLUTELY NECESSARY.
(defun kiss/internal--file-exists-p (file-path)
  "(I) This function should NOT exist.
However, `file-exists-p' and `file-symlink-p' are fundamentally broken when it
comes to broken symlinks.  Hence the need for this function.
This function returns t if FILE-PATH exists and nil if it doesn't."
  (or
   (eq 0
       (shell-command (concat "test -f " file-path)))
   (eq 0
       (shell-command (concat "test -h " file-path)))))

(defun kiss/internal--single-quote-string (str)
  "(I) Add quotes around STR.  Useful when interacting with the cli."
  (concat "'" str "'"))

(defun kiss/internal--pkg-swap (pkg path)
  "(I) Swap the owner of PATH to PKG, modifying the relevant package manifests."
  (if (kiss/internal--pkg-is-installed-p pkg)
      ;; NOTE: The quotes surrounding the string are very important.
      ;; This is because this string is only interpreted as a command argument.
      ;; This means that the shell can mangle it if it is not properly
      ;; esacped.
      (let* ((alt      (string-replace "/" ">" path))
             (alt-path (concat kiss/choices-db-dir pkg alt))
             (path-own (kiss/owns path)))
        (if (kiss/internal--file-exists-p
             (kiss/internal--single-quote-string alt-path))
            (progn
              ;; If the file is owned by a package in the database.
              (if path-own
                  (progn
                    (message (concat "Swapping " path
                                     " from " path-own
                                     " to " pkg))
                    ;; Save the path into kiss/choices-db-dir
                    (kiss/internal--shell-command-as-user
                     (concat "cp -Pf " path " "
                             (kiss/internal--single-quote-string
                              (concat kiss/choices-db-dir path-own alt)))
                     (kiss/internal--get-owner path))

                    ;; Update the manifest file to reflect the new version.
                    (kiss/internal--pkg-manifest-replace
                     path-own path (concat kiss/choices-db-dir path-own alt))))
              ;; Move over our new desired alternative to the real file.
              (kiss/internal--shell-command-as-user
               (concat "mv -f " (kiss/internal--single-quote-string alt-path)
                       " " path)
               (kiss/internal--get-owner path))
              (kiss/internal--pkg-manifest-replace pkg alt-path path))))))



(defun kiss/internal--manifest-to-string (pkg-manifest)
  "(I) Convert our internal representation of PKG-MANIFEST into a string."
  (concat (mapconcat #'identity pkg-manifest "\n") "\n"))


(defun kiss/manifest (pkg)
  "Return a list of all files owned by PKG."
  (cl-remove-if
   (lambda (s) (string= "" s))
   (string-split
    (f-read-text
     (concat kiss/installed-db-dir pkg "/manifest"))
    "\n")))


(defun kiss/internal--get-installed-manifest-files ()
  "(I) Return a list of all of the installed manifest files."
  (mapcar
   '(lambda (pkg) (concat kiss/installed-db-dir pkg "/manifest"))
   (mapcar 'car (kiss/list))))

(defun kiss/owns (file-path)
  ;; TODO: See if this can be made a little less ugly.
  (let* ((cmd (concat "grep " (rx bol (literal file-path) eol) " "
                      (kiss/internal--lst-to-str
                       (kiss/internal--get-installed-manifest-files))))
         (cmd-out (shell-command-to-string cmd)))
    (if (not (string= cmd-out ""))
        (car
         (string-split
          (replace-regexp-in-string kiss/installed-db-dir "" cmd-out) "/")))))


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
        (kiss/internal--lst-to-str
         (kiss/internal--get-installed-manifest-files))
        " /dev/null"
        ))))
    "\n"))
  )

(defun kiss/internal--get-pkg-dependencies (pkg)
  "(I) Get the dependencies of PKG as a list, nil if PKG has no dependencies."
  (let ((depends-file (concat (car (kiss/search pkg)) "/depends")))
    (if (file-exists-p depends-file)
        (cl-remove-if
         (lambda (i) (string= i ""))
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
;; FIXME: add in the ability to support passing in a list of
;; packages to this function, since it would *greatly* reduce
;; the amount of time spent checking for dependencies.
;; Also it should be somewhat easy to implement + make the code
;; that depends on this code simpler. win-win
(defun kiss/internal--get-pkg-dependency-graph (pkg-lst)
  "(I) Generate a graph of the dependencies for PKG-LST."
  (let* ((seen '())
         (queue
          (cond
           ((atom pkg-lst) `(,pkg-lst))
           ((listp pkg-lst) pkg-lst)
           (t nil)))
         (res '()))
    ;; While there are still pkgs in the queue to look at.
    (while queue
      ;; Only execute this block if we haven't already seen this pkg.
      (when (not (member (car queue) seen))
        (let* ((dep (car queue))
               (dep-deps (kiss/internal--get-pkg-dependencies dep)))
          (let ((item `(,dep ,dep-deps)))
            ;; Saves ourselves the headache of removing duplicates early.
            (if (not (member item res))
                ;; Update our result to contain the dep and it's depends.
                (setq res (cons item res))))
          ;; Make sure we only append to the cdr of the queue.
          (setq queue (append dep-deps (cdr queue))))))
    res))

(defun kiss/internal--dependency-graph-to-tsort (pkg-depgraph)
  "(I) Convert a PKG-DEPGRAPH graph to a tsort(1) compatible one."
  (let ((print-pair
         (lambda (pair)
           (if (cadr pair)
               (mapconcat (lambda (p) (message "%s %s" p (car pair)))
                          (cadr pair) "\n")))))
    (let ((pair-str
           (funcall print-pair pkg-depgraph)))
      (if pair-str pair-str ""))))


;; NOTE: will likely be removed once a tsort implementation in elisp is written.
(defun kiss/internal--get-pkg-tsort-graph (pkg-lst)
  "(I) Get a tsort(1) compatible representation of the dependencies for PKG-LST."
  (mapconcat #'kiss/internal--dependency-graph-to-tsort
             (kiss/internal--get-pkg-dependency-graph pkg-lst) "\n"))


(defun kiss/internal--get-pkg-dependency-order (pkg-lst)
  "(I) Return the proper build order of the dependencies for each pkg in PKG-LST."
  (cl-remove-if (lambda (s) (string= s ""))
                (string-split
                 (shell-command-to-string
                  (concat "printf '"
                          (kiss/internal--get-pkg-tsort-graph pkg-lst)
                          "'"
                          " | "
                          " tsort "))
                 "\n")))
;; (kiss/internal--get-pkg-dependency-order '("gcc" "clang"))
;; (kiss/internal--get-pkg-dependency-order '("llvm" "rust"))

;; As noted above, here is the code that I was testing out to try and
;; figure out a different method.

;; ;; Save our resulting depends in res
;; (let ((res '()))
;;   (mapcar
;;    ;; Here we take a pkg, and go through all of its dependency
;;    ;; graphs and add them to res only if they are not alreay present in
;;    ;; res.
;;    (lambda (pkg)
;;      (let ((deps (kiss/internal--get-pkg-dependency-graph pkg)))
;;        (mapcar (lambda (dep)
;;                  (if (not (member dep res))
;;                      (setq res (cons dep res))))
;;                deps)))
;;    ;; Our packages list
;;    '("llvm"))
;;   res)

;; TODO: make the output list prettier (ie, should be a list of pkgs,
;; not depends files)
(defun kiss/internal--get-pkg-make-dependents (pkg)
  "(I) Return a list of installed packages that have a make dependency on PKG, nil if there are no dependents."
  (mapcar
   (lambda (dep-file)
     (replace-regexp-in-string
      "/depends" ""
      (replace-regexp-in-string
       kiss/installed-db-dir ""
       dep-file)))
   (cl-remove-if-not
    (lambda (depfile)
      (string-match (rx bol (literal pkg) (one-or-more " ") (literal "make"))
                    (f-read-text depfile)))
    (cl-remove-if-not
     #'file-exists-p
     (mapcar (lambda (p) (concat kiss/installed-db-dir p "/depends"))
             (nthcdr 2 (directory-files kiss/installed-db-dir)))))))
;; (kiss/internal--get-pkg-make-dependents "ant")

(defun kiss/internal--get-pkg-make-orphans ()
  "(I) Return a list of installed packages that were only required as a make dependency."
  ;; NOTE: This function is pretty slow at the moment.
  (cl-remove-if-not
   (lambda (pkg)
     (and (eq (kiss/internal--get-pkg-hard-dependents pkg) nil)
          (not
           (eq (kiss/internal--get-pkg-make-dependents pkg) nil))))
   (mapcar #'car (kiss/list))))

;; TODO: make the output list prettier (ie, should be a list of pkgs,
;; not depends files)
(defun kiss/internal--get-pkg-hard-dependents (pkg)
  "(I) Return a list of installed packages that have a runtime dependency on PKG, nil if there are no dependents."
  (mapcar
   (lambda (dep-file)
     (replace-regexp-in-string
      "/depends" ""
      (replace-regexp-in-string
       kiss/installed-db-dir ""
       dep-file)))
   (cl-remove-if-not
    (lambda (depfile)
      (string-match (rx bol (literal pkg) (zero-or-more " ") eol)
                    (f-read-text depfile)))
    (cl-remove-if-not
     #'file-exists-p
     (mapcar (lambda (p) (concat kiss/installed-db-dir p "/depends"))
             (nthcdr 2 (directory-files kiss/installed-db-dir)))))))

;; (kiss/internal--get-pkg-hard-dependents "mpfr")


(defun kiss/internal--get-pkg-missing-dependencies (pkg)
  "(I) Return a list of dependencies that are missing for PKG, nil otherwise."
  (cl-remove-if
   #'kiss/internal--pkg-is-installed-p
   (delete-dups
    (flatten-list
     (mapcar #'cadr
             (kiss/internal--get-pkg-dependency-graph pkg))))))

;; (kiss/internal--get-pkg-missing-dependencies "gimp")
;; (kiss/internal--get-pkg-missing-dependencies "gcc")

(defun kiss/internal--get-pkg-orphan-alternatives (pkg)
  "(I) Return a list of orphaned alternatives that would result from removing PKG."
  (let ((orphaned-alternatives
         (cl-remove-if-not
          (lambda (pair) (seq-contains-p pair pkg))
          (kiss/preferred))))
    ;; Return the files associated with PKG.
    (if orphaned-alternatives
        (mapcar #'cadr orphaned-alternatives))))


(defun kiss/internal--pkg-is-removable-p (pkg)
  "(I) Return t if PKG is removable, nil otherwise."

  ;; A package is removable when the following conditions are met:
  ;; * the pkg is installed on the current system
  ;; * nothing on the system hard depends on the package
  ;; * the package does not leave any orphaned alternatives
  (and
   (kiss/internal--pkg-is-installed-p pkg)
   (eq (kiss/internal--get-pkg-hard-dependents pkg) nil)
   (eq (kiss/internal--get-pkg-orphan-alternatives pkg) nil)))

;; -> build        Build packages
;; ===========================================================================


(defun kiss/internal--get-manifest-for-dir (dir)
  "(I) Return a kiss compatible manifest for DIR."
  (let ((files-and-dirs
         ;; find cmd
         (cl-mapcar
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


(defun kiss/internal--get-pkg-version (pkg)
  "(I) Get the version for PKG using the car of `kiss/search'."
  (let ((ks (kiss/search pkg)))
    (if ks
        (let ((pdir (car ks)))
          (replace-regexp-in-string
           "\n$" ""
           ;; TODO: see if there is a way to avoid
           ;; depending on f.el
           (f-read-text (concat pdir "/version")))))))

(defun kiss/internal--get-pkg-bin-name (pkg version)
  "(I) Return the proper name for the binary for PKG at VERSION."
  (concat pkg "@"
          (replace-regexp-in-string " " "-" version)
          ".tar." kiss/KISS_COMPRESS ))

(defun kiss/internal--get-compression-command ()
  "(I) Return the proper command for the compression specified by `kiss/KISS_COMPRESS'."
  (cond ((string= "bz2" kiss/KISS_COMPRESS) "bzip2 -z")
        ((string= "gz" kiss/KISS_COMPRESS) "gzip -6")
        ((string= "lzma" kiss/KISS_COMPRESS) "lzma -z")
        ((string= "lz" kiss/KISS_COMPRESS) "lzip -z")
        ((string= "xz" kiss/KISS_COMPRESS) "xz -z")
        ((string= "zstd" kiss/KISS_COMPRESS) "zstd -z")
        (t nil)))

(defun kiss/internal--get-pkg-cached-bin (pkg)
  "(I) Return the path of the binary for PKG, nil if PKG has no binary in the cache."
  (let ((ver (kiss/internal--get-pkg-version pkg)))
    (if ver
        (let ((bin (concat kiss/KISS_BINDIR
                           (kiss/internal--get-pkg-bin-name pkg ver))))
          (if (file-exists-p bin) bin)))))


(defun kiss/internal--get-tmp-destdir ()
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


(defun kiss/internal--make-tarball-of-dir (dir file-path)
  "(I) Make a compressed tarball of DIR saved into FILE-PATH."
  ;; FIXME: need to remove the reliance on tar's -C flag, since it
  ;; is noted in upstream kiss as being a source of portability issues.
  (eq 0
      (shell-command
       (concat "tar -cf - -C " dir " . | "
               (kiss/internal--get-compression-command)
               " > " file-path))))

;; FIXME: rm missing-deps check here and move that up to the caller.
;; FIXME: need to return a signifier on whether the build was successful or
;; not, t if yes, and nil if a failure.
(defun kiss/internal--build-pkg (pkg)
  "(I) Build PKG, return t if PKG was built successfully, nil otherwise."
  (let ((missing-deps (kiss/internal--get-pkg-missing-dependencies pkg))
        (pkg-ver (replace-regexp-in-string
                  " " "-"
                  (kiss/internal--get-pkg-version pkg))))
    ;; Install/build missing dependencies
    (if missing-deps
        (mapcar #'kiss/internal--try-install-build missing-deps))

    ;; Recheck to make sure that we aren't missing any deps.
    (setq missing-deps (kiss/internal--get-pkg-missing-dependencies pkg))

    (if (not missing-deps)
        (let* ((build-script (concat (car (kiss/search pkg)) "/build"))
               (proc-dir     (kiss/internal--get-tmp-destdir))
               (build-dir    (concat proc-dir "/build/" pkg "/"))
               (install-dir  (concat proc-dir "/pkg/" pkg))
               (k-el-build   (concat proc-dir "/build-" pkg "-kiss-el")))

          ;; Extract pkg's sources to the build directory.
          (kiss/internal--extract-pkg-sources pkg build-dir)
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
                          (kiss/internal--get-manifest-for-dir install-dir))
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
                          (mapcar #'kiss/internal--b3 etc-files)
                          "\n")
                         'utf-8 (concat pkg-install-db pkg "/etcsums")))

                    ;; Next, create the manifest
                    (f-write-text
                     (kiss/internal--manifest-to-string manifest-lst)
                     'utf-8 (concat pkg-install-db pkg "/manifest"))))

                ;; Finally create the tarball
                (message (concat "Creating tarball for " pkg))
                (kiss/internal--make-tarball-of-dir
                 install-dir
                 (concat kiss/KISS_BINDIR
                         (kiss/internal--get-pkg-bin-name pkg pkg-ver)))

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

;; (kiss/internal--build-pkg "xdo")

;; FIXME: add in checks to the appropriate places.
(defun kiss/internal--build-install (pkg)
  "(I) Attempt to build and install PKG, nil if unsuccessful."
  (if (kiss/internal--build-pkg)
      (kiss/install pkg)))

(defun kiss/internal--try-install-build (pkg)
  "(I) Attempt to install a binary of PKG, else build and install PKG."
  (if (kiss/internal--get-pkg-cached-bin pkg)
      (kiss/install pkg)
    (kiss/internal--build-install pkg)))

(defun kiss/build (pkgs-l)
  (interactive)
  (cond ((listp pkgs-l)
         (progn
           (mapcar #'kiss/internal--build-pkg
                   (kiss/internal--get-pkg-order pkgs-l))))
        ((atom pkgs-l)
         (progn
           (kiss/download pkgs-l)
           (kiss/internal--build-pkg pkgs-l)))))

;; (async-shell-command
;;  (concat "kiss build " (kiss/internal--lst-to-str pkgs-l))))

;; -> checksum     Generate checksums
;; ===========================================================================

;; Initial working impl of kiss/checksum below; need to refactor some of
;; the functionality since kiss/download has similar needs.

(defun kiss/internal--get-pkg-repo-checksums (pkg)
  "(I) Return the list of checksums for PKG from a repo or nil if checksums don't exist."
  (let ((checksums-file (concat (car (kiss/search pkg)) "/checksums")))
    (if (file-exists-p checksums-file)
        (cl-remove-if
         (lambda (chk) (string= chk ""))
         (string-split
          (f-read-text checksums-file) "\n")))))

(defmacro tps-env (pkg tps expr)
  ;; "(I) Macro to aide in parsing TPS, and using the values in EXPR."
  ;; Extract out each of the variables.
  `(let* ((pkg-source-cache-dir (concat kiss/KISS_SRCDIR ,pkg "/"))
          (type                 (car  ,tps))
          (uri                  (car  (cdr ,tps)))
          (sub-dir              (cadr (cdr ,tps)))
          (dest-dir             (concat pkg-source-cache-dir sub-dir)))
     ,expr))

(defun kiss/internal--get-pkg-local-checksums (pkg)
  "(I) Return the list of checksums for PKG from the files on disk, or nil."
  (cl-mapcar
   #'kiss/internal--b3
   (cl-mapcar
    #'cdr
    (cl-remove-if
     ;; Filter out 'git' sources.
     ;; TODO: implement a lookup for future mercurial sources.
     (lambda (tps-cache)
       (string= "git" (car (car tps-cache))))
     (-zip
      (kiss/internal--get-type-pkg-sources pkg)
      (kiss/internal--get-pkg-sources-cache-path pkg))))))

(defun kiss/internal--pkg-verify-local-checksums (pkg)
  "(I) Return t if local checksums match up with the repo checksums for PKG, nil otherwise."
  (eq nil
      (cl-remove-if
       (lambda (pair) (string= (car pair) (cdr pair)))
       (-zip
        (kiss/internal--get-pkg-repo-checksums  pkg)
        (kiss/internal--get-pkg-local-checksums pkg)))))

(defun kiss/checksum (pkgs-l)
  (cond
   ((listp pkgs-l)
    (cl-mapcar #'kiss/checksum pkgs-l))
   ((atom pkgs-l)
    (let* ((pkg-path (car (kiss/search pkgs-l)))
           (chk-path (concat pkg-path "/checksums"))
           (chk-sums (mapconcat
                      #'identity
                      (kiss/internal--get-pkg-local-checksums pkgs-l) "\n")))
      (if (and (kiss/internal--am-owner-p chk-path)
               (not (string= chk-sums "")))
          (f-write-text
           chk-sums
           'utf-8 chk-path))))))


;; -> download     Download sources
;; ===========================================================================

(defun kiss/download (pkgs-l)
  (interactive "sQuery: ")
  (cond ((listp pkgs-l)
         (cl-mapcar #'kiss/download pkgs-l))
        ((atom pkgs-l)
         (kiss/internal--download-pkg-sources pkgs-l))
        (t nil)))
;; (async-shell-command (concat "kiss download " pkgs-l)))

;; (kiss/download '("kiss" "gdb"))

(defun kiss/internal--get-pkg-sources (pkg)
  "(I) Return a list of sources for PKG, or nil if PKG has no sources file."
  (let* ((pkg-repo    (car (kiss/search pkg)))
         (pkg-sources (concat pkg-repo "/sources")))
    (if (file-exists-p pkg-sources)
        (progn
          ;; Remove any non source lines.
          (cl-remove-if
           (lambda (lst) (string= (car lst) ""))
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

(defun kiss/internal--get-pkg-sources-type (pkg-source)
  "(I) Return the type of PKG-SOURCE."
  (let ((pkg-url (car pkg-source)))
    ;; TODO: need to ensure that this is the same expected behavior as
    ;; upstream.
    (cond
     ((string-match-p (rx bol "git+") pkg-url) "git")
     ((string-match-p (rx "://") pkg-url) "remote")
     (t "local"))
    ))

;; FIXME: make a macro for parsing out the clean url, the dest folder & commit
;; since that information would also be useful for other vc systems like
;; fossil and hg.
(defun kiss/internal--download-git-source (url dest-dir)
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

(defun kiss/internal--make-temp-file ()
  "(I) Make a temporary file using the `mktemp' utility."
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "mktemp")))

(defun kiss/internal--get-download-utility-arguments ()
  "(I) Get the proper arguments for the `kiss/KISS_GET' utility."
  (cond
   ((string= kiss/KISS_GET "aria2c") " -d / -o ")
   ((string= kiss/KISS_GET "axel")   " -o ")
   ((string= kiss/KISS_GET "curl")   " -fLo ")
   ((or (string= kiss/KISS_GET "wget") (string= kiss/KISS_GET "wget2")) " -O ")))

(defun kiss/internal--download-remote-source (url dest-dir)
  "(I) Download URL to DEST-DIR using `kiss/KISS_GET'."
  ;; TODO: check and make sure this is the right way to create this file name.
  (let* ((file-name (car (reverse (string-split url "/"))))
         (dest-path (concat dest-dir "/" file-name)))
    ;; Only download if the file doesn't already exist.
    (if (not (file-exists-p dest-path))
        (shell-command
         (concat kiss/KISS_GET " "
                 url
                 (kiss/internal--get-download-utility-arguments)
                 dest-path)))))

(defun kiss/internal--download-local-source (uri dest-dir)
  "(I) Copy URI to DEST-DIR using cp(1)."
  (let ((file-name (car (reverse (string-split uri "/")))))
    ;; FIXME: double check to make sure this is correct - I have a hunch
    ;; that it's not.
    (if (not (file-exists-p uri))
        (shell-command
         (concat "cp " file-path
                 " " (concat dest-dir file-name))))))

(defun kiss/internal--get-pkg-sources-cache-path (pkg)
  "(I) Return the cache path in `kiss/KISS_SRCDIR' for each of PKG's sources."
  (let* ((pkg-source-cache-dir (concat kiss/KISS_SRCDIR pkg "/"))
         (type-pkg-sources (kiss/internal--get-type-pkg-sources pkg)))
    (cl-mapcar
     (lambda (tps)
       (tps-env pkg tps
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
                      (concat (car (kiss/search pkg)) "/" uri)))
                   ))))
     type-pkg-sources)
    ))


(defun kiss/internal--pkg-sources-available-p (pkg)
  "(I) Return t if all of the sources for PKG are available locally, nil otherwise."
  (not (member nil (cl-mapcar
                    #'file-exists-p
                    (kiss/internal--get-pkg-sources-cache-path pkg)))))


(defun kiss/internal--get-type-pkg-sources (pkg)
  "(I) Return a list containing the source type, followed by the source for PKG."
  (let ((pkg-sources (kiss/internal--get-pkg-sources pkg)))
    (-zip
     (mapcar 'kiss/internal--get-pkg-sources-type pkg-sources)
     pkg-sources)))

(defun kiss/internal--download-pkg-sources (pkg)
  "(I) Download the sources for PKG into `kiss/KISS_SRCDIR'."
  (let* ((pkg-source-cache-dir (concat kiss/KISS_SRCDIR pkg "/"))
         (type-pkg-sources (kiss/internal--get-type-pkg-sources pkg)))

    (cl-mapcar
     (lambda (tps)
       (tps-env pkg tps
                (progn
                  ;; Make the cache directory if it doesn't already exist.
                  (if (not (file-exists-p dest-dir))
                      (make-directory dest-dir))

                  ;; Switch based on the type of source that it is.
                  (cond
                   ((string= type "remote")
                    (kiss/internal--download-remote-source uri dest-dir))
                   ((string= type "git")
                    (kiss/internal--download-git-source uri dest-dir))
                   ((string= type "local")
                    (if (string-match (rx bol "/") uri)
                        ;; Absolute path.
                        (kiss/internal--download-local-source uri dest-dir))
                    ;; Relative path.
                    (kiss/internal--download-local-source
                     (concat (car (kiss/search pkg)) "/" uri) dest-dir))
                   ))))
     type-pkg-sources)
    ))

;; pkg_source_tar()
(defun kiss/internal--extract-tarball (tarball dir)
  "(I) Extract TARBALL to DIR.  Emulates GNU Tar's --strip-components=1."
  (let ((decomp-tarball (kiss/internal--make-temp-file)))
    ;; Decompress the tarball.
    (kiss/internal--decompress tarball decomp-tarball)
    ;; Extract the tarball.
    (shell-command (concat "tar xf " decomp-tarball " -C " dir))
    ;; Get all of the top level directories from the tarball.
    (cl-mapcar
     (lambda (tld)
       (let* ((temp-f (kiss/internal--make-temp-file))
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
         (kiss/internal--shell-command-as-user
          (concat "rm -- " temp-f) (kiss/internal--get-owner temp-f))
         ;; Also rm the temp directory.
         (kiss/internal--shell-command-as-user
          (concat "rm -rf -- " temp-d) (kiss/internal--get-owner temp-d))
         ))

     ;; Get a list of all of the top level directories in the tarball.
     (cl-remove-if
      (lambda (s) (string= s ""))
      (string-split
       (shell-command-to-string
        (concat "tar tf " tarball " | sort -ut / -k1,1"))
       "\n")))

    ;; Iterate over all of the directories that we just
    ;; extracted, each directories contents are moved up a level.

    ;; Remove our decompressed tarball now that we are done with it.
    (kiss/internal--shell-command-as-user
     (concat "rm -f -- " decomp-tarball)
     (kiss/internal--get-owner decomp-tarball))))

;; pkg_extract() in kiss
(defun kiss/internal--extract-pkg-sources (pkg dir)
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

        (t (if (kiss/internal--str-tarball-p cache)
               (kiss/internal--extract-tarball cache outdir)
             (shell-command (concat "cp -PRf " cache " " outdir)))))))
   ;; Get the type of each cached pkg source w/ the source itself.
   (-zip
    (cl-mapcar (lambda (tps) (cons (car tps) (nth 2 tps))) (kiss/internal--get-type-pkg-sources pkg))
    (kiss/internal--get-pkg-sources-cache-path pkg))))


(defun kiss/internal--str-tarball-p (str)
  "(I) Predicate to determine if STR matches the regex for a tarball."
  (string-match-p
   (rx
    (or (: "t" any "z")
        (: "t" any "z")
        (: "tar")
        (: "tar." any any)
        (: "tar." any any any)
        (: "tar." any any any any)) eol) str))

;; (kiss/internal--str-tarball-p "ball.tar.xz")

;; -> install      Install packages
;; ===========================================================================
(defun kiss/install (pkgs-l)
  (interactive)

  ;; FIXME: support installing via a mapcar
  ;; Need to check what our arguments are.

  ;; If a path to a tarball, install said tarball.
  ;; Otherwise, look it up.
  (let* ((tarball
          (cond ((file-exists-p pkgs-l) pkgs-l)
                (t (kiss/internal--get-pkg-cached-bin pkgs-l)))))

    ;; Make sure a tarball actually exists for the pkg.
    (if (not tarball)
        (error (concat "kiss/install: tarball does not exist for " pkgs-l)))

    ;; First need to decompress the tarball to a temp directory
    (let ((proc-dir       (kiss/internal--get-tmp-destdir))
          (decomp-tarball (kiss/internal--make-temp-file)))

      ;; (let ((tarball "/tmp/z/xdo@0.5.7-1.tar.gz")
      ;;       (decomp-tarball "/tmp/z/xdo@0.5.7-1.tar")
      ;;       (pkgs-l "xdo")
      ;;       (proc-dir "/tmp/z/proc-dir"))

      (make-directory proc-dir t)

      ;; NOTE: I would like to switch to using my already written
      ;; extract-tarball function instead of having to spell it out
      ;; manually here, however when attempting to use it I end
      ;; up getting odd errors likely due to the extra processing that
      ;; occurs.
      ;; (kiss/internal--extract-tarball tarball proc-dir)

      (kiss/internal--decompress tarball decomp-tarball)
      (shell-command
       (concat "tar xf " decomp-tarball " -C " proc-dir))

      ;; assume that the existence of the manifest file is all that
      ;; makes a valid KISS pkg.

      ;; FIXME: need to make this far far far more bullet proof than
      ;; how it is at present, since this will not work for installing
      ;; tarball's directly.
      (if (not (file-exists-p
                (concat
                 proc-dir "/var/db/kiss/installed/" pkgs-l "/manifest" )))
          ;; FIXME: make this an error? also need to cleanup by this point
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
                    (concat proc-dir fp))
                   (kiss/internal--file-exists-p
                    (concat proc-dir fp))))

             ;; Yes this code is copied straight from `kiss/manifest',
             ;; I'm hoping to factor it out.
             (cl-remove-if
              (lambda (s) (string= "" s))
              (string-split
               (f-read-text
                (concat proc-dir "/var/db/kiss/installed/" pkgs-l "/manifest"))
               "\n")))))

          (message "kiss/install: manifest is valid!")
        )

      ;; )
      )
    )


  ;; (not (cl-remove-if #'identity '(nil nil nil)))
  ;; (not (cl-remove-if #'identity '(t t t)))

  ;; pkg_conflicts()

  ;; If the pkg is already installed (and this is an upgrade)
  ;; make a backup fo the manifest and etcsum files

  ;; generate a list of files which exist in the current (installed) manifest
  ;; that do not exist in the new (to be installed) manifest.

  ;; Reverse the manifest file so that we start shallow, and go deeper
  ;; as we iterate through each item. This is needed so that directories
  ;; are created in the proper order


  ;; FIXME: see pkg_install() in kiss - will need a few more
  ;; helper functions for the final install.

  (async-shell-command
   (concat "kiss install " (kiss/internal--lst-to-str pkgs-l))))
;; (kiss/install '("R"))

(defun kiss/internal--pkg-is-installed-p (pkg)
  "(I) Return t if PKG is installed, nil otherwise."
  (file-exists-p (concat kiss/installed-db-dir pkg)))

(defun kiss/install-if-not-installed (pkgs-l)
  "Only install packages in PKGS-L if they are not already installed."
  (kiss/install (cl-remove-if 'kiss/internal--pkg-is-installed-p pkgs-l)))

;; (kiss/install-if-not-installed '("emacs-git" "glibc" "R" "asdf"))

;; -> list         List installed packages
;; ===========================================================================

(defun kiss/internal--get-installed-pkg-version (pkg)
  "(I) Return the version string for PKG, nil if PKG is not installed."
  (if (kiss/internal--pkg-is-installed-p pkg)
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
                       (list p (kiss/internal--get-installed-pkg-version p))))
                   pkgs))
    (list pkg-q (kiss/internal--get-installed-pkg-version pkg-q))))

;; -> remove       Remove packages
;; ===========================================================================

;; TODO: need to account for symlinks w/ (file-symlink-p

;; FIXME: I think the `-f' flag is required to be added to rm(1).
(defun kiss/internal--remove-file (file-path)
  "(I) Remove FILE-PATH as the appropriate user using rm(1), t if successful, nil otherwise."
  (if (file-exists-p file-path)
      (let ((owner (kiss/internal--get-owner file-path))
            (rmcmd (concat "rm -- " file-path)))
        (eq 0
            (if (kiss/internal--am-owner-p file-path)
                (shell-command rmcmd)
              (kiss/internal--shell-command-as-user rmcmd owner))))))

(defun kiss/internal--remove-directory (dir-path)
  "(I) Remove DIR-PATH as the appropriate user using rmdir(1), t if successful, nil otherwise."
  (if (and (file-directory-p dir-path)
           (not (file-symlink-p dir-path)))
      (let ((owner (kiss/internal--get-owner dir-path))
            (rmcmd (concat "rmdir -- " dir-path)))
        (eq 0
            (if (kiss/internal--am-owner-p dir-path)
                (shell-command rmcmd)
              (kiss/internal--shell-command-as-user rmcmd owner))))))

(defun kiss/internal--remove-files (file-path-lst)
  "(I) Remove all files and empty directories in FILE-PATH-LST."

  ;; FIXME: does *not* take all cases into account yet, do NOT
  ;; use

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
         (kiss/internal--remove-directory file-path))

        ((file-symlink-p file-path)
         (setq symlink-queue (cons file-path symlink-queue)))

        ((file-exists-p file-path)
         (kiss/internal--remove-file file-path))

        (t nil)))
     file-path-lst)
    ;; Now to cleanup broken symlinks.
    (cl-mapcar
     (lambda (sym)
       (if (file-exists-p sym)
           (kiss/internal--remove-file sym)))
     symlink-queue)
    ))

;; (let ((pkg "xdo"))
;;   (if (kiss/internal--pkg-is-removable-p pkg)
;;       (kiss/internal--remove-files
;;        (kiss/manifest pkg))))


(defun kiss/remove (pkgs-l)
  (interactive)

  (cond ((listp pkgs-l)
         (cl-mapcar #'kiss/remove
                    (reverse
                     (kiss/internal--get-pkg-order pkgs-l))))
        ((atom pkgs-l)
         (if (kiss/internal--pkg-is-removable-p pkgs-l)
             (kiss/internal--remove-files
              (kiss/manifest pkgs-l))))
        (t nil)))


;; -> search       Search for packages
;; ===========================================================================
(defun kiss/search (q)
  (interactive "sQuery: ")
  (cl-remove-if-not 'file-exists-p
                    (mapcar (lambda (repo) (concat repo "/" q))
                            `(,@kiss/KISS_PATH
                              ,kiss/installed-db-dir))))

;; -> update       Update the repositories
;; ===========================================================================

;; FIXME: see if Emacs has something built-in to do most of this (vc.el).

(defun kiss/internal--dir-is-git-repo-p (dir)
  "(I) Return t if DIR is a git repo, nil otherwise."
  (eq 0 (shell-command (concat "git -C " dir " rev-parse 'HEAD@{upstream}'"))))

(defun kiss/internal--git-subm-superproject-dir (dir)
  "(I) Return the directory for a git submodule's (DIR) superproject."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (concat "git -C " dir " rev-parse --show-superproject-working-tree"))))

(defun kiss/internal--dir-is-git-subm-p (dir)
  "(I) Return t if DIR is a git submodule, nil otherwise."
  (not (string= "" (kiss/internal--git-subm-superproject-dir dir))))

;; (kiss/internal--dir-is-git-subm-p "/home/ethan/git/uni/fbsd-repo/repo-xorg")
;; (kiss/internal--dir-is-git-subm-p "/home/ethan/git/uni/fbsd-repo/core")

(defun kiss/internal--get-git-dir-toplevel (dir)
  "(I) Return the toplevel directory for a git repo, of which DIR is a subdir."
  (let* ((dir-is-subm-p (kiss/internal--dir-is-git-subm-p dir))
         (repo (if dir-is-subm-p
                   (kiss/internal--git-subm-superproject-dir dir)
                 dir)))
    (replace-regexp-in-string
     "\n$" ""
     (shell-command-to-string
      (concat "git -C " repo " rev-parse --show-toplevel")))))

(defun kiss/internal--kiss-path-git-repos ()
  "(I) Return only the repos in `kiss/KISS_PATH' that are git repos."
  (cl-remove-if-not 'kiss/internal--dir-is-git-repo-p kiss/KISS_PATH))

(defun kiss/internal--update-git-repos ()
  "(I) Update all git repos in `kiss/KISS_PATH'."
  (let ((git-repos (delete-dups
                    (cl-mapcar 'kiss/internal--get-git-dir-toplevel
                               (kiss/internal--kiss-path-git-repos)))))
    (dolist (repo git-repos)
      (message (concat "kiss/update: Updating " repo))
      ;; FIXME: prevent this from stalling Emacs.
      (let ((repo-owner   (kiss/internal--get-owner repo))
            (am-owner-p   (kiss/internal--am-owner-p repo))
            (git-pull-cmd (concat "git -C " repo " pull" ))
            (git-subm-cmd (concat "git -C " repo " submodule update --remote --init -f")))
        (if am-owner-p
            (progn
              (shell-command git-pull-cmd)
              (shell-command git-subm-cmd))
          (progn
            (kiss/internal--shell-command-as-user git-pull-cmd repo-owner)
            (kiss/internal--shell-command-as-user git-subm-cmd repo-owner)))))))

;; TODO: Rethink how to integrate this.
;; (defun kiss/internal--print-git-repo-MOTD ()
;;   "(I) Print out all of the MOTDs from each git repo."
;;   (let ((git-repos (delete-dups (cl-mapcar 'kiss/internal--get-git-dir-toplevel (kiss/internal--kiss-path-git-repos)))))
;;     (dolist (repo git-repos)
;;       (if (file-exists-p (concat repo "/MOTD"))
;;           (shell-command-to-string (concat "cat " repo "/MOTD"))))))

(defun kiss/update ()
  (interactive)
  (message "kiss/update")
  (kiss/internal--update-git-repos))
;; (async-shell-command "KISS_PROMPT=0 kiss update"))

;; -> upgrade      Update the system
;; ===========================================================================

(defun kiss/internal--pkg-remote-eq-pkg-local-p (pkg)
  "(I) Return t if the version of PKG is the same locally and from the remotes."
  (string=
   (kiss/internal--sanitize-ver-str
    (f-read-text (concat (car (kiss/search pkg)) "/version")))
   (kiss/internal--sanitize-ver-str
    (kiss/internal--get-installed-pkg-version pkg))))


;; TODO: consider making this *not* internal.
(defun kiss/internal--get-out-of-date-pkgs ()
  "(I) Return a list of PKGS that are out of date."
  (cl-remove-if 'kiss/internal--pkg-remote-eq-pkg-local-p
                (cl-mapcar 'car (kiss/list))))

(defun kiss/upgrade ()
  (interactive)
  (async-shell-command "KISS_PROMPT=0 kiss Upgrade"))

(defun kiss/internal--pkgs-without-repo ()
  "(I) Return all packages that are installed that are not in a remote repo."
  (let ((pkgs-l (mapcar 'car (kiss/list))))
    (cl-remove-if-not
     (lambda (p)
       ;; Naturally, anything that was only *installed* will have 0 other
       ;; occurances.
       (eq 0
           (length
            ;; Remove the installed-db-dir *repo* from the list.
            (cl-remove-if
             (lambda (repo) (string-match-p kiss/installed-db-dir repo))
             (kiss/search p)))))
     pkgs-l)))

(defun kiss/internal--get-pkg-order (pkgs-lst)
  "(I) Get the proper build order for the packages in PKGS-LST."
  (cl-remove-if-not
   (lambda (pkg) (member pkg pkgs-lst))
   (kiss/internal--get-pkg-dependency-order pkgs-lst)))

;; -> version      Package manager version
;; SEE const.

;; Run "kiss [H|help-ext]" to see all actions

(provide 'kiss)

;;; kiss.el ends here.
