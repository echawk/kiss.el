;;; kiss.el -- kiss package manager in ELisp

;; Author: Ethan Hawk <ethan.hawk@valpo.edu>
;; Maintainer: Ethan Hawk <ethan.hawk@valpo.edu>
;; URL: https://github.com/ehawkvu/kiss.el
;; Keywords: package-manager, tools
;; Package-Requires: ((f))

;; This file is under the MIT license.

;;; Commentary:

;; WHY??? - cuz it's good to have multiple implementations of kiss.
;; also, I don't want to leave Emacs, and this could lead to some
;; delcarative configurations of kiss.

;; TODO: need to make multiple versions of these fucntions.
;; Need to figure out what exactly to support - I'm thinking of
;; having a subset of kiss' features supported, primarily enough
;; for interactive use, as well as to support a delcarative configuration.
;; Anything else will be out of scope (for the time being).

;; TODO: support crux style usage???
;; - Would require that this file could be ran as an Emacs script, argument
;; parsing and all.

;; Hooks are currently on the back burner until a POC can be fleshed out.
;; However, I don't think that it will actually be all that difficult, since
;; Emacs already has built-in functionality for them...

;; Additionally, I may add some extensions/configuration specific to this
;; version of kiss.  Nothing that would break compatibilty with upstream kiss,
;; since you should be able to rely on that to fix you system should something
;; break, things such as having a nicer syntax to create your KISS_PATH,
;; the ability to export the settings in here to a shell file that you
;; can use with standard kiss, etc.

;; Useful info on files:
;; http://xahlee.info/emacs/emacs/elisp_file_name_dir_name.html

;; How to traverse directories.
;; http://xahlee.info/emacs/emacs/elisp_traverse_dir.html
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
(defconst kiss/installed-db-dir (concat kiss/root "/var/db/kiss/installed/"))
(defconst kiss/choices-db-dir (concat kiss/root "/var/db/kiss/choices/"))

(defconst kiss/KISS_TMPDIR
  (concat (getenv "HOME") "/.cache/kiss/proc/"))

(defconst kiss/KISS_SRCDIR
  (concat (getenv "HOME") "/.cache/kiss/sources/"))

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

(defun kiss/internal--decompress (file-path)
  "(I) Decompress FILE-PATH based on the file name."
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
    ;; Also... Am I sure that I'm running this command here???
    ;; My thinking is that I return the decompress command to be concatenated
    ;; with the tar command all in one 'shell-command' that way I don't have to
    ;; store random uncompressed tarballs everywhere.  I'll figure it out
    ;; eventually.
    ;; TODO: Add in error message/assertion.
    (if cmd
        (shell-command (concat cmd file-path)))))

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
(defun kiss/alternatives ()
  (interactive)
  ;; (async-shell-command "kiss alternatives")
  (mapcar
   (lambda (s)
     (let ((d (split-string s ">")))
       (list (car d)
             (concat "/" (string-join (cdr d) "/"))
             s)))
   (nthcdr 2 (directory-files kiss/choices-db-dir))))

(defun kiss/internal--get-installed-manifest-files ()
  "(I) Return a list of all of the installed manifest files."
  (mapcar
   '(lambda (pkg) (concat kiss/installed-db-dir pkg "/manifest"))
   (mapcar 'car (kiss/list))))

(defun kiss/owns (file-path)
  ;; TODO: See if this can be made a little less ugly.
  ;; FIXME: properly error out.
  (car
   (string-split
    (replace-regexp-in-string
     kiss/installed-db-dir ""
     (shell-command-to-string
      (concat "grep " (rx bol (literal file-path) eol) " "
              (kiss/internal--lst-to-str
               (kiss/internal--get-installed-manifest-files)))))
    "/")))

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
(defun kiss/internal--get-pkg-dependency-graph (pkg)
  "(I) Generate a graph of the dependencies for PKG."
  (let* ((deps (kiss/internal--get-pkg-dependencies pkg))
         (seen '())
         (queue deps)
         (res `((,pkg ,deps))))
    (progn
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
      res)))

(defun kiss/internal--dependency-graph-to-tsort (pkg-depgraph)
  "(I) Convert a PKG-DEPGRAPH graph to a tsort(1) compatible one."
  (let ((print-pair
         (lambda (pair)
           (if (cadr pair)
               (mapconcat (lambda (p) (message "%s %s" p (car pair)))
                          (cadr pair) "\n")))))
    (let ((pair-str
           (funcall print-pair
                    pkg-depgraph
                    )))
      (if pair-str pair-str ""))))


;; NOTE: will likely be removed once a tsort implementation in elisp is written.
(defun kiss/internal--get-pkg-tsort-graph (pkg)
  "(I) Get a tsort(1) compatible representation of the dependencies for PKG."
  (mapconcat #'kiss/internal--dependency-graph-to-tsort
             (kiss/internal--get-pkg-dependency-graph pkg) "\n"))


;; FIXME: There is a bug in this procedure where if two of the packages
;; in PKG-LST share a dependency tree, tsort will freak out and repot
;; that there is an odd number of dependencies in the tree.
;; I'm not entirely sure on how to fix this bug atm, since the piece of sample
;;  code that I have written does not report the correct packages (even though
;; it produces the same dependency tree.)
(defun kiss/internal--get-pkg-dependency-order (pkg-lst)
  "(I) Return the proper build order of the dependencies for each pkg in PKG-LST."
  (cl-remove-if (lambda (s) (string= s ""))
                (string-split
                 (shell-command-to-string
                  (concat "printf '"
                          ;; (mapconcat
                          ;;  #'kiss/internal--get-pkg-tsort-graph pkg-lst)
                          ;; FIXME: test this code below to see if it
                          ;; fixes the issues.
                          (let ((s (mapcar
                                    #'kiss/internal--get-pkg-tsort-graph
                                    pkg-lst)))
                            ;; Essentially just rms the duplicate lines.
                            (mapconcat #'identity
                                       (delete-dups
                                        (split-string
                                         (mapconcat #'identity s "\n")
                                         "\n"))
                                       "\n"))
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
  ;; * nothing on the system depends on the package
  ;; * the package does not leave any orphaned alternatives
  (and
   (kiss/internal--pkg-is-installed-p pkg)
   (eq (kiss/internal--get-pkg-hard-dependents pkg) nil)
   (eq (kiss/internal--get-pkg-orphan-alternatives pkg) nil)))

;; -> build        Build packages
;; ===========================================================================

(defun kiss/internal--get-pkg-version (pkg)
  "(I) Get the version for PKG using the car of `kiss/search'."
  (let ((pdir (car (kiss/search pkg))))
    (replace-regexp-in-string
     "\n$" ""
     ;; TODO: see if there is a way to avoid
     ;; depending on f.el
     (f-read-text (concat pdir "/version")))))

(defun kiss/internal--get-pkg-bin-name (pkg version)
  "(I) Return the proper name for the binary for PKG at VERSION."
  (concat pkg "@"
          (replace-regexp-in-string " " "-" version)
          ".tar." kiss/KISS_COMPRESS ))

(defun kiss/internal--build-pkg (pkg)
  "(I) Build PKG."
  (let ((pkg "clang"))
    (let ((missing-deps (kiss/internal--get-pkg-missing-dependencies pkg)))
      (if (not missing-deps)
          (let ((build-script (concat (car (kiss/search pkg)) "/build")))
            build-script

            ))))
  )

(defun kiss/build (pkgs-l)
  (interactive)
  (cond ((listp pkgs-l) nil)
        ((atom pkgs-l) nil)))
;; (async-shell-command
;;  (concat "kiss build " (kiss/internal--lst-to-str pkgs-l))))

;; -> checksum     Generate checksums
;; ===========================================================================
(defun kiss/checksum (pkgs-l)
  (interactive)
  (async-shell-command
   (concat "kiss checksum " (kiss/internal--lst-to-str pkgs-l))))

;; -> download     Download sources
;; ===========================================================================
(defun kiss/download (query)
  (interactive "sQuery: ")
  (async-shell-command (concat "kiss download " query)))

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

(defun kiss/internal--get-pkg-sources-type (pkg-sources)
  "(I) Return the type of PKG-SOURCES."
  (let ((pkg-url (car pkg-sources)))
    ;; TODO: need to ensure that this is the same expected behavior as
    ;; upstream.
    (cond
     ((string-match-p (rx bol "git+") pkg-url) "git")
     ((string-match-p (rx "://") pkg-url) "remote")
     (t "local"))
    ))

(defun kiss/internal--download-git-source (url dest)
  "(I) Download git URL to `kiss/KISS_SRCDIR' in the folder DEST."
  ;; NOTE: This currently does not support sources like the following:
  ;; git+https://github.com/user/project@somebranch#somecommit
  ;; However, I have yet to see this combo out in the wild in kiss linux.
  ;; So... It's not a bug (yet). Also, it just doesn't make sense to do anyways.
  (let* ((u (replace-regexp-in-string
             (rx bol "git+") ""
             url))
         (clean-url (car (string-split u (rx (or "#" "@")))))
         (dest-folder
          (concat dest "/"
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
  (shell-command-to-string "mktemp"))

(defun kiss/internal--get-download-utility-arguments ()
  "(I) Get the proper arguments for the set `kiss/KISS_GET' utility appending ARG."
  (cond
   ((string= kiss/KISS_GET "aria2c") " -d / -o ")
   ((string= kiss/KISS_GET "axel")   " -o ")
   ((string= kiss/KISS_GET "curl")   " -fLo ")
   ((or (string= kiss/KISS_GET "wget") (string= kiss/KISS_GET "wget2")) " -O ")))

(defun kiss/internal--download-remote-source (url dest)
  "(I) Download URL to DEST using `kiss/KISS_GET'."
  (shell-command
   (concat kiss/KISS_GET " "
           url
           (kiss/internal--get-download-utility-arguments)
           dest)))

(defun kiss/internal--download-local-source (file-path dest)
  "(I) Copy FILE-PATH to DEST using cp(1)."
  (if (file-exists-p file-path)
      (shell-command
       (concat "cp " file-path " " dest))))

(defun kiss/internal--download-pkg-sources (pkg)
  "(I) Download the sources for PKG into `kiss/KISS_SRCDIR'."
  (let* ((pkg-source-cache-dir (concat kiss/KISS_SRCDIR pkg))
         (type-pkg-sources
          (let ((pkg-sources (kiss/internal--get-pkg-sources pkg)))
            ;; Save the type as the car, with the cdr being the
            ;; URL followed by the (optional) path. Hence the zip.
            (-zip
             (mapcar 'kiss/internal--get-pkg-sources-type
                     pkg-sources)
             pkg-sources))))
    ;; Make the cache directory if it doesn't already exist.
    (if (not (file-exists-p pkg-source-cache-dir))
        (make-directory pkg-source-cache-dir))
    ;; FIXME: Actually implement the downloading.
    (dolist (tps type-pkg-sources)
      (cond
       ((string-match-p (car tps) "git")
        (print (cdr tps))

        )
       ((string-match-p (car tps) "remote")
        (print (cdr tps))

        )
       ((string-match-p (car tps) "local")
        (print (cdr tps))

        )
       )))
  )

;; -> install      Install packages
;; ===========================================================================
(defun kiss/install (pkgs-l)
  (interactive)
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

(defun kiss/internal--get-installed-package-version (pkg)
  "(I) Return the version string for PKG, nil if PKG is not installed."
  (if (kiss/internal--pkg-is-installed-p pkg)
      (let ((pdir (concat kiss/installed-db-dir pkg)))
        (replace-regexp-in-string
         "\n$" ""
         ;; TODO: see if there is a way to avoid
         ;; depending on f.el
         (f-read-text (concat pdir "/version"))))))

;; TODO: add docstring.
(defun kiss/list (&optional pkg-q)
  (if (eq nil pkg-q)
      (let ((pkgs (nthcdr 2 (directory-files kiss/installed-db-dir))))
        (cl-mapcar (lambda (p)
                     (let ((pdir (concat kiss/installed-db-dir p)))
                       (list p (kiss/internal--get-installed-package-version p))))
                   pkgs))
    (list pkg-q (kiss/internal--get-installed-package-version pkg-q))))

;; -> remove       Remove packages
;; ===========================================================================
(defun kiss/remove (pkgs-l)
  (interactive)
  (async-shell-command
   (concat "kiss remove " (kiss/internal--lst-to-str pkgs-l))))

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
    (kiss/internal--get-installed-package-version pkg))))


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

;;; kiss.el ends here.
