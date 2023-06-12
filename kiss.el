;;; kiss.el -- kiss package manager in ELisp

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
  (require 'f))

;; FIXME: Find out what the containing group should be...
(defgroup kiss nil
  "The KISS package manager, in ELisp.")

;; TODO: cleanup these names, they're not consistent...
(defconst kiss/root "/")
(defconst kiss/installed-db-dir (concat kiss/root "/var/db/kiss/installed/"))
(defconst kiss/choices-db-dir (concat kiss/root "/var/db/kiss/choices/"))

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
    ;; TODO: Add in error message/assertion.
    (if cmd
        (shell-command (concat cmd file-path)))))

(defun kiss/internal--b3 (file-path)
  "(I) Run b3sum on FILE-PATH."
  (car
   (string-split
    (replace-regexp-in-string
     "\n$" ""
     ;; NOTE: b3sum is the only support BLAKE3 utility at this time.
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

;; -> kiss [a|b|c|d|i|l|r|s|u|U|v] [pkg]...
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

;; -> build        Build packages
;; ===========================================================================
(defun kiss/build (pkgs-l)
  (interactive)
  (async-shell-command
   (concat "kiss build " (kiss/internal--lst-to-str pkgs-l))))

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
        (replace-regexp-in-string "\n$" ""
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
;; -> version      Package manager version
;; SEE const.

;; Run "kiss [H|help-ext]" to see all actions

;;; kiss.el ends here.
