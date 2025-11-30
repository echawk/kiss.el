;;; kiss-build.el --- KISS Build abstractions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(progn
  (require 'eieio)
  (require 'subr-x)
  (require 'seq)
  )

(require 'kiss-env)
(require 'kiss-file)
(require 'kiss-package)
(require 'kiss-download)

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
  (shell-command (format "chmod +x '%s'" k-el-build)))


;; FIXME: support other kiss-elf commands
(defun kiss--build-get-missing-dependencies (dir file-path-lst pkg-obj)
  ;; NOTE: only works w/ ldd.
  (pcase system-type
    ('darwin
     (message "not supported yet...")
     ;; otool -l /opt/homebrew/bin/mu | awk '$1 ~ /^name$/ { print $2 }'
     )
    ('gnu/linux
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
      (slot-value pkg-obj :depends)))))


(defun kiss--build-strip-files (dir file-path-lst)
  (mapcar
   (lambda (fp) (kiss--file-strip-file (concat dir fp)))
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
         ;;    (kiss--package-get-dependency-graph package)
         ;;    (kiss--package-get-dependency-graph package t)))

         (kiss--package-get-dependency-order
          (seq-uniq
           (append
            (cdr (reverse (kiss--package-get-dependency-order package)))
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
           (kiss--package-get-order)

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
                 (format
                  "mv -f '%s' '%s'"
                  
                  (concat
                   (kiss--file-normalize-file-path
                    (concat chroot-dir kiss-choices-db-dir pkg))
                   (concat ">" (string-join (string-split file "/" t) ">")))
                  
                  (kiss--file-normalize-file-path
                   (concat chroot-dir file))))))
            package-needs-to-provide-lst)))))))

(defun kiss--package-build (pkg-obj)
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
         (format "rm -rf -- '%s'" proc-dir))
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

(provide 'kiss-build)
