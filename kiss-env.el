;;; kiss-env.el --- KISS environment values -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ethan Hawk

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Environment values for KISS.

;;; Code:

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
(defconst kiss-compat-version "6.1.1"
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
  '("ssu" "sudo" "doas")
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
  `((,(rx ".tar" eol)                   . "cat ")
    (,(rx "." (? "t") "bz" (? "2") eol) . "bzip2 -dc ")
    (,(rx ".lz" eol)                    . "lzip -dc ")
    (,(rx "." (? "t") "gz" eol)         . "gzip -dc ")
    (,(rx ".lzma" eol)                  . "lzma -dcT0 ")
    (,(rx "." (? "t") "xz" eol)         . "xz -dcT0 ")
    (,(rx ".zst" eol)                   . "zstd -dcT0 "))
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

(defcustom kiss-strip
  1
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

(defcustom kiss-build-env-hook
  nil
  "A list of paths to executable files."
  :type '(string))

(defcustom kiss-make-chroot-strategy
  'prohibit-user-alternatives
  "Denotes the strategy that kiss--make-chroot-dir-for-pkg will use."
  :type 'symbol
  :options '(permit-user-alternatives prohibit-user-alternatives))

(defcustom kiss-perfom-build-in-sandbox
  nil
  "Set to t if you want build to be performed in a sandbox."
  :type 'boolean)

(defcustom kiss-sandbox-utility
  "bwrap"
  "set to an executable for sandboxing.

Valid strings: bwrap, proot."
  :type 'string
  :options '("bwrap" "proot"))

;;; Macros

(defmacro kiss--with-dir (dir-path expr)
  `(let ((default-directory ,dir-path))
     ,expr))

