;;; test/test-kiss-file.el --- tests for kiss-file.el -*- lexical-binding: t; -*-
;;
;; Invariant tests: each pins a property the implementation must satisfy,
;; not a particular incidental output.

(require 'ert)
(require 'kiss)
(require 'kiss-test-fixture)

(defun kiss-test-make-tmpdir ()
  "Make and return a fresh temporary directory, with a trailing slash."
  (let ((d (concat temporary-file-directory (make-temp-name "kiss-test-") "/")))
    (make-directory d t)
    d))

(defun kiss-test-rm-tree (dir)
  (ignore-errors (delete-directory dir t)))


;; --------------------------------------------------------------------------
;; Path manipulation (pure functions)

(ert-deftest kiss-file/normalize-file-path ()
  "kiss--file-normalize-file-path collapses consecutive '/' to one."
  (should (string= (kiss--file-normalize-file-path "//usr//bin///foo")
                   "/usr/bin/foo"))
  (should (string= (kiss--file-normalize-file-path "/usr/bin/foo")
                   "/usr/bin/foo"))
  ;; Idempotence.
  (let ((once (kiss--file-normalize-file-path "///a//b///")))
    (should (string= once (kiss--file-normalize-file-path once)))))

(ert-deftest kiss-file/dirname-basename-reassemble-path ()
  "kiss--dirname + '/' + kiss--basename reproduces multi-level paths."
  (dolist (path '("/usr/bin/foo" "/etc/passwd"))
    (should (string= (concat (kiss--dirname path) "/" (kiss--basename path))
                     path)))
  ;; Invariant: dirname always returns an absolute prefix.
  (dolist (path '("/usr/bin/foo" "/etc/passwd" "/foo"))
    (should (string-prefix-p "/" (kiss--dirname path)))))

(ert-deftest kiss-file/basename-is-final-component ()
  (should (string= (kiss--basename "/usr/bin/foo") "foo"))
  (should (string= (kiss--basename "/foo") "foo")))


;; --------------------------------------------------------------------------
;; Magic-byte detectors
;;
;; Invariant: a file produced by compressor X is detected as X and by no
;; other detector.  Detection is magic-byte based, so file names are
;; irrelevant.

(defun kiss-test-make-compressed (dir prefix)
  "Write PREFIX under DIR plus .gz/.bz2/.xz copies; return the four paths."
  (let ((f (concat dir prefix)))
    (with-temp-file f (insert "hello\n"))
    (shell-command (concat "gzip -c '" f "' > '" f ".gz'"))
    (shell-command (concat "bzip2 -c '" f "' > '" f ".bz2'"))
    (shell-command (concat "xz -c '" f "' > '" f ".xz'"))
    (list f (concat f ".gz") (concat f ".bz2") (concat f ".xz"))))

(ert-deftest kiss-file/magic-gzip-detects-only-gzip ()
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let ((art (kiss-test-make-compressed dir "src")))
          (should (kiss--file-is-gzip-p (nth 1 art)))
          (dolist (other (list (nth 0 art) (nth 2 art) (nth 3 art)))
            (should-not (kiss--file-is-gzip-p other)))
          ;; Cross-detector invariants on the gz file.
          (should-not (kiss--file-is-bzip2-p (nth 1 art)))
          (should-not (kiss--file-is-xz-p (nth 1 art)))
          (should-not (kiss--file-is-tarball-p (nth 1 art))))
      (kiss-test-rm-tree dir))))

(ert-deftest kiss-file/magic-bzip2-detects-only-bzip2 ()
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let ((art (kiss-test-make-compressed dir "src")))
          (should (kiss--file-is-bzip2-p (nth 2 art)))
          (dolist (other (list (nth 0 art) (nth 1 art) (nth 3 art)))
            (should-not (kiss--file-is-bzip2-p other))))
      (kiss-test-rm-tree dir))))

(ert-deftest kiss-file/magic-xz-detects-only-xz ()
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let ((art (kiss-test-make-compressed dir "src")))
          (should (kiss--file-is-xz-p (nth 3 art)))
          (dolist (other (list (nth 0 art) (nth 1 art) (nth 2 art)))
            (should-not (kiss--file-is-xz-p other))))
      (kiss-test-rm-tree dir))))

(ert-deftest kiss-file/magic-tarball-detects-tar ()
  "A real tar archive (uncompressed) is detected; compressed junk is not."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let* ((inner (concat dir "inner/"))
               (tar (concat dir "t.tar")))
          (make-directory (concat dir "inner") t)
          (make-directory (concat inner "d/"))
          (with-temp-file (concat inner "d/f") (insert "x"))
          ;; The archive must live OUTSIDE the tree that is tarred: BSD
          ;; tar refuses to add the archive to itself (empty result).
          (shell-command (format "cd '%sinner' && tar -cf - . > '../t.tar'" dir))
          (should (kiss--file-is-tarball-p tar))
          (should-not (kiss--file-is-tarball-p (concat inner "d/f")))
          (should-not (kiss--file-is-gzip-p tar)))
      (kiss-test-rm-tree dir))))


;; --------------------------------------------------------------------------
;; File classification

(ert-deftest kiss-file/identify-classifies-dir-file-symlink ()
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let* ((f (concat dir "f"))
               (d (concat dir "d/"))
               (l (concat dir "l")))
          (with-temp-file f (insert "x"))
          (make-directory d)
          (make-symbolic-link f l)
          (should (eq (kiss--file-identify d) 'directory))
          (should (eq (kiss--file-identify f) 'file))
          (should (eq (kiss--file-identify l) 'symlink))
          (should (kiss--file-exists-p f))
          (should (kiss--file-exists-p d))
          (should-not (kiss--file-exists-p (concat dir "nope"))))
      (kiss-test-rm-tree dir))))

(ert-deftest kiss-file/exists-p-broken-symlink ()
  "kiss--file-exists-p is t for a broken symlink (its reason to exist)."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let ((l (concat dir "broken-link")))
          (make-symbolic-link (concat dir "does-not-exist") l)
          (should (kiss--file-exists-p l)))
      (kiss-test-rm-tree dir))))


;; --------------------------------------------------------------------------
;; Text I/O

(ert-deftest kiss-file/write-read-text-roundtrip ()
  "kiss--write-text then kiss--read-text returns the original bytes."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let ((f (concat dir "x.txt")))
          (dolist (text '("hello\n"
                          ""
                          "no-trailing-newline"
                          "a\nb\nc\n"
                          "special: \"quotes\" \\ backslash\n"))
            (kiss--write-text text 'utf-8 f)
            (should (string= (kiss--read-text f) text))))
      (kiss-test-rm-tree dir))))

(ert-deftest kiss-file/read-file-drops-blank-lines ()
  "kiss--file-read-file drops empty lines; nil for a missing file."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let* ((f (concat dir "lines.txt"))
               (missing (concat dir "missing.txt")))
          (kiss--write-text "a\n\nb\n\n\nc\n" 'utf-8 f)
          (should (equal (kiss--file-read-file f) '("a" "b" "c")))
          (should-not (kiss--file-read-file missing)))
      (kiss-test-rm-tree dir))))


;; --------------------------------------------------------------------------
;; Permissions

(ert-deftest kiss-file/rwx-triad-to-octal ()
  "kiss--rwx-lst-to-octal maps an r/w/x/\'-' triad to its octal digit.
The triad elements are the character codes of \='r', \='w', \='x', \='-'.

Invariant: each of the eight triads yields its standard octal digit, so
the mapping agrees with the (4,2,1) bit layout of POSIX modes."
  (let* ((r ?r) (w ?w) (x ?x) (d ?-))
    (should (= (kiss--rwx-lst-to-octal (list d d d)) 0))
    (should (= (kiss--rwx-lst-to-octal (list r d d)) 4))
    (should (= (kiss--rwx-lst-to-octal (list d w d)) 2))
    (should (= (kiss--rwx-lst-to-octal (list d d x)) 1))
    (should (= (kiss--rwx-lst-to-octal (list r w x)) 7))
    (should (= (kiss--rwx-lst-to-octal (list r d x)) 5))
    (should (= (kiss--rwx-lst-to-octal (list d w x)) 3))
    (should (= (kiss--rwx-lst-to-octal (list r w d)) 6))))

(ert-deftest kiss-file/file-rwx-matches-chmod ()
  "kiss--file-rwx reports the mode set by chmod, as three octal digits."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let* ((exec (concat dir "exec"))
               (plain (concat dir "plain")))
          (with-temp-file exec (insert "#!/bin/sh\n"))
          (with-temp-file plain (insert "x"))
          (chmod exec #o755)
          (chmod plain #o644)
          (should (string= (kiss--file-rwx exec) "755"))
          (should (string= (kiss--file-rwx plain) "644"))
          (should (kiss--file-is-executable-p exec))
          (should-not (kiss--file-is-executable-p plain)))
      (kiss-test-rm-tree dir))))


;; --------------------------------------------------------------------------
;; Removal

(ert-deftest kiss-file/remove-file-is-idempotent ()
  "kiss--file-remove-file removes existing files and tolerates absent ones."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let* ((f (concat dir "to-remove"))
               (missing (concat dir "never-existed")))
          (with-temp-file f (insert "x"))
          (kiss--file-remove-file f)
          (should-not (file-exists-p f))
          (kiss--file-remove-file missing)
          (should-not (file-exists-p missing)))
      (kiss-test-rm-tree dir))))

(ert-deftest kiss-file/remove-directory-empty-only ()
  "kiss--file-remove-directory removes empty dirs, fails on non-empty."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let* ((empty (concat dir "empty/"))
               (full (concat dir "full/")))
          (make-directory empty)
          (make-directory full)
          (with-temp-file (concat full "f") (insert "x"))
          (kiss--file-remove-directory empty)
          (should-not (file-exists-p empty))
          (should-not (kiss--file-remove-directory full))
          (should (file-exists-p full)))
      (kiss-test-rm-tree dir))))


;; --------------------------------------------------------------------------
;; Compression commands & decompression

(ert-deftest kiss-env/compression-command-lookup ()
  "kiss--get-compression-command maps each valid kiss-compress to a command."
  (should (string= (let ((kiss-compress "gz")) (kiss--get-compression-command))
                   "gzip -c"))
  (should (string= (let ((kiss-compress "xz")) (kiss--get-compression-command))
                   "xz -cT0"))
  (should (string= (let ((kiss-compress "")) (kiss--get-compression-command))
                   "cat")))

(ert-deftest kiss-file/decompress-gzip-restores-content ()
  "kiss--decompress on a .tar.gz restores the original uncompressed bytes."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let* ((orig (concat dir "orig"))
               (gz (concat dir "pkg.tar.gz"))
               (out (concat dir "out")))
          (with-temp-file orig (insert "tar-content-here\n"))
          (shell-command (concat "gzip -c '" orig "' > '" gz "'"))
          (kiss--decompress gz out)
          (should (string= (kiss--read-text out) (kiss--read-text orig))))
      (kiss-test-rm-tree dir))))


;; --------------------------------------------------------------------------
;; Checksums

(ert-deftest kiss-file/sh256-known-vector ()
  "kiss--sh256 of the ASCII bytes of \"abc\" is the known SHA-256 vector."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let ((f (concat dir "abc")))
          (with-temp-file f (insert "abc"))
          (should (string= (kiss--sh256 f)
                           "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")))
      (kiss-test-rm-tree dir))))

(ert-deftest kiss-file/b3sum-when-available ()
  "kiss--b3 returns a 66-hex-char BLAKE3 digest (b3sum -l 33) when available."
  (unless (executable-find "b3sum") (skip "b3sum not installed"))
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (let ((f (concat dir "abc")))
          (with-temp-file f (insert "abc"))
          (should (string-match-p (rx bol (= 66 (any "0-9a-f")) eol)
                                  (kiss--b3 f))))
      (kiss-test-rm-tree dir))))


;; --------------------------------------------------------------------------
;; System library detection (pure)

(ert-deftest kiss-file/lib-is-system-p ()
  "kiss--lib-is-system-p recognizes glibc/system libs, rejects package libs."
  (should (kiss--lib-is-system-p "libc.so.6"))
  (should (kiss--lib-is-system-p "libm.so.6"))
  (should (kiss--lib-is-system-p "libstdc++.so.6"))
  (should (kiss--lib-is-system-p "ld-linux-x86-64.so.2"))
  (should (kiss--lib-is-system-p "ldd"))
  (should-not (kiss--lib-is-system-p "libfoo.so.1"))
  (should-not (kiss--lib-is-system-p "libxdo.so"))
  (should-not (kiss--lib-is-system-p "not-a-library")))


;; --------------------------------------------------------------------------
;; Manifest generation

(ert-deftest kiss-manifest/get-manifest-for-dir ()
  "kiss--get-manifest-for-dir lists all files/dirs, drops .la & charset.alias,
and returns them uniquely sorted in descending order."
  (let ((dir (kiss-test-make-tmpdir)))
    (unwind-protect
        (progn
          (make-directory (concat dir "usr/bin") t)
          (with-temp-file (concat dir "usr/bin/foo") (insert "#!/bin/sh\n"))
          (make-directory (concat dir "etc") t)
          (with-temp-file (concat dir "etc/conf") (insert "x"))
          (with-temp-file (concat dir "foo.la") (insert "junk"))
          (with-temp-file (concat dir "charset.alias") (insert "junk"))
          (let ((m (kiss--get-manifest-for-dir dir)))
            (should (member "/usr/bin/foo" m))
            (should (member "/usr/bin/" m))
            (should (member "/usr/" m))
            (should (member "/etc/" m))
            (should (member "/etc/conf" m))
            ;; Filtered files must not appear.
            (should-not (seq-some (lambda (s) (string-match-p "\\.la$" s)) m))
            (should-not (member "/charset.alias" m))
            ;; Invariant: unique, sorted descending.
            (should (= (length m) (length (seq-uniq m))))
            (should (equal m (reverse (seq-sort #'string-lessp m))))))
      (kiss-test-rm-tree dir))))
