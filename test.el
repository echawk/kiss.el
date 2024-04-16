
(eval-when-compile
  (require 'ert)
  (require 'kiss))

(ert-deftest kiss--with-dir ()
  (should
   (string= "/opt"
            (kiss--with-dir "/opt" default-directory))))


(ert-deftest kiss--source-download ()
  (let ((objs
         (thread-last
           '("/usr/bin/emacs"
             "git+https://github.com/kiss-community/kiss")
           (mapcar #'kiss--string-to-source-obj))))
    (mapc (lambda (obj) (oset obj :package "kiss-el-test")) objs)
    (should
     (seq-reduce (lambda (x y) (and x y)) (mapcar #'kiss--source-download objs) t))))

(ert-deftest kiss--source-to-string ()
  (should
   (let* ((str-to-str (lambda (src) (kiss--source-to-string (kiss--string-to-source-obj src))))
          (src-to-str-test (lambda (str) (string= str (funcall str-to-str str)))))
     (and
      (funcall src-to-str-test "git+https://github.com/ehawkvu/clasp@musl path/")
      (funcall src-to-str-test "hg+https://github.com/ehawkvu/tsort.el tsort/")
      (funcall src-to-str-test "patches/fix.patch patches/")
      (funcall src-to-str-test "/usr/share/src/file")))))


(ert-deftest kiss--string-to-source-obj ()
  ;; Git source w/ branch
  (should
   (with-slots
       ((package          :package)
        (type             :type)
        (uri              :uri)
        (checksum         :checksum)
        (extracted-path   :extracted-path)
        (commit-or-branch :commit-or-branch))
       (kiss--string-to-source-obj "git+https://github.com/ehawkvu/clasp@musl")
     (and
      (string-empty-p package)
      (eq type 'git)
      (string= uri "https://github.com/ehawkvu/clasp")
      (string-empty-p checksum)
      (string-empty-p extracted-path)
      (string= "musl" commit-or-branch))))
  ;; Remote source w/ extracted path & package
  (should
   (let ((obj
          (kiss--string-to-source-obj
           "https://github.com/llvm/llvm-project/releases/download/llvmorg-17.0.5/llvm-17.0.5.src.tar.xz llvm")))
     (oset obj :package "llvm")
     (with-slots
         ((package          :package)
          (type             :type)
          (uri              :uri)
          (checksum         :checksum)
          (extracted-path   :extracted-path)
          (commit-or-branch :commit-or-branch))
         obj
       (and
        (string= package "llvm")
        (eq type 'remote)
        (string= uri "https://github.com/llvm/llvm-project/releases/download/llvmorg-17.0.5/llvm-17.0.5.src.tar.xz")
        (string-empty-p checksum)
        (string= extracted-path "llvm")
        (string-empty-p commit-or-branch))))))

(ert-deftest kiss--normalize-file-path ()
  (should
   (and
    (string= "/opt" (kiss--normalize-file-path "/opt"))
    (string= "/opt" (kiss--normalize-file-path "//opt"))
    (string= "/opt/kiss/test/here/"
             (kiss--normalize-file-path "//opt/kiss//test/here//")))))


(ert-deftest kiss--uid ()
  (let ((user "root"))
    (should
     (and
      (string=
       (user-login-name)
       (kiss--get-user-from-uid (user-uid)))
      (eq
       (user-uid)
       (kiss--get-uid-from-user (user-login-name)))
      (string= user
               (kiss--get-user-from-uid
                (kiss--get-uid-from-user user)))))))


(ert-deftest kiss--get-decompression-command ()
  (let ((examples
         '((".tar"  . "cat ")
           (".tbz"  . "bzip2 -dc ")
           (".bz2"  . "bzip2 -dc ")
           (".lz"   . "lzip -dc ")
           (".tgz"  . "gzip -dc ")
           (".gz"   . "gzip -dc ")
           (".lzma" . "lzma -dcT0 ")
           (".txz"  . "xz -dcT0 ")
           (".xz"   . "xz -dcT0 ")
           (".zst"  . "zstd -dcT0 "))))
    (should
     (thread-last
       examples
       (mapcar
        (lambda (pair)
          (string= (cdr pair)
                   (kiss--get-decompression-command (car pair)))))
       (funcall (lambda (l) (seq-reduce (lambda (x y) (and x y)) l t)))))))


(ert-deftest kiss-alternatives ()
  (should
   (not
    (seq-difference
     (mapcar (lambda (l) (nth 2 l)) (kiss-alternatives))
     (string-split
      (shell-command-to-string (concat "ls " kiss-choices-db-dir)) "\n" t)))))


(ert-deftest kiss--single-quote-string ()
  (should (string= "'as df'" (kiss--single-quote-string "as df"))))


(ert-deftest kiss-manifest ()
  (should
   (string=
    (kiss--read-text (concat kiss-installed-db-dir "kiss/manifest"))
    (kiss--manifest-to-string (kiss-manifest "kiss")))))


(ert-deftest kiss-owns ()
  (should
   (string=
    "kiss"
    (kiss-owns (executable-find "kiss")))))


(ert-deftest kiss--str-tarball-p ()
  (should
   (eq 0 (and
          (kiss--str-tarball-p "txz")
          (kiss--str-tarball-p "tar.xz")
          (kiss--str-tarball-p "tar.zst")
          (kiss--str-tarball-p "tar.lzma")))))


;;https://quickref.me/chmod.html
(ert-deftest kiss--rwx-lst-to-octal ()
  (should
   (and
    (= 7 (kiss--rwx-lst-to-octal '(114 119 120)))
    (= 6 (kiss--rwx-lst-to-octal '(114 119 45)))
    (= 5 (kiss--rwx-lst-to-octal '(114 45 120)))
    (= 4 (kiss--rwx-lst-to-octal '(114 45 45)))
    (= 3 (kiss--rwx-lst-to-octal '(45 119 120)))
    (= 2 (kiss--rwx-lst-to-octal '(45 119 45)))
    (= 1 (kiss--rwx-lst-to-octal '(45 45 120)))
    (= 0 (kiss--rwx-lst-to-octal '(45 45 45))))))


(ert-deftest kiss--file-rwx ()
  (should
   (and
    (string= "755" (kiss--file-rwx "/usr/bin/"))
    (string= "750" (kiss--file-rwx "/root/"))
    (string= "755" (kiss--file-rwx "/usr/bin/git"))
    (string= "755" (kiss--file-rwx "/usr/bin/kiss")))))


(ert-deftest kiss--dirname ()
  (let ((shell-dirname
         (lambda (str)
           (replace-regexp-in-string
            "\n" ""
            (shell-command-to-string (concat "dirname " str))))))
    (should
     (and
      (string=
       (funcall shell-dirname "/usr/bin/")
       (kiss--dirname "/usr/bin/"))))))


(ert-deftest kiss--basename ()
  (let ((shell-basename
         (lambda (str)
           (replace-regexp-in-string
            "\n" ""
            (shell-command-to-string (concat "basename " str))))))
    (should
     (and
      (string=
       (funcall shell-basename "/usr/bin/cc")
       (kiss--basename "/usr/bin/cc"))))))


(ert-deftest kiss--get-pkg-from-manifest ()
  (should
   (string= "kiss" (kiss--get-pkg-from-manifest
                    `(,kiss-installed-db-dir
                      ,(concat kiss-installed-db-dir "kiss/"))))))


(ert-deftest kiss-list ()
  (should
   (and
    (equal
     (mapcar #'car (kiss-list))
     (string-split
      (shell-command-to-string (concat "ls " kiss-installed-db-dir )) "\n" t))
    (equal
     (mapcar #'cdr (kiss-list '("kiss" "git")))
     (thread-last
       '("kiss" "git")
       (mapcar (lambda (pkg) (concat kiss-installed-db-dir pkg "/version")))
       (mapcar #'kiss--read-file)))
    (string=
     (cadr (kiss-list "kiss"))
     (car (kiss--read-file (concat kiss-installed-db-dir "kiss/version")))))))
