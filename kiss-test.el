;;; kiss-test.el -- tests for the kiss package manager in ELisp

;; Author: Ethan Hawk <ethan.hawk@valpo.edu>
;; Maintainer: Ethan Hawk <ethan.hawk@valpo.edu>

;;; Commentary:

;; NOTE: To get all functions, use `M-x occur`
;; and search for "^(defun kiss"

;;; Code:

;; TODO: add in a setup/teardown function so that
;; I can test actual downloading, etc.

(require 'ert)
(require 'kiss)

(ert-deftest kiss/internal--lst-to-string ()
  (should
   (string=
    (kiss/internal--lst-to-str '(1 2 3))
    "1 2 3")))

(ert-deftest kiss/internal--sanitize-ver-str ()
  (should
   (string=
    (kiss/internal--sanitize-ver-str
     "1.2.3 1
"
     )
    "1.2.31")))

;; (defun kiss/internal--get-owner (file-path)
;; (defun kiss/internal--am-owner-p (file-path)
;; (defun kiss/internal--shell-command-as-user (command user)
;; (defun kiss/internal--decompress (file-path)
;; (defun kiss/internal--b3 (file-path)
;; (defun kiss/internal--sh256 (file-path)
;; (defun kiss/alternatives ()
;; (defun kiss/internal--get-installed-manifest-files ()
;; (defun kiss/owns (file-path)
;; (defun kiss/preferred ()
;; (defun kiss/internal--get-pkg-dependencies (pkg)
;; (defun kiss/internal--get-pkg-dependency-graph (pkg)
;; (defun kiss/internal--dependency-graph-to-tsort (pkg-depgraph)
;; (defun kiss/internal--get-pkg-tsort-graph (pkg)
;; (defun kiss/internal--get-pkg-dependency-order (pkg-lst)
;; (defun kiss/internal--get-pkg-make-dependents (pkg)
;; (defun kiss/internal--get-pkg-make-orphans ()
;; (defun kiss/internal--get-pkg-hard-dependents (pkg)
;; (defun kiss/internal--get-pkg-missing-dependencies (pkg)
;; (defun kiss/internal--get-pkg-orphan-alternatives (pkg)
;; (defun kiss/internal--pkg-is-removable-p (pkg)
;; (defun kiss/internal--get-pkg-version (pkg)
;; (defun kiss/internal--get-pkg-bin-name (pkg version)
;; (defun kiss/internal--get-pkg-cached-bin (pkg)
;; (defun kiss/internal--build-pkg (pkg)
;; (defun kiss/build (pkgs-l)
;; (defun kiss/checksum (pkgs-l)
;; (defun kiss/download (query)
;; (defun kiss/internal--get-pkg-sources (pkg)
;; (defun kiss/internal--get-pkg-sources-type (pkg-sources)
;; (defun kiss/internal--download-git-source (url dest)
;; (defun kiss/internal--make-temp-file ()
;; (defun kiss/internal--get-download-utility-arguments ()
;; (defun kiss/internal--download-remote-source (url dest)
;; (defun kiss/internal--download-local-source (file-path dest)
;; (defun kiss/internal--download-pkg-sources (pkg)
;; (defun kiss/install (pkgs-l)
;; (defun kiss/internal--pkg-is-installed-p (pkg)
;; (defun kiss/install-if-not-installed (pkgs-l)
;; (defun kiss/internal--get-installed-pkg-version (pkg)
;; (defun kiss/list (&optional pkg-q)
;; (defun kiss/remove (pkgs-l)
;; (defun kiss/search (q)
;; (defun kiss/internal--dir-is-git-repo-p (dir)
;; (defun kiss/internal--git-subm-superproject-dir (dir)
;; (defun kiss/internal--dir-is-git-subm-p (dir)
;; (defun kiss/internal--get-git-dir-toplevel (dir)
;; (defun kiss/internal--kiss-path-git-repos ()
;; (defun kiss/internal--update-git-repos ()
;; (defun kiss/update ()
;; (defun kiss/internal--pkg-remote-eq-pkg-local-p (pkg)
;; (defun kiss/internal--get-out-of-date-pkgs ()
;; (defun kiss/upgrade ()
;; (defun kiss/internal--pkgs-without-repo ()
;; (defun kiss/internal--get-pkg-order (pkgs-lst)

;;; kiss-test.el ends here.
