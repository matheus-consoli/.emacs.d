;;; -*- lexical-binding: t -*-

(setq garbage-collection-messages t)

;; Initialize package sources
(require 'package)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1)
;; use-package-always-defer nil
;; straight-cache-autoloads t
;; package-enable-at-startup t)

(straight-use-package
 '(org
   :type git
   :host github
   :repo "emacsmirror/org"
   :files ("lisp/*.el" "contrib/lisp/*.el")))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
;;(setq use-package-compute-statistics t)

(org-babel-load-file (expand-file-name (concat user-emacs-directory "readme.org")))
