;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq garbage-collection-messages t)

;;
;; Improve IO
;;
;; Optimizations for improving IO performance. Increase max bytes read from a sub-process in a single op
;; default value: 3145728 (3M)
(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 5 1024 1024)))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

(setq comp-deferred-compilation t)
(setq native-compile-prune-cache t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t
      use-package-always-defer nil
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      package-enable-at-startup t)

(straight-use-package 'org)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
;; (require 'straight-x)


(org-babel-load-file (expand-file-name (concat user-emacs-directory "readme.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "badd1a5e20bd0c29f4fe863f3b480992c65ef1fa63951f59aa5d6b129a3f9c4c" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "e2db454e757725af32c446c1abc58e1685c7f940a901faa3b7eea4e179786056" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "1278c5f263cdb064b5c86ab7aa0a76552082cf0189acf6df17269219ba496053" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:background "dark orange" :foreground "black"))))
 '(ahs-face ((t (:background "orange" :foreground "black"))))
 '(ahs-plugin-default-face ((t (:background "#1E2029" :foreground "dark orange"))))
 '(centaur-tabs-selected-modified ((t (:slant italic))))
 '(centaur-tabs-unselected-modified ((t (:slant italic))))
 '(company-tooltip-common ((t (:inhirit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inhirit company-tooltip-selection :weight bold :underline nil))))
 '(font-lock-comment-face ((t :slant italic)))
 '(lsp-inlay-hint-parameter-face ((t :inherit lsp-inlay-hint-face :height 90)))
 '(lsp-inlay-hint-type-face ((t :inherit lsp-inlay-hint-face :height 90)))
 '(mode-line ((t (:family "Martian Mono" :height 120))))
 '(mode-line-inactive ((t (:height 100 :underline nil :weight light))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
