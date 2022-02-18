;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 100 1024 1024 ))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

(setq comp-deferred-compilation t)


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(org-babel-load-file (expand-file-name (concat user-emacs-directory "readme.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1278c5f263cdb064b5c86ab7aa0a76552082cf0189acf6df17269219ba496053" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:background "dark orange" :foreground "black"))))
 '(ahs-face ((t (:background "orange" :foreground "black"))))
 '(ahs-plugin-defalt-face ((t (:background "#1E2029" :foreground "dark orange"))) t)
 '(company-tooltip-common ((t (:inhirit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inhirit company-tooltip-selection :weight bold :underline nil))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch))))))
