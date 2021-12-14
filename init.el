;; Bootstrap packages
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			             ("melpa" . "https://melpa.org/packages/")
			             ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* (* 1024 1024) 5)) ;; 5mb

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(setq comp-deferred-compilation t)


(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant


(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" default))
 '(package-selected-packages
   '(ob-ipython elpy lsp-python-ms ob-rust rust-mode yaml-mode tree-sitter-langs tree-sitter multiple-cursors highlight-indent-guides format-all auto-highlight-symbol smartparens lsp-ui iedit dap-mode flycheck-inline flycheck yasnippet-snippets yasnippet company-quickhelp company-box company git-gutter-fringe+ fringe-helper doom-modeline rainbow-delimiters all-the-icons which-key use-package undo-tree swiper-helm solaire-mode ox-reveal ox-gfm org-bullets multi-term magit kaolin-themes htmlize helm-rg helm-projectile google-translate good-scroll flyspell-popup flyspell-lazy expand-region easy-kill drag-stuff doom-themes dockerfile-mode crux comment-tags auto-package-update anzu))
 '(warning-suppress-log-types '((emacs) (comp) (comp) (:warning)))
 '(warning-suppress-types '((comp) (comp) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:background "dark orange" :foreground "black"))))
 '(ahs-face ((t (:background "orange" :foreground "black"))))
 '(ahs-plugin-defalt-face ((t (:background "#1E2029" :foreground "dark orange"))) t)
 '(ahs-plugin-default-face ((t (:background "#1E2029" :foreground "dark orange"))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:weight bold :font "Cascadia Mono" :height 2.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:weight bold :font "Cascadia Mono" :height 1.75))))
 '(org-level-2 ((t (:weight bold :font "Cascadia Mono" :height 1.5))))
 '(org-level-3 ((t (:weight bold :font "Cascadia Mono" :height 1.25))))
 '(org-level-4 ((t (:weight bold :font "Cascadia Mono" :height 1.1))))
 '(org-level-5 ((t (:weight bold :font "Cascadia Mono"))))
 '(org-level-6 ((t (:weight bold :font "Cascadia Mono"))))
 '(org-level-7 ((t (:weight bold :font "Cascadia Mono"))))
 '(org-level-8 ((t (:weight bold :font "Cascadia Mono")))))
