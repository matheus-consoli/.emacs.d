;; Bootstrap packages
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(org-agenda-files (quote ("~/Documentos/agenda.org")))
 '(package-selected-packages
   (quote
    (git-gutter-fringe+ toml-mode quick-peek fancy-battery fzf flycheck-haskell haskell-snippets haskell-tab-indent haskell-mode jedi flyspell-popup macrostep pomodoro flyspell-correct-helm pdf-tools company-auctex latex-preview-pane auctex markdown-mode+ company-math markdown-mode markdown flycheck-rust rust-mode dumb-jump ein nyan-mode elpy font-lock+ company-quickhelp multi-term htmlize ox-reveal spacemacs-theme spacelin whitespace-cleanup-mode which-key visual-regexp-steroids use-package try spaceline-all-the-icons solaire-mode smartparens simpleclip rainbow-mode rainbow-delimiters org-bullets neotree hlinum helm general evil doom-themes)))
 '(paradox-github-token t)
 '(shell-pop-universal-key "<f6>"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "CTDB" :family "Fira Code"))))
 '(ahs-definition-face ((t (:background "dark orange" :foreground "black"))))
 '(ahs-face ((t (:background "orange" :foreground "black"))))
 '(ahs-plugin-defalt-face ((t (:background "#1E2029" :foreground "dark orange"))))
 '(mode-line ((t (:background "#483D8B" :box nil))))
 '(mode-line-highlight ((t (:background "#483DAB"))))
 '(powerline-active1 ((t (:inherit mode-line :background "#242555" :foreground "white"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#242530" :foreground "white"))))
 '(spaceline-evil-insert ((t (:background "#6495ED" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-evil-motion ((t (:background "#9400D3" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-evil-normal ((t (:background "#8A2BE2" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-evil-replace ((t (:background "#6A5ACD" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-highlight-face ((t (:background "#6A5ACD" :foreground "#E6E6FA" :inherit (quote mode-line)))))
 '(spaceline-unmodified ((t (:background "#6A5ACD" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(vcursor ((t (:background "indigo" :underline t)))))
