;; Bootstrap packages
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			             ("melpa" . "https://melpa.org/packages/")
			             ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold gc-cons-threshold-original)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)


(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fc0fe24e7f3d48ac9cf1f87b8657c6d7a5dd203d5dabd2f12f549026b4c67446" "9089d25e2a77e6044b4a97a2b9fe0c82351a19fdd3e68a885f40f86bbe3b3900" "9ef81da35ce99a4c7155db7d46e4f8c20a51860d6879cf082e3ed1c5222c17d3" "6a0d7f41968908e25b2f56fa7b4d188e3fc9a158c39ef680b349dccffc42d1c8" "17a58e509bbb8318abf3558c4b7b44273b4f1b555c5e91d00d4785b7b59d6d28" "c499bf4e774b34e784ef5a104347b81c56220416d56d5fd3fd85df8704260aad" "8ce796252a78d1a69e008c39d7b84a9545022b64609caac98dc7980d76ae34e3" "7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "bb28b083fe1c61848c10c049be076afc572ea9bee6e1f8dc2631c5ee4f7388c8" "c1c459af570241993823db87096bc775506c378aa02c9c6cd9ccaa8247056b96" default))
 '(package-selected-packages
   '(magit lsp-mssql company-terraform terraform-mode composite gnuplot org-plus-contrib yasnippet-snippets which-key web-mode wakatime-mode use-package unicode-escape undo-tree treemacs-projectile treemacs-magit treemacs-icons-dired tree-mode swiper-helm solaire-mode smartparens ripgrep rainbow-mode rainbow-delimiters racer quick-peek python-black poetry ox-reveal ox-gfm org-ref org-noter-pdftools ob-rust ob-ipython nyan-mode multiple-cursors multi-term lsp-ui latex-preview-pane latex-pretty-symbols kaolin-themes jinja2-mode intero hlinum highlight-indent-guides helm-spotify-plus helm-rg helm-projectile google-translate git-gutter-fringe+ format-all flyspell-popup flyspell-lazy flycheck-inline find-file-in-project fancy-battery expand-region emmet-mode elpy elixir-yasnippets elixir-mode elfeed-org elfeed-goodies eglot dumb-jump drag-stuff doom-themes doom-modeline dockerfile-mode docker-compose-mode django-snippets diminish dashboard dap-mode company-quickhelp company-prescient company-lsp company-box company-auctex comment-tags auto-highlight-symbol anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:background "dark orange" :foreground "black"))))
 '(ahs-face ((t (:background "orange" :foreground "black"))))
 '(ahs-plugin-defalt-face ((t (:background "#1E2029" :foreground "dark orange"))))
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
