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
   '("d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "733ef3e3ffcca378df65a5b28db91bf1eeb37b04d769eda28c85980a6df5fa37" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "88f59acbeacefb4998f45126d4d8ae8b2184f2a48753db362a349fd55321c7e1" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "6e14157d0c8857e81035e6c7131dc17e4115b3911c82a1fd32e528aec8e89eab" "fc0fe24e7f3d48ac9cf1f87b8657c6d7a5dd203d5dabd2f12f549026b4c67446" "9089d25e2a77e6044b4a97a2b9fe0c82351a19fdd3e68a885f40f86bbe3b3900" "9ef81da35ce99a4c7155db7d46e4f8c20a51860d6879cf082e3ed1c5222c17d3" "6a0d7f41968908e25b2f56fa7b4d188e3fc9a158c39ef680b349dccffc42d1c8" "17a58e509bbb8318abf3558c4b7b44273b4f1b555c5e91d00d4785b7b59d6d28" "c499bf4e774b34e784ef5a104347b81c56220416d56d5fd3fd85df8704260aad" "8ce796252a78d1a69e008c39d7b84a9545022b64609caac98dc7980d76ae34e3" "7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "bb28b083fe1c61848c10c049be076afc572ea9bee6e1f8dc2631c5ee4f7388c8" "c1c459af570241993823db87096bc775506c378aa02c9c6cd9ccaa8247056b96" default))
 '(package-selected-packages
   '(org lsp-haskell auto-package-update crux easy-kill tree-sitter-langs tree-sitter iedit all-the-icons-completions highlight-numbers highlight-quoted yaml-mode lsp-python-ms company-tabnine magit lsp-mssql company-terraform terraform-mode composite gnuplot org-plus-contrib yasnippet-snippets which-key web-mode wakatime-mode use-package unicode-escape undo-tree treemacs-projectile treemacs-magit treemacs-icons-dired tree-mode swiper-helm smartparens ripgrep rainbow-mode rainbow-delimiters racer quick-peek python-black poetry ox-reveal ox-gfm org-ref org-noter-pdftools ob-rust ob-ipython nyan-mode multiple-cursors multi-term lsp-ui latex-preview-pane latex-pretty-symbols kaolin-themes jinja2-mode intero hlinum highlight-indent-guides helm-spotify-plus helm-rg helm-projectile google-translate git-gutter-fringe+ format-all flyspell-popup flyspell-lazy flycheck-inline find-file-in-project fancy-battery expand-region emmet-mode elpy elixir-yasnippets elixir-mode elfeed-org elfeed-goodies eglot dumb-jump drag-stuff doom-themes doom-modeline dockerfile-mode docker-compose-mode django-snippets diminish dashboard dap-mode company-quickhelp company-prescient company-lsp company-box company-auctex comment-tags auto-highlight-symbol anzu))
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
