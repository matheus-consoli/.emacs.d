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
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(helm-M-x-fuzzy-match t)
 '(helm-autoresize-max-height 0)
 '(helm-autoresize-min-height 40)
 '(helm-buffers-fuzzy-matching t)
 '(helm-echo-input-in-header-line t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-imenu-fuzzy-match t t)
 '(helm-mode-to-line-cycle-in-source nil t)
 '(helm-recentf-fuzzy-match t t)
 '(helm-scroll-amount 8)
 '(helm-semantic-fuzzy-match t t)
 '(helm-split-window-in-side-p nil)
 '(helm-split-window-inside-p nil)
 '(org-adapt-indentation nil)
 '(org-bullets-bullet-list (quote ("☣" "☢" "☠" "⚛" "◉")))
 '(org-export-with-smart-quotes t)
 '(org-reveal-mathjax t t)
 '(org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
 '(org-src-fontfy-natively t t)
 '(org-src-tab-acts-natively nil)
 '(package-selected-packages
   (quote
    (intero haskell-mode highlight-indent-guides ob-rust rust-ob racer cargo rust-mode format-all auto-highlight-symbol swiper-helm smartparens flycheck-inline quick-peek company-prescient flyckeck yasnippet-snippets yasnippet company-tabnine company-quickhelp company-box company elfeed htmlize ox-reveal flyspell-lazy treemacs-magit rainbow-delimiters doom-themes org-bullets use-package)))
 '(projectile-completion-system (quote helm))
 '(rmh-elfeed-org-files (quote ("~/.emacs.d/elfeed.org")))
 '(solaire-mode-remap-modeline nil)
 '(url-queue-timeout 30)
 '(which-key-popup-type (quote minibuffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:background "dark orange" :foreground "black"))))
 '(ahs-face ((t (:background "orange" :foreground "black"))))
 '(ahs-plugin-defalt-face ((t (:background "#1E2029" :foreground "dark orange"))))
 '(mode-line ((t (:height 0.9))))
 '(mode-line-inactive ((t (:height 0.9)))))
