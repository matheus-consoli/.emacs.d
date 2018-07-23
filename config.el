(use-package general
    :ensure t)

;; (use-package eshell
;;   :init
;;   (setq
;;    eshell-scroll-to-bottom-on-input 'all
;;    eshell-error-if-no-glob t
;;    eshell-hist-ignoredups t
;;    eshell-save-history-on-exit t
;;    eshell-prefer-lisp-functions nil
;;    eshell-destroy-buffer-when-process-dies t)
;;   (add-hook 'eshell-mode-hook
;;         (lambda ()
;;           (add-to-list 'eshell-visual-commands "ssh")
;;           (add-to-list 'eshell-visual-commands "tail")
;;           (add-to-list 'eshell-visual-commands "top"))))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(setenv "PATH"
    (concat
     "/usr/local/bin/:/usr/local/sbin:"
     (getenv "PATH")))

(defun eshell-clear ()
"Clear the eshell buffer."
(let ((inhibit-read-only t))
  (erase-buffer)
  (eshell-send-input)))

(use-package try
    :ensure t)

(use-package which-key
    :ensure t
    :config (which-key-mode))

(use-package org-bullets
    :ensure t
    :init (setq org-bullets-bullet-list
                '("◉" "☢"  "☣" "⚛" "☠"))
    :config (add-hook `org-mode-hook
                    (lambda () (org-bullets-mode 1))))

(use-package ox-reveal
  :ensure ox-reveal)
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t)

(use-package htmlize
  :ensure t)

(use-package multi-term
  :ensure t
  :bind ("<f6>" . consoli/open-multi-term-here)
  :config
  (setq ;; multi-term-dedicated-window-height (lambda () (/ (window-total-height) 3))
   multi-term-program "/bin/zsh")
  (add-hook 'term-mode-hook
            (lambda ()
              (dolist
                  (bind '(("C-y" . term-paste)
                          ("C-<backspace>" . term-send-backward-kill-word)
                          ("C-c C-c" . term-interrupt-subjob)
                          ("C-l" . (lambda () (let ((inhibit-read-only t)) (erase-buffer) (term-send-input))))
                          ("C-s" . isearch-forward)
                          ("C-r" . isearch-backward)))
                (add-to-list 'term-bind-key-alist bind)))))

(defun consoli/open-multi-term-here ()
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t)))))
    (multi-term-dedicated-open)
    (switch-to-buffer-other-window (multi-term-dedicated-get-buffer-name))
    (linum-mode -1)))

(defun last-term-mode-buffer (list-of-buffers)
  "Returns the most recently used term-mode buffer."
  (when list-of-buffers
    (if (eq 'term-mode (with-current-buffer (car list-of-buffers) major-mode))
        (car list-of-buffers)
      (last-term-mode-buffer (cdr list-of-buffers)))))

(setq multi-term-dedicated-close-back-to-open-buffer t)

(defun switch-to-term-mode-buffer ()
  "Switch to the most recently used term-mode buffer, or create a new one."
  (interactive)
  (let ((buffer (last-term-mode-buffer (buffer-list))))
    (if (not buffer)
        (consoli/open-multi-term-here)
      (multi-term-dedicated-toggle))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/img/banner_epurple.png")
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 3)))
  (setq dashboard-banner-logo-title "Happy Hacking!")
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package evil
  :ensure t)
(evil-mode t)

(require 'font-lock)

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "<C-f8>") 'neotree-hidden-file-toggle)
(setq neo-smart-open t)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil))
(require 'doom-themes)
(doom-themes-neotree-config)
(setq doom-neotree-enable-type-colors t)
(setq doom-neotree-enable-file-icons t)
(doom-themes-org-config)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq spaceline-buffer-encoding-abbrev-p nil)
  (setq spaceline-line-column-p nil)
  (setq spaceline-line-p nil)
  (setq powerline-default-separator (quote arrow))
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode 1))

(use-package flycheck
  :ensure t)

(use-package elpy
  :ensure t)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i  --simple-prompt")

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimun-prefix-lenght 1)) ;; maybe 3?
;; (with-eval-after-load 'company
  ;; (define-key company-active-map (kbd "SPC") #'company-abort))

(use-package company-jedi
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-jedi))
(defun python-mode-company-init ()
  (setq-local company-backends '((company-jedi
                                  company-etags
                                  company-dabbrev-code))))
(use-package company-jedi
  :ensure t
  :config
  (require 'company)
  (add-hook 'python-mode-hook 'python-mode-company-init))

(use-package company-quickhelp
  :ensure t
  :config (eval-after-load 'company
            '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))
(add-hook 'company-mode-hook #'company-quickhelp-mode)

(use-package whitespace-cleanup-mode
    :ensure t)

(use-package smartparens
  :ensure t)
(require 'smartparens-config)
(add-hook 'prog-mode #'smartparens-mode)
(add-hook 'org-mode #'smartparens-mode)
(smartparens-global-mode t)

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config (rainbow-delimiters-mode 1))

(use-package helm
  :ensure t
  :bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  :config
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 40
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-split-window-in-side-p nil
        helm-mode-to-line-cycle-in-source nil
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-echo-input-in-header-line t)
  :init
  (helm-mode 1))
(require 'helm-config)
(helm-autoresize-mode 1)

(use-package hlinum
  :ensure t)
(hlinum-activate)
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#3e4446")
(set-face-background 'highlight nil)

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "")
  (add-hook 'prog-mode-hook 'linum-relative-mode))

(use-package simpleclip
  :ensure t
  :init (simpleclip-mode 1))

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :init
  (require 'company)
  (slime-setup '(slime-fancy slime-company)))

(use-package projectile
  :ensure t)

(use-package solaire-mode
  :ensure t)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
(setq solaire-mode-remap-modeline nil)
(solaire-mode t)
(solaire-mode-swap-bg)

(use-package diminish
  :ensure t)

(use-package magit
  :ensure t)

(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t
        mode-line-format (list
                          '(:eval (list (nyan-create)))))
  (nyan-mode t))

(use-package auto-highlight-symbol
  :ensure t
  :init (global-auto-highlight-symbol-mode))

(load-theme 'doom-dracula
    :no-confirm)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq frame-title-format " CONSOLI")

;; no toolbar
(tool-bar-mode -1)

;; no menubar
(menu-bar-mode -1)

;; no scroll bar
(scroll-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t)

(setq initial-scratch-message nil
      inhibit-startup-echo-area-message t)
(message " WELCOME TO EMACS!")

(save-place-mode 1)

(global-linum-mode 1)

(setq default-fill-column 80)

(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-zsh)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
;; (global-set-key (kbd "<f6>") 'ansi-term) ;; I use shell-pop now

(when window-system
  (global-prettify-symbols-mode t))

(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-margin 5)

(defvar consoli/backup_dir
  (concat user-emacs-directory "backups"))

(if (not (file-exists-p consoli/backup_dir))
    (make-directory consoli/backup_dir t))

(setq backup-directory-alist
      `(("." . ,consoli/backup_dir)))

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 3)
(setq kept-old-versions 2)
(setq version-control t)

(setq auto-save-default nil)

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M")
(display-time-mode 1)

(global-subword-mode 1)

(require 'paren)
(set-face-foreground 'show-paren-match "#00BFFF")
(set-face-background 'show-paren-match (face-background 'default))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode 1)

(setq kill-ring-max 100)

(setq tls-checktrust t)

;;(set-frame-parameter (selected-frame) 'alpha '(85 80))
;;(add-to-list 'default-frame-alist '(alpha 85 80))

(global-auto-revert-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(windmove-default-keybindings)

(defun consoli/edit-init ()
  "Easy open init.el file."
  (interactive)
  (find-file "~/.emacs.d/config.org")
  (message "Welcome back to configuration file!"))
(global-set-key (kbd "<S-f1>") 'consoli/edit-init)

(defun consoli/kill-whitespaces ()
  (interactive)
  (whitespace-cleanup)
  (message "Whitespaces killed!"))

(global-set-key (kbd "<f9>") 'consoli/kill-whitespaces)

(defun consoli/indent-context ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (set-mark-command nil)
    (end-of-defun)
    (indent-region (region-beginning) (region-end)))
  (message "Indented!"))

(global-set-key (kbd "<f7>") 'consoli/indent-context)

(defun consoli/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Buffer indented!"))

(global-set-key (kbd "<C-f7>") 'consoli/indent-buffer)

(defun consoli/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'consoli/kill-current-buffer)

(defun consoli/reload-config ()
  (interactive)
  (message "Reloading configurations...")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'consoli/reload-config)

(defun consoli/infer-indentation-style ()
  "If our souce file use tabs, we use tabs, if spaces, spaces.
And if neither, we use the current indent-tabs-mode"
  (let ((space-count (how-many "^ " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(add-hook 'prog-mode-hook #'consoli/infer-indentation-style)

(defun consoli/set-buffer-to-unix-format ()
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun consoli/set-buffer-to-unix-format ()
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun consoli/insert-line-bellow ()
  (interactive)
  (let ((current-point (point)))
    (move-end-of-line 1)
    (open-line 1)
    (goto-char current-point)))

(defun consoli/insert-line-above ()
  (interactive)
  (let ((current-point (point)))
    (move-beginning-of-line 1)
    (newline-and-indent)
    (indent-according-to-mode)
    (goto-char current-point)
    (forward-char)))

(global-set-key (kbd "<f10>") 'whitespace-mode)

(global-set-key (kbd "<f12>") 'linum-mode)

(global-set-key (kbd "C-x b") 'ibuffer)

(add-hook 'python-mode-hook 'yas-minor-mode)

(add-hook 'python-mode-hook 'flycheck-mode)

(with-eval-after-load 'company
  (add-hook 'python-mode-hook 'company-mode))
;; take a look at `use-package/company-jedi' for more"

(setq python-shell-interpreter "ipython")

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)

(add-hook 'emacs-lisp-mode-hook 'company-mode)
;; take a look at `use-package/smile' and `use-package/slime-company' for more

(setq org-src-fontfy-natively t)
(setq org-src-tab-acts-natively t)
(setq org-export-with-smart-quotes t)
;;(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook
          '(lambda ()
             (visual-line-mode 1)))

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(add-to-list 'org-structure-template-alist
             '("py" "#+BEGIN_SRC python\n?\n#+END_SRC"))

(diminish 'which-key-mode)
(diminish 'linum-relative-mode)
(diminish 'subword-mode)
(diminish 'rainbow-delimiters-mode)
(diminish 'rainbow-mode)
(diminish 'helm-mode)
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(diminish 'org-indent-mode)
(diminish 'whitespace-mode)
(diminish 'eldoc-mode)
(diminish 'yas-minor-mode)
(diminish 'company-mode)
(diminish 'page-break-lines-mode)
(diminish 'highlight-indentation-mode)
(diminish 'smartparens-mode)
