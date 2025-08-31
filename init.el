;;; init.el --- Personal Emacs Configuration -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BOOTSTRAP & PACKAGE MANAGEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; performance optimization during startup
(setq-default garbage-collection-messages nil)

;; UTF-8 encoding setup
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)

(setq-default package-enable-at-startup nil)

(setq-default straight-cache-autoloads t
              straight-use-package-by-default t
              straight-vc-git-default-clone-depth 1)

(straight-use-package 'use-package)
(setq use-package-compute-statistics nil)

(straight-use-package
 '(org
   :type git
   :host github
   :repo "emacsmirror/org"
   :files ("lisp/*.el" "contrib/lisp/*.el")))

;; Compile Elisp files automatically for better performance
;; (use-package compile-angel
;;   :demand t
;;   :custom
;;   (compile-angel-verbose nil)
;;   :config
;;   (setq compile-angel-excluded-files
;;         (append '("/init.el"
;;                   "/early-init.el"
;;                   "/.mc-lists.el" ;; multiple-cursors
;;                   "/my-themes/tale-themes-common.el"
;;                   "/my-themes/dark-tale-themes.el"
;;                   "/my-themes/bright-tale-themes.el")
;;                 compile-angel-excluded-files))
;;   (compile-angel-on-load-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CORE EMACS SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuration constants
(defconst consoli-config/completion-delay 0.2
  "Default delay for completion systems.")

(defconst consoli-config/idle-timer-delay 3
  "Default idle timer delay in seconds.")

(defun shut-up--advice (fn &rest args)
  "Inhibit message from the provided FN."
  (let ((inhibit-message t)
        (message-log-max))
    (apply fn args)))

;; Core Emacs configuration
(use-package emacs
  :hook (after-init . consoli-config/setup-core-modes)
  :custom
  ;; Completion settings
  (initial-major-mode 'org-mode)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (context-menu-mode t)
  (enable-recursive-minibuffers nil)
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Minibuffer settings
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Window divider settings
  (window-divider-default-places t)
  (window-divider-default-bottom-width 0)
  (window-divider-default-right-width 20)

  (treesit-font-lock-level 4)

  :init
  ;; Load paths
  (add-to-list 'load-path (expand-file-name "my-themes" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  ;; Helper functions
  (defun consoli-config/setup-core-modes ()
    "Initialize core Emacs modes."
    (global-completion-preview-mode)
    (repeat-mode)
    (xterm-mouse-mode t)
    (window-divider-mode t)
    (pending-delete-mode t)
    (delete-selection-mode t)
    (global-auto-composition-mode)
    (global-auto-revert-mode))

  (defun crm-indicator (args)
    "Add CRM indicator to completing-read-multiple."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

  :config
  (when (daemonp)
    (global-set-key (kbd "C-x C-c") #'delete-frame))
  (advice-add #'repeat-mode :around #'shut-up--advice)
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package helpful
  :bind
  ((("C-h f" . helpful-callable)
    ("C-h v" . helpful-variable)
    ("C-h k" . helpful-key)
    ("C-h x" . helpful-command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMPLETION FRAMEWORK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico - vertical completion UI
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :hook (after-init . consoli-config/setup-vertico)
  :custom
  (vertico-cycle t)
  (vertico-count 6)
  :config
  (require 'vertico-quick)

  (defun consoli-config/setup-vertico ()
    "Initialize Vertico completion system."
    (vertico-mode)
    (vertico-multiform-mode)))

;; Vertico posframe for popup completion
(use-package vertico-posframe
  :after vertico
  :custom
  (vertico-posframe-poshandler 'consoli-config/vertico-posframe-conditional-echo-poshandler)
  (vertico-posframe-border-width 10)
  (vertico-posframe-truncate-lines t)
  :config
  (defun consoli-config/vertico-posframe-conditional-echo-poshandler (info)
    "Position posframe over echo area only when echo area is directly below the current window."
    (let* ((window (plist-get info :parent-window))
           (frame (window-frame window))
           (frame-pixel-height  (frame-pixel-height frame))
           (window-edges (window-pixel-edges window))
           (window-bottom (nth 3 window-edges))
           (window-left (nth 0 window-edges))
           (window-right (nth 2 window-edges))
           (echo-area-height (window-pixel-height (minibuffer-window frame)))
           (echo-area-top (- frame-pixel-height echo-area-height))
           ;; Check if echo area is directly below the window (within a few pixels tolerance)
           (echo-below-window-p (<= (abs (- window-bottom echo-area-top)) 5))
           (posframe-width (plist-get info :posframe-width)))

      (if echo-below-window-p
          ;; Position over echo area when it's below the window
          (cons (+ window-left (/ (- (- window-right window-left) posframe-width) 2))
                (- frame-pixel-height (+ 10 (* echo-area-height (+ 2 vertico-count)))))
        ;; Fallback to normal bottom-center positioning
        (posframe-poshandler-window-bottom-center info))))

  (defun consoli-config/vertico-posframe-size (buffer)
    "Set posframe width to match current window, with fixed height."
    (let* ((window (selected-window))
           (window-edges (window-pixel-edges window))
           (window-width (- (nth 2 window-edges) (nth 0 window-edges) (* 2 vertico-posframe-border-width)))
           (char-width (frame-char-width))
           (width-chars (/ window-width char-width))
           (fixed-height (+ 2 vertico-count)))
      (list
       :height fixed-height
       :width (round width-chars)
       :min-height fixed-height
       :min-width (round width-chars))))

  ;; Store original mode-line format per window
  (defvar consoli-config/original-mode-line-alist nil
    "Alist storing original mode-line format for each window.")

  (defun consoli-config/hide-mode-line-for-vertico ()
    "Hide mode-line in the main window when vertico opens."
    (let ((main-window (minibuffer-selected-window)))
      (when (and main-window (window-live-p main-window))
        (with-selected-window main-window
          (unless (assq main-window consoli-config/original-mode-line-alist)
            (push (cons main-window mode-line-format) consoli-config/original-mode-line-alist)
            (setq mode-line-format nil)
            (force-mode-line-update))))))

  (defun consoli-config/restore-mode-line-for-vertico ()
    "Restore mode-line in the main window when vertico closes."
    (let ((main-window (minibuffer-selected-window)))
      (when (and main-window (window-live-p main-window))
        (let ((original-format (assq main-window consoli-config/original-mode-line-alist)))
          (when original-format
            (with-selected-window main-window
              (setq mode-line-format (cdr original-format))
              (force-mode-line-update))
            (setq consoli-config/original-mode-line-alist
                  (delq original-format consoli-config/original-mode-line-alist)))))))

  (defun consoli-config/refresh-vertico-posframe ()
    "Refresh vertico-posframe to fix potential display bugs."
    (when (and (featurep 'vertico-posframe) vertico-posframe-mode)
      (when (and vertico-posframe--buffer (buffer-live-p vertico-posframe--buffer))
        (posframe-refresh vertico-posframe--buffer))))

  (add-hook 'vertico-posframe-mode-hook
            (lambda ()
              (if vertico-posframe-mode
                  (progn
                    (add-hook 'minibuffer-setup-hook #'consoli-config/hide-mode-line-for-vertico)
                    (add-hook 'minibuffer-exit-hook #'consoli-config/restore-mode-line-for-vertico)
                    (add-hook 'minibuffer-setup-hook #'consoli-config/refresh-vertico-posframe))
                (progn
                  (remove-hook 'minibuffer-setup-hook #'consoli-config/hide-mode-line-for-vertico)
                  (remove-hook 'minibuffer-exit-hook #'consoli-config/restore-mode-line-for-vertico)
                  (remove-hook 'minibuffer-setup-hook #'consoli-config/refresh-vertico-posframe)))))

  (setq vertico-posframe-size-function #'consoli-config/vertico-posframe-size)
  (vertico-posframe-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DEVELOPMENT TOOLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package apheleia
  :defer t)

(defun consoli-config/eglot-mode-if-available ()
  "Start eglot if a server is available for this mode."
  (require 'eglot)
  (when-let* ((lookup-result (eglot--lookup-mode major-mode)))
    (when (cdr lookup-result)
      (eglot-ensure))))

;; Eglot - LSP client
(use-package eglot
  :defer t
  :init
  (defvar eglot-mode-map (make-sparse-keymap)
    "Keymap for eglot commands.")
  :custom
  ;; Connection settings
  (eglot-autoshutdown t)
  (eglot-sync-connect-timeout 30)
  (eglot-events-buffer-size 0)
  (eglot-connect-timeout 30)
  (eglot-send-changes-idle-time 0.3)

  ;; Eldoc settings
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil)

  :config
  ;; Server programs
  (add-to-list 'eglot-server-programs
               '(toml-ts-mode . '("taplo" "lsp" "stdio")))

  (defun format-on-eglot ()
    (when (and (fboundp 'eglot-current-server)
               (eglot-current-server))
      (eglot-format-buffer)))

  (add-hook 'before-save-hook #'format-on-eglot nil t)

  ;; Completion integration
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

  ;; Helper functions
  (defun consoli-config/eglot-orderless-dispatch (_pattern index _total)
    "Set `orderless-flex` for the first component of a pattern."
    (and (eq index 0) 'orderless-flex))

  (defun consoli-config/eglot-setup-completion ()
    "Configure completion for Eglot-managed buffers."
    (setf (alist-get 'styles (alist-get 'lsp completion-category-defaults))
          '(orderless))
    (add-hook 'orderless-style-dispatchers #'consoli-config/eglot-orderless-dispatch nil 'local)
    (setq-local completion-at-point-functions
                (list (cape-capf-buster #'eglot-completion-at-point))))

  (defun consoli-config/eglot-auto-save-after-edit (&rest _)
    "Auto-save buffers after workspace edit."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name (buffer-modified-p))
          (save-buffer)))))

  (advice-add 'eglot--apply-workspace-edit :after #'consoli-config/eglot-auto-save-after-edit)

  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l t" . eglot-find-type-definition)
              ("C-c l v" . consult-eglot-symbols)
              ("<f4>" . eglot-inlay-hints-mode))

  :hook ((prog-mode . consoli-config/eglot-mode-if-available)
         (eglot-managed-mode . consoli-config/eglot-setup-completion)
         (eglot-managed-mode . eglot-inlay-hints-mode)))

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :hook (eglot-managed . eglot-booster-mode))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-c l s" . consult-eglot-symbols)
              ("C-c l i" . consult-imenu)
              ("C-c l e" . consult-flycheck)))

(use-package eldoc-box
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-cleanup-internval 0.3)
  (eldoc-idle-delay 1.5)
  :config
  (defun consoli-config/eldoc-box-dynamic-sizing ()
    "Dynamically set eldoc-box dimensions and font size based on window size."
    (let* ((window (selected-window))
           (window-width (window-pixel-width window))
           (window-height (window-pixel-height window))
           (screen-width (display-pixel-width))
           (screen-height (display-pixel-height))
           ;; Calculate proportional dimensions (50% width, 30% height max)
           (box-width (min (truncate (* window-width 0.5)) (truncate (* screen-width 0.5))))
           (box-height (min (truncate (* window-height 0.3)) (truncate (* screen-height 0.3))))
           ;; Calculate font size based on window size (scale between 0.75 and programming font size)
           (max-scale (/ (consoli-config/font-height-programming) 100.0))
           (font-scale (+ 0.75 (* (/ (float window-width) (float screen-width)) (- max-scale 0.75)))))

      ;; Set dynamic dimensions
      (setq-local eldoc-box-max-pixel-width box-width
                  eldoc-box-max-pixel-height box-height
                  default-text-properties '(line-spacing 0 line-height 1))

      ;; Apply font parameters to eldoc-box faces
      (set-face-attribute 'eldoc-box-body nil
                          :family programming-font
                          :height font-scale)
      (set-face-attribute 'eldoc-box-border nil :height font-scale)))

  (defun consoli-config/eldoc-box-bottom-right-position (width height)
    "Position eldoc-box at bottom-right, accounting for frame size, fringes, and modeline."
    (let* ((window (selected-window))
           (frame (window-frame window))
           (window-edges (window-pixel-edges window))
           (window-right (nth 2 window-edges))
           (window-bottom (nth 3 window-edges))
           (frame-width (frame-pixel-width frame))
           (frame-height (frame-pixel-height frame))
           (right-fringe (or (cadr (window-fringes window)) 0))
           (modeline-height (window-mode-line-height window))
           ;; Calculate margins as percentage of frame size with minimums
           (margin-x (max 20 (+ right-fringe (/ frame-width 40))))
           (margin-y (max 15 (+ (or modeline-height 0) (/ frame-height 50)))))
      (cons (- window-right width margin-x)
            (- window-bottom height margin-y))))

  ;; Apply dynamic sizing before showing eldoc-box
  (advice-add 'eldoc-box--display :before
              (lambda (&rest _) (consoli-config/eldoc-box-dynamic-sizing)))

  (setq eldoc-box-position-function #'consoli-config/eldoc-box-bottom-right-position)

  ;; some margin for the docs
  (setf (alist-get 'internal-border-width eldoc-box-frame-parameters) 5)
  :bind (:map eglot-mode-map
              ("C-c l d" . eldoc-box-help-at-point))
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(use-package breadcrumb
  :hook (eglot-managed-mode . breadcrumb-mode)
  :custom
  (breadcrumb-project-max-length 0.5)
  (breadcrumb-project-crumb-separator "󠀠󠀠󠀠󠀠󠀠󠀠󠀠󠀠") ;; space, nf-fae-slash, TAG SPACE
  (breadcrumb-imenu-max-length 0.5))

(use-package breadcrumb-icons
  :straight (breadcrumb-icons :type built-in)
  :hook (breadcrumb-mode . breadcrumb-icons-mode)
  :custom
  (breadcrumb-icons-icon-string "󠀠") ;; M-x insert-icon TAG SPACE
  (breadcrumb-icons-icon-spacing 2)
  :load-path "~/.emacs.d/lisp")

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :after eglot
  :custom
  (eglot-x-enable-colored-diagnostics t)
  (eglot-x-enable-server-status t)
  (eglot-x-enable-snippet-text-edit t)
  :bind
  (:map eglot-mode-map
        ("M-S-." . eglot-x-find-refs)
        ("C-c l R" . eglot-x-ask-runnables)
        ("C-c l w" . eglot-x-find-workspace-symbol)
        ("C-c l o" . eglot-x-open-external-documentation)
        ("C-c l s" . eglot-x-structural-search-replace))
  :hook (eglot-managed-mode . eglot-x-setup))

(use-package eglot-cthier
  :straight (:host codeberg :repo "harald/eglot-supplements" :files ("eglot-cthier.el" "toggletree.el"))
  :after eglot
  :bind (:map eglot-mode-map
              ("C-c l h i" . eglot-cthier-request-incoming-call-hierarchy)
              ("C-c l h o" . eglot-cthier-request-outgoing-call-hierarchy)
              ("C-c l h S" . eglot-cthier-request-supertype-call-hierarchy)
              ("C-c l h s" . eglot-cthier-request-subtype-call-hierarchy)))

(use-package eglot-semtok
  :straight (:host codeberg :repo "harald/eglot-supplements" :files ("eglot-semtok.el"))
  :hook
  (eglot-connect-hook . eglot-semtok-on-connected)
  (rust-ts-mode . eglot-semtok-font-lock-init))

(use-package sideline-eglot
  :defer t
  :custom
  (sideline-eglot-code-actions-prefix ""))

(use-package sideline-flycheck
  :defer t
  :hook (flycheck-mode . sideline-flycheck-setup))

(use-package sideline
  :defer t
  :hook (eglot-managed-mode . sideline-mode)
  :custom-face
  (slideline-default ((t (:height 100))))
  :custom
  (sideline-backends-right '(sideline-eglot sideline-flycheck))
  (sideline-display-backend-name t)
  (sideline-delay 0.4)
  (sideline-truncate t))

(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(extended-command-history))
  (history-length 1000)
  (history-delete-duplicates t))

;; Orderless - flexible completion matching
(use-package orderless
  :custom
  (completion-styles '(orderless substring basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp))
  :config
  (defun consoli-config/orderless-dispatch (word _index _total)
    "Dispatcher for special orderless syntax."
    (cond
     ((string-prefix-p "$" word) `(orderless-regexp . ,(concat (substring word 1) "\\'")))
     ((and (string-suffix-p "." word) (> (length word) 1))
      `(orderless-regexp . ,(concat "\\." (substring word 0 -1) "\\'")))
     ((equal "!" word) `(orderless-literal . ""))
     ((string-prefix-p "%" word) `(char-fold-to-regexp . ,(substring word 1)))
     ((string-prefix-p "!" word) `(orderless-without-literal . ,(substring word 1)))
     ((string-prefix-p "`" word) `(orderless-initialism . ,(substring word 1)))
     ((string-prefix-p "=" word) `(orderless-literal . ,(substring word 1)))
     ((string-prefix-p "~" word) `(orderless-flex . ,(substring word 1)))))

  (setq orderless-style-dispatchers '(consoli-config/orderless-dispatch
                                      orderless-affix-dispatch))

  (defun consoli-config/completion-sort-by-history (completions)
    "Sort COMPLETIONS by putting recently used items first."
    (let ((hist (symbol-value minibuffer-history-variable)))
      (sort completions
            (lambda (a b)
              (let ((pos-a (cl-position a hist :test #'string=))
                    (pos-b (cl-position b hist :test #'string=)))
                (cond
                 ((and pos-a pos-b) (< pos-a pos-b))
                 (pos-a t)
                 (pos-b nil)
                 (t (string< a b))))))))

  ;; Apply history-based sorting to commands
  (setq completion-category-overrides
        (append completion-category-overrides
                '((command (styles orderless)
                           (cycle . t)
                           (sort . consoli-config/completion-sort-by-history))))))

;; Corfu - completion overlay
(use-package corfu
  :hook (after-init . consoli-config/setup-corfu)
  :custom
  (corfu-auto t)
  (corfu-auto-delay consoli-config/completion-delay)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-on-exact-match 'insert)
  (corfu-popupinfo-delay '(1.30 . 1.50))
  (corfu-popupinfo-max-height 15)
  (corfu-popupinfo-max-width 80)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 3)
  (corfu-separator ?\s)
  :bind
  (:map corfu-map
        ("M-q" . corfu-quick-complete)
        ("C-i" . corfu-quick-insert))
  :config
  (defun consoli-config/setup-corfu ()
    "Initialize Corfu completion system."
    (global-corfu-mode)
    (corfu-history-mode)
    (corfu-popupinfo-mode))

  (defun consoli-config/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'consoli-config/corfu-enable-in-minibuffer))

(defun consoli-config/set-custom-text-properties ()
  "Modern line spacing for current buffer."
  (setq-local default-text-properties '(line-spacing 0.15 line-height 1.15)))

(dolist (hook '(text-mode-hook
                git-commit-mode-hook))
  (add-hook hook #'consoli-config/set-custom-text-properties))

;; Nerd icons for Corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Cape - completion at point extensions
(use-package cape
  :custom
  (cape-dabbrev-min-length 3)
  (cape-dabbrev-check-other-buffers t)
  (cape-file-directory-must-exist t)
  :bind ("M-p" . cape-prefix-map)
  :config
  (defun consoli-config/cape-setup-prog ()
    "Setup completion-at-point for programming modes."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-elisp-block
                       #'cape-dabbrev
                       #'cape-keyword
                       #'cape-file))
                cape-dabbrev-min-length 3))

  (defun consoli-config/cape-setup-text ()
    "Setup completion-at-point for text modes."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-dabbrev
                       #'cape-dict
                       #'cape-file))
                cape-dabbrev-min-length 3))

  (add-hook 'text-mode-hook #'consoli-config/cape-setup-text)

  (defun consoli-config/prog-mode-setup ()
    "Complete setup for prog-mode."
    ;; Set custom text properties (line spacing, etc)
    (consoli-config/set-custom-text-properties)
    ;; Setup completion at point
    (consoli-config/cape-setup-prog)
    ;; Set programming font
    (face-remap-add-relative 'default
                             :family programming-font
                             :height (consoli-config/font-height-programming))
    ;; Infer indentation style
    (consoli-config/infer-indentation-style))

  (add-hook 'prog-mode-hook #'consoli-config/prog-mode-setup)

  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword))

;; Dabbrev - dynamic abbreviation expansion
(setq-default abbrev-mode t)
(use-package dabbrev
  :defer t
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Marginalia - completion annotations
(use-package marginalia
  :custom
  (marginalia-align 'right)
  (marginalia-align-offset 1)
  :hook (after-init . marginalia-mode))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.25
        which-key-max-description-length 45
        which-key-idle-secondary-delay 0.05
        which-key-show-remaining-keys t)
  :init (which-key-mode)
  ;; config for which-key output for repeat-mode maps
  ;; taken from https://gist.github.com/karthink/9f054dc8fba07fd117738bec31652a90
  ;; (slightly modified)
  :preface
  (setq consoli-config/which-key-last-timer nil)
  (defun consoli-config/which-key-reset-last-timer ()
    ;; `consoli-config/which-key-last-timer' holds the timer returned by `run-with-idle-timer'
    ;; it is set on every key pressed while on repeat-mode, and reset to nil on both entry and exit
    ;; of repeat-mode through `repeat-mode-hook'
    (setq consoli-config/which-key-last-timer nil))

  :hook ((repeat-mode . consoli-config/which-key-reset-last-timer))
  :config
  (defun consoli-config/which-key-repeat ()
    (unless (null consoli-config/which-key-last-timer)
      (cancel-timer consoli-config/which-key-last-timer))
    (when-let* ((cmd (or this-command real-this-command))
                (keymap1 (repeat--command-property 'repeat-mode-map)))
      (run-with-idle-timer
       which-key-idle-delay nil
       (lambda ()
         (which-key--create-buffer-and-show
          nil (symbol-value keymap1))))))

  (defun consoli-config/which-key-repeat-mode-dispatch ()
    (interactive)
    (setq this-command last-command)
    (when-let* (keymap2 (repeat--command-property 'repeat-map))
      (which-key--create-buffer-and-show
       nil (symbol-value keymap2))))

  (defun consoli-config/which-key-repeat-mode-binding ()
    (when repeat-mode
      (when-let* ((rep-map-sym (or repeat-map (repeat--command-property 'repeat-map)))
                  (keymap3 (and (symbolp rep-map-sym) (symbol-value rep-map-sym))))
        (set-transient-map
         (make-composed-keymap
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-h") #'consoli-config/which-key-repeat-mode-dispatch)
            map)
          keymap3)))))

  ;; for some odd reason `repeat-post-hook' and `repeat-pre-hook' are functions instead
  ;; of variables so functions on hook must be added through `advice-add'
  (advice-add 'repeat-post-hook :after #'consoli-config/which-key-repeat)
  (advice-add 'repeat-post-hook :after #'consoli-config/which-key-repeat-mode-binding)
  :bind ("C-c c w" . which-key-show-major-mode))

(use-package avy
  :bind
  ("C-:" . avy-goto-char-timer)
  ("C-c a l" . avy-goto-line)
  ("C-c a w" . avy-goto-subword-0)
  ("C-c a d" . avy-goto-word-0)
  ("C-c a s" . avy-goto-symbol-1))

(use-package avy-zap
  :defer t
  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("M-Z" . avy-zap-up-to-char-dwim))

(use-package treesit-jump
  :defer t
  :straight (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  :if (treesit-available-p)
  :init
  (defvar treesit-jump-map (make-sparse-keymap)
    "Keymap for treesit-jump commands.")
  :bind-keymap ("C-," . treesit-jump-map)
  :bind (:map treesit-jump-map
              ("j" . treesit-jump-jump)
              ("s" . treesit-jump-select)
              ("d" . treesit-jump-delete)
              ("p" . treesit-jump-parent-jump)
              ("t" . treesit-jump-transient)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;;("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."

  (when (bound-and-true-p which-key-mode)
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding)))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(use-package yasnippet
  :defer t
  :hook (prog-mode . yas-minor-mode)
  :config
  (advice-add #'yas-reload-all :around #'shut-up--advice)
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

(use-package flycheck-inline
  :defer t
  :after flycheck-mode)

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package consult-flycheck
  :defer t
  :after (flycheck consult))

(use-package consult-yasnippet
  :defer t
  :after (yasnippet consult))

(use-package vterm
  :commands (vterm vterm-other-window)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-kill-buffer-on-exit t)
  (vterm-copy-exclude-prompt t)
  :config
  (defun consoli-config/vterm-setup ()
    "Complete setup for vterm mode."
    ;; Disable some modes that don't work well with vterm
    (toggle-truncate-lines)
    (hl-line-mode -1)
    ;; Set font
    (face-remap-add-relative 'default
                             :family programming-font
                             :height (consoli-config/font-height-programming))
    ;; Hide modeline
    (consoli-config/hide-modeline))

  (add-hook 'vterm-mode-hook #'consoli-config/vterm-setup)

  (defvar consoli-config/project-terminals (make-hash-table :test 'equal)
    "Hash table mapping project roots to lists of terminal buffers.")

  (defvar consoli-config/terminal-window nil
    "Window used for displaying terminals.")

  (defvar consoli-config/terminal-height 15
    "Height of terminal window in lines.")

  (defun consoli-config/get-project-root ()
    "Get current project root, compatible with tabspaces."
    (if-let* ((project (project-current)))
        (project-root project)
      default-directory))

  (defun consoli-config/project-terminal-name (project-root index)
    "Generate terminal buffer name for PROJECT-ROOT and INDEX."
    (let ((project-name (if-let* ((project (project-current)))
                            (project-name project)
                          (file-name-nondirectory (string-remove-suffix "/" project-root)))))
      (format "*vterm: %s-%d*" project-name index)))

  (defun consoli-config/get-project-terminals (project-root)
    "Get list of live terminal buffers for PROJECT-ROOT."
    (seq-filter #'buffer-live-p
                (gethash project-root consoli-config/project-terminals '())))

  (defun consoli-config/add-project-terminal (project-root buffer)
    "Add BUFFER to PROJECT-ROOT's terminal list."
    (let ((terminals (consoli-config/get-project-terminals project-root)))
      (puthash project-root (cons buffer terminals) consoli-config/project-terminals)))

  (defun consoli-config/remove-project-terminal (project-root buffer)
    "Remove BUFFER from PROJECT-ROOT's terminal list."
    (let ((terminals (consoli-config/get-project-terminals project-root)))
      (puthash project-root (remove buffer terminals) consoli-config/project-terminals)))

  (defun consoli-config/cleanup-dead-terminals ()
    "Remove dead buffers from project terminals tracking."
    (maphash (lambda (project-root terminals)
               (let ((live-terminals (seq-filter #'buffer-live-p terminals)))
                 (if live-terminals
                     (puthash project-root live-terminals consoli-config/project-terminals)
                   (remhash project-root consoli-config/project-terminals))))
             consoli-config/project-terminals))

  (defun consoli-config/is-terminal-for-current-project-p (buffer)
    "Check if BUFFER is a terminal buffer for the current project."
    (when (and (string-prefix-p "*vterm:" (buffer-name buffer))
               (buffer-live-p buffer))
      (let* ((current-project-root (consoli-config/get-project-root))
             (project-terminals (consoli-config/get-project-terminals current-project-root)))
        (memq buffer project-terminals))))

  ;; Terminal Window Management
  (defun consoli-config/create-terminal-window ()
    "Create a window at the bottom for terminals."
    (let* ((root-window (frame-root-window))
           (window (split-window root-window (- consoli-config/terminal-height) 'below)))
      (setq consoli-config/terminal-window window)
      ;; Configure window parameters for better integration
      (set-window-parameter window 'no-other-window t)
      (set-window-parameter window 'no-delete-other-windows t)
      window))

  (defun consoli-config/get-or-create-terminal-window ()
    "Get existing terminal window or create new one."
    (if (and consoli-config/terminal-window
             (window-live-p consoli-config/terminal-window))
        consoli-config/terminal-window
      (consoli-config/create-terminal-window)))

  (defun consoli-config/hide-terminal-window ()
    "Hide the terminal window."
    (when (and consoli-config/terminal-window
               (window-live-p consoli-config/terminal-window))
      (delete-window consoli-config/terminal-window)
      (setq consoli-config/terminal-window nil)))

  (defun consoli-config/terminal-window-visible-p ()
    "Check if terminal window is currently visible."
    (and consoli-config/terminal-window
         (window-live-p consoli-config/terminal-window)))

  (defun consoli-config/focus-terminal-window ()
    "Focus the terminal window if visible."
    (when (consoli-config/terminal-window-visible-p)
      (select-window consoli-config/terminal-window)))

  (defun consoli-config/new-project-terminal ()
    "Create a new terminal for the current project."
    (interactive)
    (consoli-config/cleanup-dead-terminals)
    (let* ((project-root (consoli-config/get-project-root))
           (terminals (consoli-config/get-project-terminals project-root))
           (index (1+ (length terminals)))
           (buffer-name (consoli-config/project-terminal-name project-root index))
           (default-directory project-root))

      (let ((terminal-window (consoli-config/get-or-create-terminal-window)))
        (with-selected-window terminal-window
          (let ((buffer (vterm buffer-name)))
            (consoli-config/add-project-terminal project-root buffer)
            ;; Refresh centaur-tabs to show the new terminal
            (when (bound-and-true-p centaur-tabs-mode)
              (centaur-tabs-display-update))
            buffer)))))

  (defun consoli-config/switch-project-terminal ()
    "Switch between terminals in the current project."
    (interactive)
    (consoli-config/cleanup-dead-terminals)
    (let* ((project-root (consoli-config/get-project-root))
           (terminals (consoli-config/get-project-terminals project-root)))

      (cond
       ((null terminals)
        (consoli-config/new-project-terminal))

       ((= 1 (length terminals))
        (let ((terminal-window (consoli-config/get-or-create-terminal-window)))
          (with-selected-window terminal-window
            (switch-to-buffer (car terminals)))))

       (t
        (let* ((terminal-names (mapcar #'buffer-name terminals))
               (choice (completing-read "Switch to terminal: " terminal-names nil t)))
          (let ((terminal-window (consoli-config/get-or-create-terminal-window)))
            (with-selected-window terminal-window
              (switch-to-buffer choice))))))))

  (defun consoli-config/toggle-project-terminal ()
    "Toggle terminal visibility for current project."
    (interactive)
    (if (consoli-config/terminal-window-visible-p)
        (consoli-config/hide-terminal-window)
      (progn
        (consoli-config/switch-project-terminal)
        (consoli-config/focus-terminal-window))))

  (defun consoli-config/kill-project-terminals ()
    "Kill all terminals for the current project."
    (interactive)
    (let* ((project-root (consoli-config/get-project-root))
           (terminals (consoli-config/get-project-terminals project-root)))
      (when terminals
        (dolist (terminal terminals)
          (when (buffer-live-p terminal)
            (kill-buffer terminal)))
        (remhash project-root consoli-config/project-terminals)
        (when (consoli-config/terminal-window-visible-p)
          (consoli-config/hide-terminal-window))
        (message "Killed %d terminal(s) for project" (length terminals)))))

  ;; Terminal lifecycle management
  (defun consoli-config/cleanup-terminal-on-kill ()
    "Clean up terminal tracking when buffer is killed."
    (when (string-match-p "^\\*vterm:" (buffer-name))
      (let ((project-root (consoli-config/get-project-root)))
        (consoli-config/remove-project-terminal project-root (current-buffer)))))

  (add-hook 'kill-buffer-hook #'consoli-config/cleanup-terminal-on-kill)

  ;; Hide terminal when switching projects
  (with-eval-after-load 'project
    (add-hook 'project-switch-hook
              (lambda ()
                (consoli-config/hide-terminal-window)
                (consoli-config/refresh-centaur-tabs)))))

;; Integration with your existing mode-line hiding
;; (Note: consoli-config/hide-modeline is already added via the dolist below)

(defun consoli-config/refresh-centaur-tabs ()
  "Refresh centaur-tabs display."
  (when (bound-and-true-p centaur-tabs-mode)
    (centaur-tabs-display-update)))

;; Refresh tabs when switching terminals
(advice-add 'consoli-config/switch-project-terminal :after #'consoli-config/refresh-centaur-tabs)

;; Refresh tabs when killing terminals
(advice-add 'consoli-config/kill-project-terminals :after #'consoli-config/refresh-centaur-tabs)

(use-package consult-org-roam
  :defer t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; use `ripgrep` for searching with `consult-org-roam-search`
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; custom narrow key for `consult-buffer`
  (consult-org-roam-narrow-key ?r)
  ;; display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-.")
  :bind
  ;; define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n B" . consult-org-roam-backlinks-recursive)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(defun consoli-config/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(use-package copilot
  :defer t
  :straight (:host github :repo "copilot-emacs/copilot.el")
  :custom
  (copilot-idle-delay 30)
  :config
  (add-to-list 'copilot-major-mode-alist '("rust-ts-mode" . "rust"))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  :bind
  (:map prog-mode-map
        ("C-c C-o RET" . global-copilot-mode))
  (:map copilot-mode-map
        ("<TAB>" . consoli-config/copilot-tab)
        ("C-c C-o n" . copilot-next-completion)
        ("C-c C-o SPC" . copilot-complete)
        ("C-c C-o p" . copilot-previous-completion)
        ("C-c C-o a" . copilot-accept-completion)
        ("C-c C-o C-a l" . copilot-accept-completion-by-line)
        ("C-c C-o C-a w" . copilot-accept-completion-by-word)))

(use-package aidermacs
  :bind (("C-;" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" (shell-command-to-string "echo -n $ANTHROPIC_API_KEY"))
  (setenv "OPENROUTER_API_KEY" (shell-command-to-string "echo -n $OPENROUTER_API_KEY"))
  :custom
  (aidermacs-auto-commits nil)
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-backend 'vterm)
  ;; watch for comments ending with `AI` (vterm only)
  (aidermacs-watch-files t)
  (aidermacs-vterm-use-theme-colors t)
  (aidermacs-show-diff-after-change nil)
  (aidermacs-default-model "anthropic/claude-sonnet-4-20250514")
  (aidermacs-architect-model "anthropic/claude-opus-4-20250514")
  ;; (aidermacs-extra-args '("--reasoning-effort high"))
  (aidermacs-global-read-only-files '("~/Projects/templates/agents/rust-pro.md" "~/Projects/templates/agents/backend-architect.md"))
  (aidermacs-project-read-only-files '("CONVENTIONS.md" "README.md")))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-d" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'vterm)
  :config
  (claude-code-ide-emacs-tools-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THEME CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :defer t)

(use-package nerd-icons
  :defer t)

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package all-the-icons-nerd-fonts
  :defer t
  :straight (:host github :repo "mohkale/all-the-icons-nerd-fonts")
  :after all-the-icons
  :init
  (all-the-icons-nerd-fonts-prefer))

(use-package doom-themes
  :defer t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-treemacs-theme "doom-colors")
  (doom-themes-enable-italic t)
  :init
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package kaolin-themes
  :defer t
  :custom
  (kaolin-themes-bold t)
  (kaolin-themes-italic t)
  (kaolin-themes-italic-comments t)
  (kaolin-themes-distinct-parentheses t)
  (kaolin-themes-distinct-fringe nil)
  (kaolin-themes-comments-style 'alt)
  (kaolin-themes-hl-line-colored t)
  (kaolin-themes-underline t)
  :defer t)

(use-package spacemacs-theme
  :defer t
  :custom
  (spacemacs-theme-comment-bg nil)
  (spacemacs-theme-keyword-italic t)
  (spacemacs-theme-org-bold t)
  (spacemacs-theme-comment-italic t))

;; Theme configuration
(defvar consoli-themes
  '((gui . witch-tale)
    (cli . dark-tale))
  "Theme configuration for different display types.")

(defun consoli-config/apply-theme ()
  "Apply appropriate theme based on display type."
  (condition-case err
      (if (display-graphic-p)
          (load-theme (alist-get 'gui consoli-themes) t)
        (load-theme (alist-get 'cli consoli-themes) t))
    (error (message "Failed to load theme: %s" err))))

;; Initialize theme system
(setopt custom-theme-directory (expand-file-name "my-themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path custom-theme-directory)

;; Apply theme on startup and new frames
(consoli-config/apply-theme)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (consoli-config/apply-theme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FONT & FACE CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default UI font
(set-face-attribute 'default nil
                    :family ui-font
                    :height (consoli-config/font-height-ui))

;; Smaller font for transient menus
(add-hook 'transient-setup-buffer-hook
          (lambda ()
            (face-remap-add-relative 'default
                                     :family ui-font
                                     :height (consoli-config/font-height-small))))

;; Mode line fonts
(set-face-attribute 'mode-line nil
                    :family modeline-font
                    :height (consoli-config/font-height-modeline))

(set-face-attribute 'mode-line-active nil
                    :family modeline-font
                    :height (consoli-config/font-height-modeline))

(set-face-attribute 'mode-line-inactive nil
                    :family modeline-font
                    :height (consoli-config/font-height-modeline))

;; Programming-specific face customizations
(set-face-attribute 'font-lock-comment-face nil
                    :family programming-font
                    :slant 'italic
                    :height (consoli-config/font-height-programming))

;; Tab bar font
(set-face-attribute 'tab-bar nil
                    :family alternative-programming-font
                    :height (consoli-config/font-height-tab-bar))

;; Region selection
(set-face-attribute 'region nil :extend nil)

;; Frame parameters for better appearance
(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))

(setq-default overflow-newline-into-fringe nil)

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :hook (after-init . global-ligature-mode)
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures
   'prog-mode
   '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
     "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
     "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
     "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
     "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
     "<:" ":<" ":>" ">:" "***" ";;" "/==" ".=" ".-" "__" ;; "<>"
     "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
     "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
     "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
     "&=")))

(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; USER INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup and UI cleanup
(setopt inhibit-startup-screen t
        initial-scratch-message nil)
(eval '(setq inhibit-startup-echo-area-message "consoli"))

;; Disable UI elements
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Cursor configuration
(setq-default cursor-type '(bar . 1)
              blink-cursor-delay 5
              blink-cursor-interval 0.75
              cursor-in-non-selected-windows '(hbar . 1))

(defun consoli-config/childframe-cursor-setup (frame)
  "Adjust cursor in childframes."
  (when (frame-parameter frame 'parent-frame) ;; it’s a childframe
    (with-selected-frame frame
      (setq-local cursor-in-non-selected-windows nil))))
(add-hook 'after-make-frame-functions #'consoli-config/childframe-cursor-setup)


(use-package hl-line
  :ensure nil
  :custom
  (hl-line-overlay-priority 999999)
  :config

  (defcustom consoli-config/show-hl-line-after-secs 3
    "Show the hl-line after N seconds of idle time."
    :type 'number
    :group 'hl-line)

  (defvar consoli-config/hl-line-ignored-modes
    '(treemacs-mode dired-mode magit-mode)
    "List of major modes where the idle hl-line behavior should be ignored.")

  (defvar-local consoli-config/hide-hl-line-timer nil
    "Timer to show the hl-line after a certain idle time.")

  (defun consoli-config/setup-idle-hl-line ()
    "Set up hooks to hide/show the hl-line based on idle time and input."
    (unless (apply #'derived-mode-p consoli-config/hl-line-ignored-modes)
      (add-hook 'post-command-hook #'consoli-config/hl-line-reset-on-command nil t)
      (add-hook 'kill-buffer-hook #'consoli-config/cleanup-hl-line-timer nil t)
      (add-hook 'change-major-mode-hook #'consoli-config/cleanup-hl-line-timer nil t)
      (consoli-config/start-idle-timer)))

  (defun consoli-config/show-hl-line ()
    "Enable `hl-line-mode` unless in ignored modes."
    (unless (or hl-line-mode
                (apply #'derived-mode-p consoli-config/hl-line-ignored-modes))
      (hl-line-mode 1)))

  (defun consoli-config/hide-hl-line ()
    "Disable `hl-line-mode` unless in ignored modes."
    (when (and hl-line-mode
               (not (apply #'derived-mode-p consoli-config/hl-line-ignored-modes)))
      (hl-line-mode -1)))

  (defun consoli-config/start-idle-timer ()
    "Start idle timer to show `hl-line-mode` after some time."
    (unless (apply #'derived-mode-p consoli-config/hl-line-ignored-modes)
      (when consoli-config/hide-hl-line-timer
        (cancel-timer consoli-config/hide-hl-line-timer))
      (condition-case err
          (setq consoli-config/hide-hl-line-timer
                (run-with-idle-timer consoli-config/show-hl-line-after-secs nil #'consoli-config/show-hl-line))
        (error (message "Error starting idle timer: %s" err)))))

  (defun consoli-config/cleanup-hl-line-timer ()
    "Cancel and clean up idle hl-line timer."
    (when consoli-config/hide-hl-line-timer
      (cancel-timer consoli-config/hide-hl-line-timer)
      (setq consoli-config/hide-hl-line-timer nil)))

  (defun consoli-config/hl-line-reset-on-command ()
    "Disable hl-line and start the idle timer again."
    (unless (apply #'derived-mode-p consoli-config/hl-line-ignored-modes)
      (consoli-config/hide-hl-line)
      (consoli-config/start-idle-timer))))
;;:hook (prog-mode . consoli-config/setup-idle-hl-line))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

(define-key prog-mode-map (kbd "C-|")
            (lambda ()
              (interactive)
              (hl-todo-insert "TODO(matheus-consoli): ")))

;; Line numbers for programming modes
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 5))

;; Terminal color support
(defun consoli-config/enable-256color-term ()
  "Enable 256 color support in terminal."
  (interactive)
  (load-library "term/xterm")
  (terminal-init-xterm))

(unless (display-graphic-p)
  (when (string-suffix-p "256color" (getenv "TERM"))
    (consoli-config/enable-256color-term)))

(defun consoli-config/tab-bar-project-name ()
  "Return project name for tab, fallback to buffer name."
  (if-let* ((project (project-current)))
      (concat " " (project-name project))
    (buffer-name)))

(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :bind
  (:map tab-bar-map
        ("<next>" . tab-bar-switch-to-next-tab)
        ("<prior>" . tab-bar-switch-to-prev-tab))
  :config
  (setq tab-bar-new-tab-choice t
        tab-bar-new-tab-to 'rightmost
        tab-bar-close-button-show nil
        tab-bar-show 0
        tab-bar-new-button-show nil
        tab-bar-tab-name-function #'consoli-config/tab-bar-project-name
        tab-bar-tab-name-truncated-max 30
        tab-bar-tab-hints nil
        tab-bar-auto-width t
        tab-bar-auto-width-max '((100) 20)
        tab-bar-auto-width-min '((15) 2)))

(defun consoli-config/buffer-has-project-p ()
  "Return non-nil if current buffer belongs to a project."
  (project-current))

(defun consoli-config/advice-tab-bar-new-tab (orig-fun &rest args)
  "Only create new tab if current buffer has a project."
  (when (consoli-config/buffer-has-project-p)
    (apply orig-fun args)))

(defun consoli-config/setup-project-only-tabs ()
  "Configure tabs to only show for projects."
  ;; Hide tab-bar initially
  (setq tab-bar-show 0)

  ;; Advice tab creation functions
  (advice-add 'tab-bar-new-tab :around #'consoli-config/advice-tab-bar-new-tab)
  (advice-add 'tab-bar-new-tab-to :around #'consoli-config/advice-tab-bar-new-tab)

  ;; Show tab-bar only when we have project tabs
  (add-hook 'tab-bar-tab-post-open-functions
            (lambda (&rest _)
              (when (> (length (tab-bar-tabs)) 1)
                (setq tab-bar-show 0))))

  ;; Close non-project tabs on startup
  (run-with-idle-timer 0.1 nil
                       (lambda ()
                         (let ((tabs (tab-bar-tabs)))
                           (dolist (tab tabs)
                             (let ((tab-name (alist-get 'name tab)))
                               (unless (and tab-name
                                            (not (string-match-p "^\\*.*\\*$" tab-name))
                                            (not (string= tab-name "Default")))
                                 (when (> (length tabs) 1)
                                   (tab-bar-close-tab (1+ (cl-position tab tabs)))))))))))

(add-hook 'after-init-hook #'consoli-config/setup-project-only-tabs)

(use-package project-tab-groups
  :after project
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (keymap-set project-prefix-map "m" #'magit-project-status)
  (project-tab-groups-mode 1))

(use-package tabspaces
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-session-project-session-store "~/.emacs.d/tabspaces-sessions")
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore nil)
  :config
  ;; Tabspaces integration for terminals
  (add-hook 'tabspaces-switch-workspace-finish-hook
            (lambda ()
              (consoli-config/hide-terminal-window)
              (consoli-config/refresh-centaur-tabs)))
  ;; Filter Buffers for Consult-Buffer

  ;; set consult-workspace buffer list
  (defun consoli-config/consult-buffer-filter ()
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))

  (eval-after-load 'consult #'consoli-config/consult-buffer-filter))

(defun consoli-config/setup-centaur-hooks ()
  "Hide centaur tabs for some modes."
  (dolist (hook '(aidermacs-comint-mode-hook
                  aidermacs-vterm-mode-hook
                  compilation-mode-hook
                  git-commit-mode-hook
                  magit-mode-hook
                  message-mode-hook
                  messages-buffer-mode-hook
                  org-src-mode-hook
                  special-mode-hook
                  transient-setup-buffer-hook))
    (add-hook hook #'centaur-tabs-local-mode)))

(use-package centaur-tabs
  :hook (after-init . centaur-tabs-mode)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts alternative-programming-font (consoli-config/font-height-centaur-tabs))
  (consoli-config/setup-centaur-hooks)
  :init
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blocklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)
       (string-prefix-p "*Warnings*" name)
       (string-prefix-p "*Messages*" name)
       (string-prefix-p "*Async Native" name)
       (string-prefix-p "*Native Compile" name)
       (string-prefix-p "*Compile-Log" name)
       (string-prefix-p "*aidermacs" name)
       (string-prefix-p "*eldoc" name)
       (string-prefix-p "*scratch*" name)
       (string-prefix-p "*Backtrace*" name)
       (string-prefix-p "*Completions*" name)
       (string-prefix-p "*Embark" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))

       ;; Hide non-project terminal buffers (terminals from other projects)
       (and (string-prefix-p "*vterm:" name)
            (not (consoli-config/is-terminal-for-current-project-p x))))))

  ;; hide tabs for some modes

  :bind
  (:map centaur-tabs-mode-map
        (([remap next-buffer] . centaur-tabs-forward)
         ([remap previous-buffer] . centaur-tabs-backward)
         ("C-c t j" . centaur-tabs-ace-jump)
         ("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward)))
  :custom
  (centaur-tabs-height 25)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-style "alternate")
  (centaur-tabs-new-tab-text "")
  ;;(centaur-tabs-hide-tab-function 'centaur-tabs-hide-tab)
  (centaur-tabs-set-bar nil)
  (x-underline-at-descent-line t) ;; to correctly display the icon
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-modified-marker t)
  (centaur-tabs-enable-key-bindings t))



(use-package treemacs
  :defer t
  :custom
  (treemacs-file-follow-delay 3)
  (treemacs-expand-after-init nil)
  (treemacs-file-follow-delay 1.5)
  :config
  (treemacs-resize-icons 11)
  (treemacs-git-commit-diff-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode nil)
  (treemacs-filewatch-mode t)
  (treemacs-peek-mode t)
  (treemacs-resize-icons 11)
  :bind
  (:map global-map
        ("C-c ; ;" . treemacs)
        ("C-c ; B" . treemacs-bookmark)
        ("C-c ; f" . treemacs-find-file)
        ("C-c ; g" . treemacs-find-tag)))

(use-package treemacs-magit
  :defer t
  :after (treemacs magit))

(use-package treemacs-nerd-icons
  :defer t
  :after (treemacs))

(use-package treemacs-tab-bar
  :defer t
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(use-package mood-line
  :hook (after-init . mood-line-mode)
  :config
  (defun consoli-config/custom-segment-cursor-position ()
    (format-mode-line "line: %l column: %c"))

  (defun consoli-config/custom-segment-scroll ()
    (let* ((scroll (mood-line-segment-scroll)))
      (if (member scroll '("All" "Top" "Bottom"))
          ""
        scroll)))

  (defun consoli-config/buffer-status-segment ()
    (or (mood-line-segment-buffer-status)
        (propertize "●" 'face '(:inherit success :weight normal))))

  (setq mood-line-format
        (mood-line-defformat
         :left
         (((mood-line-segment-major-mode) . " ")
          ((consoli-config/buffer-status-segment) . " ")
          ((consoli-config/custom-segment-cursor-position) . " ")
          ((consoli-config/custom-segment-scroll) . " ")
          ((mood-line-segment-region) . " ")
          (mood-line-segment-multiple-cursors))
         :right
         (((mood-line-segment-misc-info) . " ")
          ((mood-line-segment-checker) . " ")
          (mood-line-segment-vc))))
  :custom (mood-line-glyph-alist mood-line-glyphs-fira-code))

(defun consoli-config/setup-echo-area ()
  "Ensure echo area buffers are created."
  (dolist (fn '(save-buffer
                write-file
                set-mark-command
                keyboard-quit))
    (advice-add fn :around #'shut-up--advice))
  (put 'quit 'error-message "")

  (get-buffer-create " *Echo Area 0*")
  (with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default '(:inherit mode-line :height 0.8 :box nil)))
  (get-buffer-create " *Echo Area 1*")
  (with-current-buffer " *Echo Area 1*" (face-remap-add-relative 'default '(:inherit mode-line :height 0.8 :box nil))))

(add-hook 'after-init-hook #'consoli-config/setup-echo-area)

(setq-default display-time-default-load-average nil)
(setq-default display-time-format "%I:%M")
(display-time-mode t)

(defun consoli-config/hide-modeline ()
  "Hide the mode-line in the current buffer."
  (setq-local mode-line-format nil))

(dolist (hook '(aidermacs-comint-mode-hook
                aidermacs-vterm-mode-hook
                compilation-mode-hook
                git-commit-mode-hook
                messages-buffer-mode-hook
                special-mode-hook
                transient-setup-buffer-hook))
  (add-hook hook #'consoli-config/hide-modeline))

;; Org prettify symbols helper functions
(defvar-local rasmus/org-at-src-begin -1
  "Variable that holds whether last position was a ")

(defcustom rasmus/ob-header-symbol ?☰
  "Symbol used for babel headers")

(defun rasmus/org-prettify-src--update ()
  (let ((case-fold-search t)
        (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
        found)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (goto-char (match-end 0))
        (let ((args (org-trim
                     (buffer-substring-no-properties (point)
                                                     (line-end-position)))))
          (when (org-string-nw-p args)
            (let ((new-cell (cons args rasmus/ob-header-symbol)))
              (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
              (cl-pushnew new-cell found :test #'equal)))))
      (setq prettify-symbols-alist
            (cl-set-difference prettify-symbols-alist
                               (cl-set-difference
                                (cl-remove-if-not
                                 (lambda (elm)
                                   (eq (cdr elm) rasmus/ob-header-symbol))
                                 prettify-symbols-alist)
                                found :test #'equal)))
      ;; Clean up old font-lock-keywords.
      (font-lock-remove-keywords nil prettify-symbols--keywords)
      (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
      (font-lock-add-keywords nil prettify-symbols--keywords)
      (while (re-search-forward re nil t)
        (font-lock-flush (line-beginning-position) (line-end-position))))))

(defun rasmus/org-prettify-src ()
  "Hide src options via `prettify-symbols-mode'.
`prettify-symbols-mode' is used because it has uncollpasing. It's
may not be efficient."
  (let* ((case-fold-search t)
         (at-src-block (save-excursion
                         (beginning-of-line)
                         (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
    ;; Test if we moved out of a block.
    (when (or (and rasmus/org-at-src-begin
                   (not at-src-block))
              ;; File was just opened.
              (eq rasmus/org-at-src-begin -1))
      (rasmus/org-prettify-src--update))
    (setq rasmus/org-at-src-begin at-src-block)))

(defun rasmus/org-prettify-symbols ()
  (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
        (cl-reduce 'append
                   (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                           `(("#+begin_src" . ?)
                             ("#+end_src"   . ?)
                             ("#+header:" . ,rasmus/ob-header-symbol)
                             ("#+begin_quote" . ?)
                             ("#+end_quote" . ?)
                             ("#+begin_comment" . ?)
                             ("#+end_comment" . ?)))))
  (turn-on-prettify-symbols-mode)
  (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))

(use-package org
  :defer t
  :straight (:host github :repo "emacsmirror/org" :files ("lisp/*.el" "contrib/lisp/*.el"))
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . org-appear-mode)
         (org-mode . olivetti-mode)
         (org-mode . (lambda ()
                       "Beautify Org Checkbox Symbol"
                       (push '("[ ]" . "") prettify-symbols-alist)
                       (push '("[X]" . "" ) prettify-symbols-alist)
                       (push '("[-]" . "" ) prettify-symbols-alist)
                       (prettify-symbols-mode)))
         (org-mode . rasmus/org-prettify-symbols)
         (org-after-todo-statistics . org-summary-todo)
         (org-checkbox-statistics . my/org-checkbox-todo))
  :custom
  ;; Basic org settings
  (org-adapt-indentation nil)
  (org-ellipsis "   ")
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-folded 'fold)
  (org-return-follows-link t)
  (org-cycle-separator-lines 2)
  (org-support-shift-select 'always)
  (org-edit-src-persistent-message nil)
  (org-use-sub-superscripts nil)

  :custom-face
  ;; Fixed-pitch faces for code blocks and technical elements
  (org-block ((t (:inherit fixed-pitch))))
  (org-checkbox ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  (org-formula ((t (:inherit fixed-pitch))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-property-value ((t (:inherit fixed-pitch))))
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-table ((t (:inherit fixed-pitch))))
  (org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))
  (org-column ((t (:background unspecified))))
  (org-column-title ((t (:background unspecified))))
  ;; Variable-pitch and other styling
  (variable-pitch ((t (:family org-font))))
  (fixed-pitch ((t (:family alternative-programming-font))))
  (font-lock-doc-face ((t (:inherit font-lock-string-face))))
  (org-ellipsis ((t (:inherit default :box nil :underline nil :weight ultra-bold))))
  :config
  ;; Set dynamic font sizes (always applied, respecting scaling preference)
  (set-face-attribute 'variable-pitch nil
                      :family org-font
                      :height (consoli-config/font-height-org))
  (set-face-attribute 'fixed-pitch nil
                      :family alternative-programming-font
                      :height (consoli-config/font-height-programming))
  (set-face-attribute 'font-lock-doc-face nil
                      :height (consoli-config/font-height-programming))

  ;; Add language modes
  (add-to-list 'org-src-lang-modes (cons "rust" 'rust-ts))
  (add-to-list 'org-src-lang-modes (cons "go" 'go-ts))

  ;; Custom functions
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (defun consoli-config/org-checkbox-todo ()
    "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
        (save-excursion
          (org-back-to-heading t)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (goto-char beg)
          (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                                 end t)
              (if (match-end 1)
                  (if (equal (match-string 1) "100%")
                      (unless (string-equal todo-state "DONE")
                        (org-todo 'done))
                    (unless (string-equal todo-state "TODO")
                      (org-todo 'todo)))
                (if (and (> (match-end 2) (match-beginning 2))
                         (equal (match-string 2) (match-string 3)))
                    (unless (string-equal todo-state "DONE")
                      (org-todo 'done))
                  (unless (string-equal todo-state "TODO")
                    (org-todo 'todo))))))))))

;; Font-lock customizations
(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

(use-package org-variable-pitch
  :after org)

(use-package org-appear
  :after org
  :custom (org-appear-delay 2))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)

(defun consoli-config/org-mode-setup ()
  "Complete setup for org-mode."
  ;; Beautify Org Checkbox Symbol
  (push '("[ ]" . "") prettify-symbols-alist)
  (push '("[X]" . "" ) prettify-symbols-alist)
  (push '("[-]" . "" ) prettify-symbols-alist)
  (prettify-symbols-mode)
  ;; Setup prettify symbols for src blocks
  (rasmus/org-prettify-symbols)
  ;; Disable drag-stuff in org buffers
  (drag-stuff-mode -1))

(add-hook 'org-mode-hook #'consoli-config/org-mode-setup)

(add-hook 'org-mode-hook (lambda ()
                           (push '("[ ]" . "") prettify-symbols-alist)
                           (push '("[X]" . "" ) prettify-symbols-alist)
                           (push '("[-]" . "" ) prettify-symbols-alist)
                           (prettify-symbols-mode)))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package olivetti
  :hook
  ((org-mode . olivetti-mode)
   (markdown-mode . olivetti-mode)
   (prog-mode . olivetti-mode))
  :custom
  (olivetti-body-width 0.80))

(setopt default-justification 'full)

(use-package sqlite3
  :defer t)

(require 'org-fold)

(use-package org-roam
  :defer t
  :bind (("C-c b f" . org-roam-node-find)
         ("C-c b i" . org-roam-node-insert)
         ("C-c b c" . org-roam-capture)
         ("C-c b g" . org-roam-graph)
         ("C-c b d" . org-roam-dailies-capture-today)
         ("C-c b D" . org-roam-dailies-goto-today)
         ("C-c b y" . org-roam-dailies-goto-previous))
  :config
  (cl-defmethod org-roam-node-keywords ((node org-roam-node))
    "Return the currently set category for the NODE."
    (cdr (assoc-string "KEYWORDS" (org-roam-node-properties node))))
  (cl-defmethod org-roam-node-authors ((node org-roam-node))
    "Return the currently set category for the NODE."
    (cdr (assoc-string "AUTHORS" (org-roam-node-properties node))))
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:15}" 'face 'org-tag)
                (propertize "${keywords:20}" 'face 'org-tag)
                (propertize "${authors:15}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(setq-default
 org-roam-directory (file-truename "~/projects/brainiac/")
 org-roam-db-location (file-truename "~/projects/brainiac/org-roam.db")
 org-roam-dailies-directory "dailies/")

(setq-default
 org-roam-dailies-capture-templates
 '(("d" "default" entry
    "\n\n* %<%I:%M %p>: %?"
    :target (file+head "%<%Y-%m-%d>.org"
                       "#+TITLE: %<%Y-%m-%d>\n"))))

(setq-default org-roam-capture-templates
              '(("u" "uncategorized" plain
                 "* %?"
                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                    "#+FILETAGS: :uncategorized:\n#+TITLE: ${title}\n#+DATE: %U\n\n")
                 :unnarrowed t)))

(add-to-list 'org-roam-capture-templates
             '("k" "book" plain
               "\n\n* Contents%?"
               :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                  ":PROPERTIES:\n:AUTHORS: %^{authors}\n:KEYWORDS: %^{keywords}\n:END:\n\n#+FILETAGS: :book:\n#+TITLE: ${title}\n#+DATE: %U\n")
               :unnarrowed t))

(add-to-list 'org-roam-capture-templates
             '("p" "paper" plain
               "* Reference\nYear: %^{year}\nLink: %^{Link}\n\n* Abstract\n%?"
               :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                  ":PROPERTIES:\n:AUTHORS: %^{authors}\n:KEYWORDS: %^{keywords}\n:END:\n\n#+FILETAGS: :paper:\n#+TITLE: ${title}\n#+DATE: %U\n")
               :unnarrowed t))

(add-to-list 'org-roam-capture-templates
             '("b" "blog post" plain
               "* Reference\nYear: %^{year}\nLink: %^{link}\n\n* %?"
               :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                  ":PROPERTIES:\n:AUTHORS: %^{authors}\n:KEYWORDS: %^{keywords}\n:END:\n\n#+FILETAGS: :blog:\n#+TITLE: ${title}\n#+DATE: %U\n\n")
               :unnarrowed t))

(add-to-list 'org-roam-capture-templates
             '("s" "therapy session" entry
               "** sessão %? - %<%Y/%m/%d>"
               :target (file+head "20231113224353-therapy.org"
                                  "* Sessões")
               ))

(add-to-list 'org-roam-capture-templates
             '("t" "disfunctional thought" entry
               "** [%<%Y/%m/%d %Hh%M>] "
               :target (file+head "20231113224353-therapy.org"
                                  "* Disfunctional thought")
               ))

(use-package org-roam-ui
  :defer t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  (defun open-org-roam-ui ()
    (interactive)
    (when (not (bound-and-true-p org-roam-ui-mode))
      (org-roam-ui-mode))
    (org-roam-ui-open))
  :bind
  ("C-c b g" . open-org-roam-ui))

(use-package deft
  :defer t
  :after org
  :bind
  ("<f8>" . deft)
  :config
  (setq deft-recursive t
        deft-use-filter-string-for-filename t
        deft-default-extension "org"
        deft-directory (file-truename "~/projects/brainiac")))

(defun cm/deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
   If `deft-use-filename-as-title' is nil, the title is taken to
   be the first non-empty line of the FILE.  Else the base name of the FILE is
   used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
        (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

(advice-add 'deft-parse-title :override #'cm/deft-parse-title)

(setq-default deft-strip-summary-regexp
              (concat "\\("
                      "[\n\t]" ;; blank
                      "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                      "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                      "\\)"))

(with-eval-after-load 'org
  (defun rasmus/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?)
                               ("#+end_src"   . ?)
                               ("#+header:" . ,rasmus/ob-header-symbol)
                               ("#+begin_quote" . ?)
                               ("#+end_quote" . ?)
                               ("#+begin_comment" . ?)
                               ("#+end_comment" . ?)))))
    (turn-on-prettify-symbols-mode)
    (add-hook 'post-command-hook 'rasmus/org-prettify-src t t)))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(defun consoli-config/org-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
  (let ((todo-state (org-get-todo-state)) beg end)
    (unless (not todo-state)
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    (unless (string-equal todo-state "DONE")
                      (org-todo 'done))
                  (unless (string-equal todo-state "TODO")
                    (org-todo 'todo)))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (unless (string-equal todo-state "DONE")
                    (org-todo 'done))
                (unless (string-equal todo-state "TODO")
                  (org-todo 'todo)))))))))

(add-hook 'org-checkbox-statistics-hook 'consoli-config/org-checkbox-todo)

;; (setq-default
;;  ;; adapt indentation of content to match its heading
;;  org-adapt-indentation nil
;;  org-ellipsis "   "
;;  org-hide-emphasis-markers t
;;  ;; non-nil = utf-8
;;  org-pretty-entities t
;;  org-startup-folded 'fold
;;  org-return-follows-link t
;;  ;; only needs one empty line to show an empty line when collapsed
;;  org-cycle-separator-lines 2
;;  ;; shift-select with mouse
;;  org-support-shift-select 'always
;;  ;; no help message when editing code
;;  org-edit-src-persistent-message nil
;;  ;; disable a_b to be rendered as subscript, still can use a_{b} to get the same result
;;  org-use-sub-superscripts nil)

;; (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil :underline nil :weight 'ultra-bold)

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens
  :hook (after-init . smartparens-global-mode)
  :bind
  ("C-M-f" . sp-forward-sexp)
  ("C-M-b" . sp-backward-sexp)
  ("C-M-n" . sp-next-sexp)
  ("C-M-p" . sp-previous-sexp)
  ("C-M-a" . sp-beginning-of-sexp)
  ("C-M-e" . sp-end-of-sexp)
  ("C-M-d" . sp-down-sexp)
  ("C-M-u" . sp-backward-up-sexp)

  ("C-M-[" . sp-wrap-square)
  ("C-M-{" . sp-wrap-curly)
  ("C-M-(" . sp-wrap-round)
  ("C-M-)" . sp-unwrap-sexp)
  ("C-M-<backspace>" . sp-backward-unwrap-sexp)

  ("C-{" . sp-backward-slurp-sexp)
  ("C-}" . sp-forward-slurp-sexp)
  ("C-M--" . sp-backward-barf-sexp)
  ("C-M-=" . sp-forward-barf-sexp)

  ("C-M-t" . sp-transpose-sexp)
  ("C-M-k" . sp-kill-sexp)
  ("C-M-w" . sp-copy-sexp)
  :config
  ;; Rust-specific pairs
  (sp-with-modes '(rust-ts-mode)
    (sp-local-pair "<" ">" :post-handlers '((indent-between-pair "RET")))
    (sp-local-pair "'" nil :actions nil))

  ;; Org-mode pairs
  (sp-with-modes '(org-mode)
    (sp-local-pair "~" "~")
    (sp-local-pair "=" "="))

  ;; Programming mode pairs with better indentation
  (sp-with-modes '(prog-mode)
    (sp-local-pair "{" nil :post-handlers '((indent-between-pair "RET")))
    (sp-local-pair "[" nil :post-handlers '((indent-between-pair "RET")))
    (sp-local-pair "(" nil :post-handlers '((indent-between-pair "RET")))))

(show-paren-mode 1)
(setopt show-paren-style 'expression)
;; (set-face-attribute 'show-paren-match nil :foreground "#FF3377" :weight 'regular :inherit t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :hook
  (after-save . magit-after-save-refresh-status)
  :config
  (advice-add #'magit-auto-revert-mode :around #'shut-up--advice)
  (defun consoli-config/setup-magit-hooks ()
    (when (magit-git-repo-p default-directory)
      (add-hook 'after-save-hook 'magit-after-save-refresh-status nil t))))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package magit-delta
  :after magit
  :commands magit-delta-mode
  :hook (magit-mode . magit-delta-mode))

(use-package conventional-commit
  :defer t
  :straight (:host github :repo "akirak/conventional-commit.el")
  :hook
  (git-commit-mode . conventional-commit-setup))

(use-package fringe-helper
  :defer t)

(use-package git-gutter
  :hook (after-init . global-git-gutter-mode))

(use-package git-gutter-fringe
  :after git-gutter
  :custom
  (git-gutter-fr:side 'right-fringe)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [#b10000000
     #b11000000
     #b11100000
     #b11110000
     #b11111000
     #b11111100
     #b11111110
     #b11111111] nil nil 'bottom)

  ;; Automatically update git-gutter on focus change
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  ;; Update git-gutter when saving buffer
  (add-hook 'after-save-hook 'git-gutter)

  ;; Integrate with magit for smoother operation
  (when (fboundp 'magit-mode)
    (add-hook 'magit-post-refresh-hook 'git-gutter:update-all-windows)))

(use-package blamer
  :defer t
  :bind (("C-c i" . blamer-show-posframe-commit-info))
  :hook (prog-mode . blamer-mode)
  :custom
  (blamer-datetime-formatter ", %s ")
  (blamer-author-formatter " %s") ;; 
  (blamer-commit-formatter "")
  (blamer-idle-time 3)
  (blamer-smart-background-p t)
  (blamer-min-offset 5)
  (blamer-type 'visual) ;; only about one line
  (blamer-prettify-time-p t)
  (blamer-max-commit-message-length 30)
  (blamer-show-avatar-p t)
  (blamer-symbol-count-before-new-line 1))

(use-package forge
  :defer t
  :after magit)

(use-package gh-notify
  :defer t)

(use-package github-review
  :defer t
  :straight
  (github-review :type git :host github :repo "charignon/github-review" :files ("*.el"))
  :after forge
  :bind (("C-x r" . github-review-forge-pr-at-point)
         :map diff-mode-map ("C-c s" . my/github-review-kill-suggestion))
  :config
  ;; from github.com/anticomputer/gh-notify
  (defun my/github-review-kill-suggestion ()
    ;; kill a region of diff+ as a review suggestion template
    (interactive)
    (setq deactivate-mark t)
    (let ((s-region
           (buffer-substring-no-properties
            (region-beginning)
            (region-end))))
      (kill-new
       (format "# ```suggestion\n%s\n# ```\n"
               (replace-regexp-in-string "^\\+" "# " s-region))))))

(setq-default
 ediff-keep-variants nil
 ediff-make-buffers-readonly-at-startup nil
 ediff-merge-revisions-with-ancestor t
 ediff-show-clashes-only t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package expand-region
  :defer t
  :bind ("C-c e =" . 'er/expand-region)
  ("C-c e p" . 'er/mark-inside-pairs)
  ("C-c e P" . 'er/mark-outside-pairs)
  ("C-c e q" . 'er/mark-inside-quotes)
  ("C-c e Q" . 'er/mark-outside-quotes)
  ("C-c e m" . 'er/mark-method-call)
  ("C-c e c" . 'er/mark-comment)
  ("C-c e -" . 'er/contract-region))

(use-package multiple-cursors
  :defer t
  :config (define-key mc/keymap (kbd "RET") nil)
  :init
  (defvar-keymap me/multiple-cursors-map
    :repeat t
    "r" #'mc/mark-all-in-region
    "l" #'mc/edit-lines
    "." #'mc/mark-next-like-this-symbol
    ">" #'mc/skip-to-next-like-this
    "," #'mc/mark-previous-like-this-symbol
    "<" #'mc/skip-to-previous-like-this)
  :bind-keymap ("C->" . me/multiple-cursors-map))

(use-package ts-movement
  :defer t
  :straight (:type git :host github :repo "matheus-consoli/ts-movement")
  :ensure multiple-cursors
  :init
  (defvar-keymap me/ts-movement-map
    :repeat t
    "d" #'tsm/delete-overlay-at-point
    "D" #'tsm/clear-overlays-of-type
    "b" #'tsm/node-prev
    "C-b" #'tsm/backward-overlay
    "C-f" #'tsm/forward-overlay
    "f" #'tsm/node-next
    "p" #'tsm/node-parent
    "n" #'tsm/node-child
    "N" #'tsm/node-children
    "s" #'tsm/node-children-of-type
    "a" #'tsm/node-start
    "e" #'tsm/node-end
    "m" #'tsm/node-mark
    "c" #'tsm/mc/mark-all-overlays)
  :bind-keymap ("C-c ." . me/ts-movement-map)
  :hook
  (bash-ts-mode . ts-movement-mode)
  (c++-ts-mode . ts-movement-mode)
  (c-ts-mode . ts-movement-mode)
  (cmake-ts-mode . ts-movement-mode)
  (csharp-ts-mode . ts-movement-mode)
  (css-ts-mode . ts-movement-mode)
  (dockerfile-ts-mode . ts-movement-mode)
  (go-mod-ts-mode . ts-movement-mode)
  (go-ts-mode . ts-movement-mode)
  (java-ts-mode . ts-movement-mode)
  (js-ts-mode . ts-movement-mode)
  (json-ts-mode . ts-movement-mode)
  (python-ts-mode . ts-movement-mode)
  (ruby-ts-mode . ts-movement-mode)
  (rust-ts-mode . ts-movement-mode)
  (toml-ts-mode . ts-movement-mode)
  (tsx-ts-mode . ts-movement-mode)
  (typescript-ts-mode . ts-movement-mode)
  (yaml-ts-mode . ts-movement-mode))

(use-package symbol-overlay)

(use-package casual-symbol-overlay
  :bind ("M-i" . casual-symbol-overlay-tmenu))

(use-package drag-stuff
  :hook (after-init . drag-stuff-global-mode)
  :config
  (drag-stuff-define-keys))

(save-place-mode 1)

(use-package subword
  :ensure nil
  :after prog-mode)

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

(use-package treesitter-context
  :defer t
  :straight
  (:host github
         :repo "matheus-consoli/treesitter-context.el"
         :files ("*.el"))
  :hook
  (rust-ts-mode . treesitter-context-mode)
  (js-ts-mode . treesitter-context-mode)
  (typescript-ts-mode . treesitter-context-mode)
  (tsx-ts-mode . treesitter-context-mode)
  :bind
  (:map treesitter-context-mode
        ("C-)" . treesitter-context-focus-mode))
  :custom
  (treesitter-context-frame-position 'top-right)
  (treesitter-context-frame-padding 1)
  (treesitter-context-frame-offset-x -3)
  (treesitter-context-frame-offset-y 70)
  (treesitter-context-show-context-always nil) ;; only show when the outmost parent is invisible
  (treesitter-context-frame-autohide-timeout 8)
  (treesitter-context-show-line-number nil)
  (treesitter-context-frame-min-height 1)
  (treesitter-context-border-width 2)
  (treesitter-context-frame-min-width 1)
  (treesitter-context-fold-show-fringe-marks nil)
  (treesitter-context-frame-font-fraction 0.85)
  (treesitter-context-border-color (face-attribute 'mode-line :background))
  (treesitter-context-background-color (face-attribute 'default :background)))

(use-package treesit-fold
  :hook (after-init . global-treesit-fold-mode)
  :straight (:host github :repo "emacs-tree-sitter/treesit-fold")
  :custom
  (treesit-fold-summary-show nil)
  :bind (:map treesit-fold-mode-map
              (("C-{" . treesit-fold-toggle)
               ("C-*" . treesit-fold-open-all)
               ("C-}" . treesit-fold-close-all))))

(use-package visual-regexp
  :defer t)
(use-package visual-regexp-steroids
  :defer t
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)
   ("C-r" . vr/isearch-backward)
   ("C-s" . vr/isearch-forward))
  (:map me/multiple-cursors-map
        ("r" . vr/mc-mark)))

(use-package better-jumper
  :init
  (defvar-keymap me/better-jumper-map
    :repeat t
    "<left>" #'better-jumper-jump-forward
    "<right>" #'better-jumper-jump-backward
    "." #'better-jumper-set-jump)
  :bind-keymap ("C-c j" . me/better-jumper-map)
  :hook (prog-mode turn-on-better-jumper-mode))

(use-package rust-ts-mode
  :defer t)

(defun consoli-config/rust-eglot-setup ()
  "Set up rust-analyzer initialization options for Eglot."
  (setq-local eglot-workspace-configuration
              '((:rust-analyzer
                 (:lruCapacity 1920) ; (* 15 128)
                 (:procMacro (:enable . t))
                 (:lens
                  (:references
                   (:adt (:enable . t))
                   (:enumVariant (:enable . t))
                   (:method (:enable . t))
                   (:trait (:enable . t))))
                 (:completion (:autoimport (:enable . t)))
                 (:inlayHints
                  (:bindingModeHints (:enable . t))
                  (:chainingHints (:enable . t))
                  (:closingBraceHints (:minLines . 0))
                  (:closureCaptureHints (:enable . t))
                  (:closureReturnTypeHints (:enable . "with_block"))
                  (:discriminantHints (:enable . "always"))
                  (:expressionAdjustmentHints
                   (:enable . "always")
                   (:mode . "prefer_postfix"))
                  (:implicitDrops (:enable . t))
                  (:lifetimeElisionHints
                   (:enable . "always")
                   (:useParameterNames . t))
                  (:maxLen . 25)
                  (:parameterHints (:enable . t))
                  (:reborrowHints (:enable . "always"))
                  (:displayClosureReturnTypeHints . t)
                  ;; (:hideClosureInitialization . t)
                  ;; (:hideNamedConstructor . t)
                  )))))
(defun consoli-config/rust-ts-mode-setup ()
  "Complete setup for rust-ts-mode."
  ;; Setup eglot for Rust
  (consoli-config/rust-eglot-setup)
  ;; Setup syntax highlighting
  (consoli-config/rust-ts-mode-highlighting))

(add-hook 'rust-ts-mode-hook #'consoli-config/rust-ts-mode-setup)

(defface consoli-config-rust-unwrap-face
  '((t (:inherit error
                 :weight bold
                 :foreground "#AD3E3E")))
  "Face for Rust .unwrap() calls")

(defface consoli-config-rust-macro-face
  '((t (:inherit font-lock-function-call-face
                 :slant italic
                 :width expanded
                 :underline t)))
  "Face for Rust todo! macro calls")

(defface consoli-config-rust-attribute-face
  '((t (:inherit font-lock-preprocessor-face
                 :weight light
                 :width expanded
                 :slant oblique
                 :foreground "#7e57c2")))
  "Face for Rust attribute items")

(defun consoli-config/rust-ts-mode-highlighting ()
  "Apply custom tree-sitter highlights for Rust mode."
  (setq-local
   treesit-font-lock-settings
   (append
    treesit-font-lock-settings
    ;; Highlight .unwrap() calls
    (treesit-font-lock-rules
     :language 'rust
     :feature 'unwrap-call
     :override t
     '(((field_expression
         field: (field_identifier) @consoli-config-rust-unwrap-face)
        (:match "^unwrap$" @consoli-config-rust-unwrap-face))))

    ;; Highlight todo!() macro
    (treesit-font-lock-rules
     :language 'rust
     :feature 'todo-macro
     :override t
     '(((macro_invocation
         (identifier) @consoli-config-rust-macro-face)
        (:match "^todo$" @consoli-config-rust-macro-face))))

    ;; Highlight #[attribute] items
    (treesit-font-lock-rules
     :language 'rust
     :feature 'attribute-items
     :override t
     '((attribute_item) @consoli-config-rust-attribute-face)))))

(use-package cargo-transient
  :defer t
  :after rust-ts-mode
  :bind (:map rust-ts-mode-map
              ("C-c C-c t" . cargo-transient)
              ("C-c C-c c" . cargo-transient-check)
              ("C-c C-c l" . cargo-transient-clippy)
              ("C-c C-c f" . cargo-transient-clippy-fix)
              ("C-c C-c k" . cargo-transient-clippy-test)
              ("C-c C-c r" . cargo-transient-run)))

;; (setq
;;  lsp-rust-analyzer-rustfmt-override-command ["./build/x86_64-unknown-linux-gnu/stage0/bin/rustfmt", "--edition=2021"]
;;  lsp-rust-analyzer-cargo-run-build-scripts t
;;  lsp-rust-analyzer-rustc-source "./Cargo.toml"
;;  lsp-rust-analyzer-proc-macro-enable t)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(defun consoli-config/python-eglot-setup ()
  (setq-local eglot-workspace-configuration
              '((:pylsp . (:configurationSources ["flake8"]
                                                 :plugins (:pycodestyle (:enabled :json-false)
                                                                        :mccabe (:enabled :json-false)
                                                                        :pyflakes (:enabled :json-false)
                                                                        :flake8 (:enabled :json-false
                                                                                          :maxLineLength 88)
                                                                        :ruff (:enabled t
                                                                                        :lineLength 88)
                                                                        :pydocstyle (:enabled t
                                                                                              :convention "numpy")
                                                                        :yapf (:enabled :json-false)
                                                                        :autopep8 (:enabled :json-false)
                                                                        :black (:enabled t
                                                                                         :line_length 88
                                                                                         :cache_config t)))))))

(use-package hyprlang-ts-mode
  :defer t
  :config
  (add-to-list 'treesit-language-source-alist
               '(hyprlang "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"))
  :custom
  (hyprlang-ts-mode-indent-offset 4))

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer t
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

(use-package just-ts-mode
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EDITING BEHAVIOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic editing settings
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Scrolling behavior
(setq-default scroll-preserve-screen-position 'always
              scroll-conservatively 101
              fast-but-imprecise-scrolling t
              redisplay-dont-pause 1
              jit-lock-defer-time 0)

(use-package scroll-restore
  :hook (after-init . scroll-restore-mode)
  :custom
  ((scroll-restore-handle-cursor t)
   (scroll-restore-restore-jump-back t)
   (scroll-restore-cursor-type nil))
  :bind ("<Scroll_Lock>" . scroll-restore-mode))


;; Smart indentation detection
(defun consoli-config/infer-indentation-style ()
  "Infer indentation style from buffer content."
  (let ((space-count (how-many "^ " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (cond
     ((> space-count tab-count) (setq indent-tabs-mode nil))
     ((> tab-count space-count) (setq indent-tabs-mode t)))))

;; Terminal suspend function
(defun consoli-config/suspend-if-in-shell ()
  "Suspend process if not running in GUI."
  (when (not (display-graphic-p))
    (suspend-emacs)))

(use-package good-scroll
  :hook (after-init . good-scroll-mode)
  :bind
  (([remap next] . good-scroll-up-full-screen)
   ([remap prior] . good-scroll-down-full-screen))
  :custom
  (good-scroll-duration 0.2))

(setopt backup-directory-alist '(("." . "~/.emacs.d/backup/per-save"))
        auto-save-file-name-transforms '(("\\(.*/\\)?\\([^/]*\\)\\'" "~/.emacs.d/backup/auto-saves/\\2" t)))

(setopt delete-old-versions t
        ;; number of new versions of a file to kept
        kept-new-versions 1
        ;; number of old version to kept
        kept-old-versions 2
        ;; numeric version control
        version-control t
        ;; copy files, dont rename them
        backup-by-copying t)

(setq-default auto-save-timeout 15 ;; seconds
              auto-save-interval 200)

(use-package super-save
  :hook (after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 5)
  (super-save-delete-trailing-whitespace 'except-current-line))

(setq-default create-lockfiles nil)

;; Enhanced line editing with crux
(use-package crux
  :bind ([remap kill-line] . crux-smart-kill-line)
  ([remap kill-whole-line] . crux-kill-whole-line)
  ("C-a" . crux-move-beginning-of-line)
  ("C-c d" . crux-duplicate-current-line-or-region)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region))

;; Frame and bell settings
(set-frame-parameter (selected-frame) 'buffer-predicate #'buffer-file-name)
(setq-default ring-bell-function 'ignore)

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :config
  (setq jinx-languages "en_UK en_US pt_BR")

  ;; `M-$` correct the word at point
  :bind ([remap ispell-word] . jinx-correct)
  ("M-|" . jinx-correct-nearest))

;; Additional editing settings
(setopt tab-always-indent 'complete
        text-mode-ispell-word-completion nil)

(setq-default pulse-delay 0.03
              pulse-iterations 13)

;; Pulse region on region-based operations
(dolist (fn '(kill-ring-save sp-copy-sexp sp-backward-copy-sexp
                             kill-region
                             sp-wrap-round sp-wrap-square sp-wrap-curly))
  (advice-add fn :before #'consoli-config/pulse-current-region))

;; Pulse sexp on sexp-based operations
(dolist (fn '(sp-kill-sexp sp-backward-kill-sexp sp-kill-hybrid-sexp
                           sp-transpose-sexp sp-unwrap-sexp sp-backward-unwrap-sexp))
  (advice-add fn :before #'consoli-config/pulse-sexp-at-point))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        undo-tree-visualizer-lazy-drawing 10))

(use-package colorful-mode
  :hook (prog-mode text-mode))

(use-package annotate
  :config
  (setq annotate-annotation-column 25
        annotate-use-messages nil
        annotate-use-echo-area nil
        annotate-highlight-faces '((:underline "#c4addd")
                                   (:underline "#d1c0ec")
                                   (:underline "#8d77a8"))
        annotate-annotation-text-faces '((:inherit 'font-lock-comment-face
                                                   :slant italic
                                                   :family alternative-programming-font)
                                         (:inherit 'font-lock-comment-face
                                                   :slant italic
                                                   :family alternative-programming-font)
                                         (:inherit 'font-lock-comment-face
                                                   :slant italic
                                                   :family alternative-programming-font)))
  :hook (prog-mode . annotate-mode)
  :init
  (setq annotate-mode-map (make-sparse-keymap))
  (define-prefix-command 'annotate-command-prefix)
  (define-key annotate-mode-map (kbd "C-c C-a") 'annotate-command-prefix)
  :bind
  (:map annotate-command-prefix
        ("n" . annotate-toggle-annotation-text)
        ("a" . annotate-annotate)
        ("d" . annotate-delete-annotation)
        ("s" . annotate-show-annotation-summary)
        ("c" . annotate-change-annotation-colors)
        ("p" . annotate-change-annotation-position)
        ("<left>" . annotate-goto-next-annotation)
        ("<right>" . annotate-goto-previous-annotation)))

(use-package exec-path-from-shell
  :if (and (eq system-type 'gnu/linux) (display-graphic-p))
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH" "XDG_DATA_DIRS" "XDG_CONFIG_DIRS"
     "CARGO_HOME" "RUSTUP_HOME"
     "GOPATH" "GOROOT"
     "PYTHONPATH"
     "NODE_PATH"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-shell-name (getenv "SHELL"))
  :hook (after-init . exec-path-from-shell-initialize))

;; Bookmark and buffer settings
(setq-default bookmark-save-flag 1
              ibuffer-expert t)

(use-package clipetty
  :hook (after-init . load-clipetty-only-in-term))

(defun load-clipetty-only-in-term ()
  (when (not (display-graphic-p))
    (global-clipetty-mode t)))

(setq-default focus-follows-mouse t)

(global-set-key (kbd "<f9>") 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GLOBAL KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Text scaling
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; Buffer management
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Terminal suspend
(global-set-key (kbd "C-z") (lambda () (interactive) (consoli-config/suspend-if-in-shell)))

;; Text case conversion
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; Line editing
(global-set-key (kbd "C-S-<down>") 'consoli-config/insert-new-line-below)
(global-set-key (kbd "C-S-<up>") 'consoli-config/insert-new-line-above)
(global-set-key (kbd "RET") 'indent-new-comment-line)

;; Special characters
(global-set-key (kbd "ć") (lambda () (interactive) (insert "ç")))
(global-set-key (kbd "Ć") (lambda () (interactive) (insert "Ç")))

;; Terminal keybindings
(defvar-keymap consoli-config/terminal-map
  :doc "Keymap for terminal operations"
  :repeat t
  "t" #'consoli-config/toggle-project-terminal
  "n" #'consoli-config/new-project-terminal
  "s" #'consoli-config/switch-project-terminal
  "k" #'consoli-config/kill-project-terminals
  "f" #'consoli-config/focus-terminal-window)

(global-set-key (kbd "C-`") #'consoli-config/toggle-project-terminal)
(global-set-key (kbd "C-c t") consoli-config/terminal-map)

;; Which-key integration
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements consoli-config/terminal-map
    "t" "toggle terminal"
    "n" "new terminal"
    "s" "switch terminal"
    "k" "kill terminals"
    "f" "focus terminal"))

;; Utility functions
(global-set-key (kbd "<f9>") 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consoli-config/insert-new-line-below ()
  "Insert a new line below the current line without moving cursor."
  (interactive)
  (let ((current-point (point)))
    (move-end-of-line 1)
    (open-line 1)
    (goto-char current-point)))

(defun consoli-config/insert-new-line-above ()
  "Insert a new line above the current line."
  (interactive)
  (let ((current-point (point)))
    (move-beginning-of-line 1)
    (newline-and-indent)
    (indent-according-to-mode)
    (goto-char current-point)
    (forward-char)))

(defun consoli-config/pulse-current-region (&rest _)
  "Pulse the current implicit or active region."
  (cond
   ;; Active region exists
   (mark-active
    (pulse-momentary-highlight-region (region-beginning) (region-end)))
   ;; Mark exists but no active region
   ((and (mark t) (not (= (mark t) (point))))
    (pulse-momentary-highlight-region (mark t) (point)))
   ;; Fallback: pulse current line
   (t
    (pulse-momentary-highlight-one-line (point)))))

(defun consoli-config/pulse-sexp-at-point (&rest _)
  "Pulse the sexp that smartparens will operate on."
  (when (bound-and-true-p smartparens-mode)
    (condition-case nil
        (let ((bounds (cond
                       ;; If we're inside a sexp and at the beginning, get the enclosing sexp
                       ((and (sp-point-in-sexp) (sp-beginning-of-sexp-p))
                        (save-excursion (sp-backward-up-sexp) (sp-get-sexp)))
                       ;; If we're at the end of a sexp, get the sexp we're after
                       ((sp-end-of-sexp-p)
                        (save-excursion (sp-backward-sexp) (sp-get-sexp)))
                       ;; Try to get the sexp immediately following point
                       ((save-excursion (sp-forward-sexp) (sp-get-sexp)))
                       ;; Try to get the sexp immediately before point
                       ((save-excursion (sp-backward-sexp) (sp-get-sexp)))
                       ;; If inside a sexp, get the enclosing one
                       ((sp-point-in-sexp)
                        (save-excursion (sp-backward-up-sexp) (sp-get-sexp)))
                       ;; Last resort: try sp-get-sexp at point
                       (t (sp-get-sexp)))))
          (if bounds
              (pulse-momentary-highlight-region
               (sp-get bounds :beg)
               (sp-get bounds :end))
            ;; Fallback: pulse current line if no sexp found
            (pulse-momentary-highlight-one-line (point))))
      ;; If any smartparens function fails, fallback to current line
      (error (pulse-momentary-highlight-one-line (point))))))

;; Provide the configuration
(provide 'init)

;;; init.el ends here
