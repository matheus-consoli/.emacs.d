;;; tale-themes-common.el --- Enhanced common theme generator for Tale themes -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025
;; Author: Generated theme system
;; Keywords: faces, theme
;; Version: 2.0

;;; Commentary:

;; Enhanced Tale themes common generator with improved color science,
;; accessibility, and comprehensive face coverage for modern Emacs.

;;; Code:

(require 'color)

(defun tale-themes--lighten-color (color amount)
  "Lighten COLOR by AMOUNT (0.0-1.0)."
  (when (and color (> (length color) 0))
    (apply 'color-rgb-to-hex
           (append (color-lighten-hsl
                    (apply 'color-rgb-to-hsl
                           (color-name-to-rgb color))
                    amount)
                   (list 2)))))

(defun tale-themes--darken-color (color amount)
  "Darken COLOR by AMOUNT (0.0-1.0)."
  (when (and color (> (length color) 0))
    (apply 'color-rgb-to-hex
           (append (color-darken-hsl
                    (apply 'color-rgb-to-hsl
                           (color-name-to-rgb color))
                    amount)
                   (list 2)))))

(defun tale-themes--blend-colors (color1 color2 alpha)
  "Blend COLOR1 with COLOR2 using ALPHA (0.0-1.0)."
  (when (and color1 color2 (> (length color1) 0) (> (length color2) 0))
    (let* ((rgb1 (color-name-to-rgb color1))
           (rgb2 (color-name-to-rgb color2))
           (blended (list (+ (* (nth 0 rgb1) (- 1 alpha)) (* (nth 0 rgb2) alpha))
                         (+ (* (nth 1 rgb1) (- 1 alpha)) (* (nth 1 rgb2) alpha))
                         (+ (* (nth 2 rgb1) (- 1 alpha)) (* (nth 2 rgb2) alpha)))))
      (apply 'color-rgb-to-hex (append blended (list 2))))))

(defun tale-themes--create-theme (theme-name palette)
  "Create a theme with THEME-NAME using PALETTE."
  (let-alist palette
    (custom-theme-set-faces
     theme-name
     ;; == CORE UI FACES ==
     `(cursor ((t (:background ,.lavender))))
     `(default ((t (:background ,.bg-main :foreground ,.fg-main))))
     `(fill-column-indicator ((t (:background ,.bg-dim :foreground ,.grey-border))))
     `(fringe ((t (:background ,.bg-main :foreground ,.grey-subtle))))
     `(highlight ((t (:background ,.hover-bg))))
     `(hl-line ((t (:background ,.bg-alt :extend t))))
     `(link ((t (:foreground ,.purple :underline t))))
     `(link-visited ((t (:foreground ,.magenta :underline t))))
     `(minibuffer-prompt ((t (:background unspecified :foreground ,.purple :weight bold))))
     `(region ((t (:background ,.visual-selection :foreground unspecified :extend t))))
     `(secondary-selection ((t (:background ,.secondary-selection :extend t))))
     `(vertical-border ((t (:foreground ,.grey-border))))
     `(window-divider ((t (:foreground ,.bg-dim))))
     `(window-divider-last-pixel ((t (:foreground ,.bg-alt))))
     `(window-divider-first-pixel ((t (:foreground ,.bg-alt))))
     `(success ((t (:foreground ,.success-muted :weight bold))))
     `(warning ((t (:foreground ,.warning-muted :weight bold))))
     `(error ((t (:foreground ,.error-muted :weight bold))))
     `(match ((t (:foreground ,.bg-main :background ,.green :weight bold))))
     `(shadow ((t (:foreground ,.grey-subtle))))

     ;; == ENHANCED UI ELEMENTS ==
     `(tooltip ((t (:background ,.bg-popup :foreground ,.fg-main :box (:line-width 1 :color ,.grey-border)))))
     `(button ((t (:foreground ,.purple :underline t))))
     `(header-line ((t (:background ,.bg-alt :foreground ,.grey-docstring :weight normal))))
     `(widget-field ((t (:background ,.bg-contrast :foreground ,.fg-main))))
     `(help-key-binding ((t (:background ,.bg-special  :foreground ,.hint-fg :box (:line-width (2 . 2) :color ,.bg-special)))))

     `(colorful-base ((t (:box nil))))

     ;; Selection-related faces
     `(lazy-highlight ((t (:background ,.visual-selection :foreground ,.fg-bright))))
     `(isearch ((t (:background ,.purple :foreground ,.bg-main :bold t))))
     `(isearch-fail ((t (:background ,.error-muted :foreground ,.magenta))))
     `(match ((t (:background ,.success-muted :foreground ,.green))))
     `(isearch-overlay ((t (:background ,.purple :foreground ,.bg-main :weight bold))))
     `(lazy-highlight-overlay ((t (:background ,.bg-dim :box (:line-width -1 :color ,.purple)))))
     `(match-overlay ((t (:background ,.bg-selection :foreground ,.green))))
     `(rectangle-preview ((t (:background ,.primary-selection))))
     `(region-inactive ((t (:background ,.inactive-selection))))
     `(evil-ex-lazy-highlight ((t (:background ,.visual-selection :foreground ,.fg-bright))))

     ;; Specific minibuffer-related faces
     `(completions-common-part ((t (:foreground ,.purple))))
     `(completions-first-difference ((t (:foreground ,.magenta))))
     `(completions-annotations ((t (:foreground ,.grey-subtle :slant italic))))
     `(completions-highlight ((t (:background ,.bg-selection :foreground ,.fg-bright))))

     ;; Basic faces
     `(escape-glyph ((t (:foreground ,.yellow))))
     `(homoglyph ((t (:foreground ,.yellow))))
     `(nobreak-hyphen ((t (:foreground ,.yellow))))
     `(nobreak-space ((t (:foreground ,.yellow :underline t))))
     `(trailing-whitespace ((t (:background ,.error-muted :foreground ,.fg-bright))))

     ;; == ENHANCED MODELINE ==
     `(mode-line ((t (:background ,.bg-main :foreground ,.grey-docstring :weight light
                     :box (:line-width (1 . -1) :style flat-button) :inherit nil))))
     `(mode-line-active ((t (:background ,.bg-main :foreground ,.fg-main :weight normal
                            :box (:line-width (1 . -1) :style flat-button) :inherit nil))))
     `(mode-line-inactive ((t (:background ,.bg-special :foreground ,.grey-subtle :weight light
                               :box (:line-width (1 . -1) :style flat-button) :inherit nil))))
     `(mode-line-highlight ((t (:background ,.hover-bg :foreground ,.fg-main :box (:line-width 2 :color ,.purple)))))
     `(mode-line-buffer-id ((t (:foreground ,.purple :weight bold))))
     `(mode-line-emphasis ((t (:foreground ,.green :weight bold))))
     `(mode-line-inactive-buffer-id ((t (:foreground ,.grey-subtle))))

     ;; == ENHANCED FONT LOCK & SYNTAX HIGHLIGHTING ==
     `(font-lock-builtin-face ((t (:foreground ,.magenta))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,.grey-subtle :slant italic))))
     `(font-lock-comment-face ((t (:foreground ,.grey-comment-blue :slant italic))))
     `(font-lock-constant-face ((t (:foreground ,.purple :weight bold))))
     `(font-lock-doc-face ((t (:foreground ,.grey-docstring :slant italic))))
     `(font-lock-function-name-face ((t (:foreground ,.purple :weight bold))))
     `(font-lock-keyword-face ((t (:foreground ,.magenta :weight bold))))
     `(font-lock-negation-char-face ((t (:foreground ,.rose :weight bold))))
     `(font-lock-preprocessor-face ((t (:foreground ,.teal))))
     `(font-lock-regexp-grouping-backslash ((t (:foreground ,.yellow))))
     `(font-lock-regexp-grouping-construct ((t (:foreground ,.magenta))))
     `(font-lock-string-face ((t (:foreground ,.yellow))))
     `(font-lock-type-face ((t (:foreground ,.blue))))
     `(font-lock-variable-name-face ((t (:foreground ,.lavender))))
     `(font-lock-warning-face ((t (:foreground ,.warning-muted :weight bold))))

     ;; == ENHANCED TREE-SITTER SUPPORT ==
     `(treesit-font-lock-function-call-face ((t (:foreground ,.purple))))
     `(treesit-font-lock-variable-use-face ((t (:foreground ,.lavender))))
     `(treesit-font-lock-property-use-face ((t (:foreground ,.blue))))
     `(treesit-font-lock-number-face ((t (:foreground ,.purple :weight bold))))
     `(treesit-font-lock-operator-face ((t (:foreground ,.magenta))))
     `(treesit-font-lock-bracket-face ((t (:foreground ,.grey-border))))
     `(treesit-font-lock-delimiter-face ((t (:foreground ,.grey-border))))
     `(treesit-font-lock-escape-face ((t (:foreground ,.yellow :weight bold))))
     `(treesit-font-lock-constructor-face ((t (:foreground ,.teal :weight bold))))
     `(treesit-font-lock-class-face ((t (:foreground ,.blue :weight bold))))
     `(treesit-font-lock-interface-face ((t (:foreground ,.teal :slant italic))))
     `(treesit-font-lock-enum-face ((t (:foreground ,.magenta :weight bold))))
     `(treesit-font-lock-struct-face ((t (:foreground ,.blue))))
     `(treesit-font-lock-namespace-face ((t (:foreground ,.lavender :weight bold))))
     `(treesit-font-lock-module-face ((t (:foreground ,.purple :underline t))))
     `(treesit-font-lock-macro-face ((t (:foreground ,.yellow :weight bold :slant italic))))
     `(treesit-font-lock-attribute-face ((t (:foreground ,.grey-docstring :slant italic))))
     `(treesit-font-lock-parameter-face ((t (:foreground ,.rose))))
     `(treesit-font-lock-field-face ((t (:foreground ,.teal))))
     `(treesit-font-lock-method-face ((t (:foreground ,.purple :slant italic))))

     ;; Enhanced Tree-sitter Semantic Tokens
     `(treesit-font-lock-class-face ((t (:foreground ,.blue :weight bold))))
     `(treesit-font-lock-interface-face ((t (:foreground ,.teal :slant italic))))
     `(treesit-font-lock-enum-face ((t (:foreground ,.magenta :weight bold))))
     `(treesit-font-lock-struct-face ((t (:foreground ,.blue :weight normal))))
     `(treesit-font-lock-namespace-face ((t (:foreground ,.lavender :weight bold))))
     `(treesit-font-lock-module-face ((t (:foreground ,.purple :underline t))))
     `(treesit-font-lock-macro-face ((t (:foreground ,.yellow :weight bold :slant italic))))
     `(treesit-font-lock-attribute-face ((t (:foreground ,.grey-docstring :slant italic))))
     `(treesit-font-lock-parameter-face ((t (:foreground ,.rose :slant italic))))
     `(treesit-font-lock-field-face ((t (:foreground ,.teal))))
     `(treesit-font-lock-method-face ((t (:foreground ,.purple :slant italic))))

     ;; Whitespace
     `(whitespace-space ((t (:foreground ,.grey-border))))
     `(whitespace-tab ((t (:foreground ,.grey-border))))
     `(whitespace-newline ((t (:foreground ,.grey-border))))
     `(whitespace-trailing ((t (:background ,.error-muted))))
     `(whitespace-empty ((t (:background ,.warning-muted))))

     ;; tab-bar-mode
     `(tab-bar ((t (:background ,.bg-main :foreground ,.fg-main))))
     `(tab-bar-tab-highlight ((t (:background ,.bg-alt :foreground ,.fg-main))))
     `(tab-bar-tab ((t (:background ,.bg-contrast :foreground ,.fg-popup :height 95
                                    :box (:line-width (1 . 5) :color ,.bg-contrast :style flat-button)))))
     `(tab-bar-tab-inactive ((t (:background ,.bg-main :foreground ,.grey-subtle :height 95 :weight extra-light
                                             :box (:line-width (1 . 5) :color ,.bg-main :style flat-button)))))
     `(tab-bar-tab-ungrouped ((t (:background ,.bg-contrast :foreground ,.grey-subtle))))
     `(tab-bar-tab-group-current ((t (:background ,.bg-alt :foreground ,.purple))))
     `(tab-bar-tab-group-inactive ((t (:background ,.bg-contrast :foreground ,.grey-subtle))))

     ;; THIRD PARTY PACKAGES
     ;; web-mode
     `(web-mode-string-face ((t (:foreground ,.yellow))))
     `(web-mode-html-tag-face ((t (:foreground ,.magenta))))
     `(web-mode-html-tag-bracket-face ((t (:foreground ,.magenta))))
     `(web-mode-html-attr-name-face ((t (:foreground ,.yellow))))
     `(web-mode-css-property-name-face ((t (:foreground ,.purple))))
     `(web-mode-css-selector-face ((t (:foreground ,.magenta))))
     `(web-mode-json-key-face ((t (:foreground ,.purple))))
     `(web-mode-json-string-face ((t (:foreground ,.yellow))))

     ;; company-mode
     `(company-tooltip ((t (:background ,.bg-popup :foreground ,.fg-popup))))
     `(company-tooltip-common ((t (:foreground ,.purple))))
     `(company-tooltip-selection ((t (:background ,.bg-selection))))
     `(company-scrollbar-fg ((t (:background ,.grey-border))))
     `(company-scrollbar-bg ((t (:background ,.bg-popup))))

     ;; Enhanced Org-mode
     `(org-level-1 ((t (:foreground ,.lavender :height 1.3 :weight bold))))
     `(org-level-2 ((t (:foreground ,.rose :height 1.2 :weight bold))))
     `(org-level-3 ((t (:foreground ,.teal :height 1.1 :weight bold))))
     `(org-level-4 ((t (:foreground ,.blue :weight bold))))
     `(org-level-5 ((t (:foreground ,.purple :weight normal))))
     `(org-level-6 ((t (:foreground ,.magenta :weight normal))))
     `(org-level-7 ((t (:foreground ,.yellow :weight normal))))
     `(org-level-8 ((t (:foreground ,.green :weight normal))))
     `(org-todo ((t (:foreground ,.magenta :weight bold :box (:line-width 1 :color ,.magenta)))))
     `(org-done ((t (:foreground ,.green :weight bold :box (:line-width 1 :color ,.green)))))
     `(org-priority ((t (:foreground ,.yellow :weight bold))))
     `(org-tag ((t (:foreground ,.purple :weight normal :box (:line-width 1 :color ,.purple :style released-button)))))
     `(org-block ((t (:background ,.bg-code :foreground ,.fg-main :extend t))))
     `(org-block-begin-line ((t (:foreground ,.grey-comment-blue :background ,.bg-alt :extend t :weight bold))))
     `(org-block-end-line ((t (:foreground ,.grey-comment-blue :background ,.bg-alt :extend t :weight bold))))
     `(org-code ((t (:foreground ,.yellow :background ,.bg-contrast))))
     `(org-verbatim ((t (:foreground ,.green :background ,.bg-contrast))))
     `(org-quote ((t (:background ,.bg-contrast :foreground ,.fg-main :slant italic :extend t :border-left (:width 3 :color ,.purple)))))
     `(org-checkbox ((t (:foreground ,.purple :weight bold))))
     `(org-checkbox-done-text ((t (:foreground ,.grey-border :strike-through t))))
     `(org-table ((t (:foreground ,.fg-main :background ,.bg-code))))
     `(org-formula ((t (:foreground ,.yellow :slant italic))))
     `(org-date ((t (:foreground ,.blue :underline t))))
     `(org-footnote ((t (:foreground ,.magenta :underline t))))
     `(org-link ((t (:foreground ,.purple :underline t))))
     `(org-agenda-structure ((t (:foreground ,.purple :weight bold :height 1.1))))
     `(org-agenda-date ((t (:foreground ,.blue :weight bold))))
     `(org-agenda-date-today ((t (:foreground ,.yellow :weight bold :height 1.1))))
     `(org-agenda-date-weekend ((t (:foreground ,.rose :weight normal))))
     `(org-scheduled ((t (:foreground ,.green))))
     `(org-scheduled-today ((t (:foreground ,.yellow :weight bold))))
     `(org-scheduled-previously ((t (:foreground ,.magenta :weight bold))))
     `(org-upcoming-deadline ((t (:foreground ,.yellow :slant italic))))
     `(org-deadline-announce ((t (:foreground ,.magenta :weight bold))))
     `(org-time-grid ((t (:foreground ,.grey-border))))
     `(org-warning ((t (:foreground ,.error-muted :weight bold))))

     `(org-superstar-leading-bullet-face ((t (:foreground ,.grey-subtle))))
     `(org-superstar-item-bullet-face ((t (:foreground ,.purple))))

     ;; Enhanced Org-Roam
     `(org-roam-node-highlight-face ((t (:background ,.focus-bg :foreground ,.lavender :weight bold))))
     `(org-roam-link ((t (:foreground ,.purple :underline t :weight bold))))
     `(org-roam-backlinks-highlight ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(org-roam-title ((t (:foreground ,.lavender :weight bold :height 1.2))))

     ;; Enhanced Org-Modern
     `(org-modern-tag ((t (:foreground ,.blue :background ,.bg-contrast :box (:line-width -1 :color ,.bg-contrast :style released-button) :height 0.9))))
     `(org-modern-priority ((t (:foreground ,.yellow :weight bold :box (:line-width 1 :color ,.yellow)))))
     `(org-modern-date-active ((t (:foreground ,.green :background ,.bg-contrast :box (:line-width 1 :color ,.green)))))
     `(org-modern-date-inactive ((t (:foreground ,.grey-subtle :background ,.bg-alt))))
     `(org-modern-time-active ((t (:foreground ,.blue :background ,.bg-contrast :weight bold))))
     `(org-modern-time-inactive ((t (:foreground ,.grey-subtle :background ,.bg-alt))))
     `(org-modern-done ((t (:foreground ,.green :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.green)))))
     `(org-modern-todo ((t (:foreground ,.magenta :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.magenta)))))

     ;; Enhanced Magit and VC
     `(magit-section-heading ((t (:foreground ,.purple :weight bold :height 1.1))))
     `(magit-section-highlight ((t (:background ,.bg-contrast))))
     `(magit-section-secondary-heading ((t (:foreground ,.blue :weight bold))))
     `(magit-branch-current ((t (:foreground ,.yellow :weight bold :box (:line-width 1 :color ,.yellow)))))
     `(magit-branch-local ((t (:foreground ,.green))))
     `(magit-branch-remote ((t (:foreground ,.purple))))
     `(magit-branch-upstream ((t (:foreground ,.teal :slant italic))))
     `(magit-tag ((t (:foreground ,.magenta :weight bold))))
     `(magit-diff-added ((t (:background ,.diff-added-bg :foreground ,.diff-added-fg))))
     `(magit-diff-removed ((t (:background ,.diff-removed-bg :foreground ,.diff-removed-fg))))
     `(magit-diff-context ((t (:foreground ,.grey-subtle))))
     `(magit-diff-hunk-heading ((t (:background ,.bg-alt :foreground ,.fg-bright :weight bold))))
     `(magit-diff-hunk-heading-highlight ((t (:background ,.bg-selection :foreground ,.fg-bright :weight bold))))
     `(magit-diff-added-highlight ((t (:background ,.diff-added-bg :foreground ,.diff-added-fg :weight bold))))
     `(magit-diff-removed-highlight ((t (:background ,.diff-removed-bg :foreground ,.diff-removed-fg :weight bold))))
     `(magit-diff-context-highlight ((t (:background ,.bg-dim :foreground ,.fg-main))))
     `(magit-blame-highlight ((t (:background ,.bg-contrast :foreground ,.fg-bright))))
     `(magit-blame-heading ((t (:background ,.bg-dim :foreground ,.grey-subtle :weight bold))))
     `(magit-blame-name ((t (:foreground ,.purple :weight bold))))
     `(magit-blame-date ((t (:foreground ,.yellow))))
     `(magit-blame-summary ((t (:foreground ,.fg-bright))))
     `(magit-log-author ((t (:foreground ,.blue))))
     `(magit-log-date ((t (:foreground ,.grey-docstring))))
     `(magit-log-graph ((t (:foreground ,.grey-border))))
     `(magit-reflog-commit ((t (:foreground ,.green))))
     `(magit-reflog-amend ((t (:foreground ,.magenta))))
     `(magit-reflog-merge ((t (:foreground ,.purple))))
     `(magit-reflog-checkout ((t (:foreground ,.blue))))
     `(magit-reflog-reset ((t (:foreground ,.yellow))))
     `(magit-reflog-rebase ((t (:foreground ,.lavender))))
     `(magit-reflog-cherry-pick ((t (:foreground ,.rose))))
     `(magit-reflog-remote ((t (:foreground ,.teal))))
     `(magit-reflog-other ((t (:foreground ,.grey-docstring))))
     `(magit-signature-good ((t (:foreground ,.green :weight bold))))
     `(magit-signature-bad ((t (:foreground ,.magenta :weight bold))))
     `(magit-signature-untrusted ((t (:foreground ,.yellow))))
     `(magit-signature-expired ((t (:foreground ,.grey-subtle))))
     `(magit-signature-revoked ((t (:foreground ,.magenta :strike-through t))))

     ;; Magit Todos Integration
     `(magit-todos-keyword-face ((t (:foreground ,.yellow :weight bold))))
     `(magit-todos-item-face ((t (:foreground ,.fg-main))))

     ;; Conventional Commit
     `(conventional-commit-type-face ((t (:foreground ,.magenta :weight bold))))
     `(conventional-commit-scope-face ((t (:foreground ,.yellow :slant italic))))
     `(conventional-commit-breaking-change-face ((t (:foreground ,.error-muted :weight bold :underline t))))

     `(diff-added ((t (:background ,.diff-added-bg :foreground ,.diff-added-fg))))
     `(diff-removed ((t (:background ,.diff-removed-bg :foreground ,.diff-removed-fg))))
     `(diff-changed ((t (:background ,.diff-changed-bg :foreground ,.diff-changed-fg))))
     `(diff-hl-insert ((t (:foreground ,.diff-added-fg :background ,.diff-added-bg))))
     `(diff-hl-delete ((t (:foreground ,.diff-removed-fg :background ,.diff-removed-bg))))
     `(diff-hl-change ((t (:foreground ,.diff-changed-fg :background ,.diff-changed-bg))))
     `(ediff-current-diff-A ((t (:background ,.diff-removed-bg :foreground ,.diff-removed-fg))))
     `(ediff-current-diff-B ((t (:background ,.diff-added-bg :foreground ,.diff-added-fg))))
     `(ediff-current-diff-C ((t (:background ,.diff-changed-bg :foreground ,.diff-changed-fg))))
     `(ediff-fine-diff-A ((t (:background ,.magenta :foreground ,.bg-main))))
     `(ediff-fine-diff-B ((t (:background ,.green :foreground ,.bg-main))))
     `(ediff-fine-diff-C ((t (:background ,.yellow :foreground ,.bg-main))))
     `(ediff-even-diff-face ((t (:inherit default))))
     `(ediff-odd-diff-face ((t (:background ,.bg-code))))

     `(git-gutter:added ((t (:foreground ,.diff-added-fg))))
     `(git-gutter:deleted ((t (:foreground ,.diff-removed-fg))))
     `(git-gutter:modified ((t (:foreground ,.diff-changed-fg))))

     `(git-gutter-fr:added ((t (:foreground ,.diff-added-fg))))
     `(git-gutter-fr:deleted ((t (:foreground ,.diff-removed-fg))))
     `(git-gutter-fr:modified ((t (:foreground ,.diff-changed-fg))))
     `(transient-argument ((t (:foreground ,.yellow :weight bold))))

     ;; olivetti
     `(olivetti-fringe ((t (:background ,.bg-main))))

     ;; markdown-mode
     `(markdown-header-face ((t (:foreground ,.purple))))
     `(markdown-code-face ((t (:background ,.bg-popup :height 0.9 :weight light))))
     `(markdown-inline-code-face ((t (:height 0.9))))
     `(markdown-link-face ((t (:foreground ,.purple))))

     ;; dired
     `(dired-directory ((t (:foreground ,.purple))))
     `(dired-marked ((t (:foreground ,.yellow))))
     `(dired-flagged ((t (:foreground ,.magenta))))

     ;; eshell
     `(eshell-prompt ((t (:foreground ,.purple))))
     `(eshell-ls-directory ((t (:foreground ,.purple))))
     `(eshell-ls-executable ((t (:foreground ,.green))))
     `(eshell-ls-symlink ((t (:foreground ,.yellow))))
     `(ielm-prompt ((t (:foreground ,.teal :weight bold))))

     ;; Terminal colors
     `(term-color-black ((t (:foreground ,.fg-main :background ,.bg-main))))
     `(term-color-red ((t (:foreground ,.magenta :background ,.magenta))))
     `(term-color-green ((t (:foreground ,.green :background ,.green))))
     `(term-color-yellow ((t (:foreground ,.yellow :background ,.yellow))))
     `(term-color-blue ((t (:foreground ,.purple :background ,.purple))))
     `(term-color-magenta ((t (:foreground ,.purple :background ,.purple))))
     `(term-color-cyan ((t (:foreground ,.green :background ,.green))))
     `(term-color-white ((t (:foreground ,.fg-bright :background ,.fg-bright))))

     ;; ANSI colors
     `(ansi-color-black ((t (:foreground ,.bg-contrast :background ,.bg-contrast))))
     `(ansi-color-red ((t (:foreground ,.magenta :background ,.magenta))))
     `(ansi-color-green ((t (:foreground ,.green :background ,.green))))
     `(ansi-color-yellow ((t (:foreground ,.yellow :background ,.yellow))))
     `(ansi-color-blue ((t (:foreground ,.blue :background ,.blue))))
     `(ansi-color-magenta ((t (:foreground ,.purple :background ,.purple))))
     `(ansi-color-cyan ((t (:foreground ,.teal :background ,.teal))))
     `(ansi-color-white ((t (:foreground ,.fg-bright :background ,.fg-bright))))
     `(ansi-color-bright-black ((t (:foreground ,.grey-subtle :background ,.grey-subtle))))
     `(ansi-color-bright-red ((t (:foreground ,.rose :background ,.rose))))
     `(ansi-color-bright-green ((t (:foreground ,.teal :background ,.teal))))
     `(ansi-color-bright-yellow ((t (:foreground ,.yellow :background ,.yellow))))
     `(ansi-color-bright-blue ((t (:foreground ,.lavender :background ,.lavender))))
     `(ansi-color-bright-magenta ((t (:foreground ,.purple :background ,.purple))))
     `(ansi-color-bright-cyan ((t (:foreground ,.teal :background ,.teal))))
     `(ansi-color-bright-white ((t (:foreground ,.fg-popup :background ,.fg-popup))))

     ;; Vterm faces
     `(vterm-color-default ((t (:foreground ,.fg-bright :background ,.bg-special))))
     `(vterm-color-black ((t (:foreground ,.bg-contrast :background ,.bg-contrast))))
     `(vterm-color-red ((t (:foreground ,.magenta :background ,.magenta))))
     `(vterm-color-green ((t (:foreground ,.green :background ,.green))))
     `(vterm-color-yellow ((t (:foreground ,.yellow :background ,.yellow))))
     `(vterm-color-blue ((t (:foreground ,.blue :background ,.blue))))
     `(vterm-color-magenta ((t (:foreground ,.purple :background ,.purple))))
     `(vterm-color-cyan ((t (:foreground ,.teal :background ,.teal))))
     `(vterm-color-white ((t (:foreground ,.fg-bright :background ,.fg-bright))))
     `(vterm-color-bright-black ((t (:foreground ,.grey-subtle :background ,.grey-subtle))))
     `(vterm-color-bright-red ((t (:foreground ,.rose :background ,.rose))))
     `(vterm-color-bright-green ((t (:foreground ,.teal :background ,.teal))))
     `(vterm-color-bright-yellow ((t (:foreground ,.yellow :background ,.yellow))))
     `(vterm-color-bright-blue ((t (:foreground ,.lavender :background ,.lavender))))
     `(vterm-color-bright-magenta ((t (:foreground ,.purple :background ,.purple))))
     `(vterm-color-bright-cyan ((t (:foreground ,.teal :background ,.teal))))
     `(vterm-color-bright-white ((t (:foreground ,.fg-popup :background ,.fg-popup))))

     ;; Enhanced VTerm Features
     `(vterm-prompt-face ((t (:foreground ,.purple :weight bold))))
     `(vterm-directory-face ((t (:foreground ,.blue :weight bold))))
     `(vterm-command-face ((t (:foreground ,.green))))
     `(vterm-output-face ((t (:foreground ,.fg-main))))
     `(vterm-error-face ((t (:foreground ,.magenta :weight bold))))

     ;; Which-key
     `(which-key-key-face ((t (:foreground ,.purple))))
     `(which-key-separator-face ((t (:foreground ,.grey-subtle))))
     `(which-key-note-face ((t (:foreground ,.grey-subtle))))
     `(which-key-command-description-face ((t (:foreground ,.fg-bright))))
     `(which-key-group-description-face ((t (:foreground ,.yellow))))
     `(which-key-special-key-face ((t (:foreground ,.magenta))))

     ;; ivy/helm
     `(ivy-current-match ((t (:background ,.bg-selection))))
     `(ivy-minibuffer-match-face-1 ((t (:foreground ,.purple))))
     `(helm-selection ((t (:background ,.bg-selection))))
     `(helm-match ((t (:foreground ,.purple))))

     ;; Bookmarks
     `(bookmark-face ((t (:foreground ,.purple :background ,.bg-contrast))))
     `(bookmark-menu-heading ((t (:foreground ,.purple :weight bold))))
     `(bookmark-menu-bookmark ((t (:foreground ,.magenta))))

     ;; Quick-peek faces
     `(quick-peek-background-face ((t (:background ,.bg-popup))))
     `(quick-peek-border-face ((t (:background ,.grey-border))))
     `(quick-peek-padding-face ((t (:background ,.bg-popup))))
     `(quick-peek-header-face ((t (:background ,.bg-alt :foreground ,.fg-bright :height 1.1))))

     ;; Flycheck etc.
     `(flycheck-error ((t (:underline (:style wave :color ,.magenta)))))
     `(flycheck-info ((t (:underline (:style wave :color ,.purple)))))
     `(flycheck-warning ((t (:underline (:style wave :color ,.yellow)))))
     `(flycheck-fringe-error ((t (:foreground ,.magenta))))
     `(flycheck-fringe-warning ((t (:foreground ,.yellow))))
     `(flycheck-fringe-info ((t (:foreground ,.purple))))
     `(flycheck-error-list-error ((t (:foreground ,.magenta))))
     `(flycheck-error-list-warning ((t (:foreground ,.yellow))))
     `(flycheck-error-list-info ((t (:foreground ,.purple))))
     `(flycheck-error-list-line-number ((t (:foreground ,.grey-subtle))))
     `(flycheck-error-list-column-number ((t (:foreground ,.grey-subtle))))
     `(flycheck-error-list-filename ((t (:foreground ,.purple))))
     `(flycheck-error-list-highlight ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(flycheck-verify-select-checker ((t (:background ,.bg-selection))))
     `(flycheck-error-list-checker-name ((t (:foreground ,.yellow))))
     `(flycheck-error-list-id ((t (:foreground ,.purple))))
     `(flycheck-error-list-id-with-explainer ((t (:foreground ,.purple :box (:line-width -1 :color ,.purple)))))
     `(flycheck-inline-error ((t (:background ,.error-muted :foreground ,.magenta :box (:line-width -1 :color ,.magenta)))))
     `(flycheck-inline-warning ((t (:background ,.warning-muted :foreground ,.yellow :box (:line-width -1 :color ,.yellow)))))
     `(flycheck-inline-info ((t (:background ,.bg-contrast :foreground ,.purple :box (:line-width -1 :color ,.purple)))))
     `(flycheck-color-mode-line-error-face ((t (:foreground ,.magenta :weight bold))))
     `(flycheck-color-mode-line-warning-face ((t (:foreground ,.yellow :weight bold))))
     `(flycheck-color-mode-line-info-face ((t (:foreground ,.purple :weight bold))))
     `(flycheck-color-mode-line-running-face ((t (:foreground ,.grey-subtle :weight bold))))
     `(flycheck-color-mode-line-success-face ((t (:foreground ,.green :weight bold))))
     `(flycheck-posframe-face ((t (:foreground ,.fg-bright))))
     `(flycheck-posframe-background-face ((t (:background ,.bg-popup))))
     `(flycheck-posframe-error-face ((t (:foreground ,.magenta :weight bold))))
     `(flycheck-posframe-warning-face ((t (:foreground ,.yellow :weight bold))))
     `(flycheck-posframe-info-face ((t (:foreground ,.purple :weight bold))))
     `(flycheck-posframe-border-face ((t (:foreground ,.grey-border))))
     `(flycheck-error-list-header ((t (:background ,.bg-alt :foreground ,.fg-bright :weight bold))))
     `(flycheck-error-list-separator ((t (:foreground ,.grey-border))))
     `(flycheck-error-list-checkername-face ((t (:foreground ,.purple))))
     `(flycheck-inline-message ((t (:inherit default :slant italic))))
     `(flycheck-inline-suggestion ((t (:foreground ,.green :slant italic))))
     `(flycheck-inline-explanation ((t (:foreground ,.grey-subtle :slant italic))))
     `(flycheck-checker-disabled-face ((t (:foreground ,.grey-subtle :strike-through t))))
     `(flycheck-checker-running-face ((t (:foreground ,.yellow :slant italic))))
     `(flycheck-checker-success-face ((t (:foreground ,.green :weight bold))))
     `(flycheck-checker-error-face ((t (:foreground ,.magenta :weight bold))))
     `(flycheck-checker-warning-face ((t (:foreground ,.yellow :weight bold))))
     `(flycheck-checker-info-face ((t (:foreground ,.purple :weight bold))))
     `(flycheck-error-overlay ((t (:underline (:style wave :color ,.magenta) :background ,.bg-contrast))))
     `(flycheck-warning-overlay ((t (:underline (:style wave :color ,.yellow) :background ,.bg-contrast))))
     `(flycheck-info-overlay ((t (:underline (:style wave :color ,.purple) :background ,.bg-contrast))))
     `(flycheck-overlay-error ((t :background ,.error-muted :foreground ,.bg-inactive-sel :height 0.9 :weight normal)))
     `(flycheck-overlay-warning ((t :background ,.warning-muted :foreground ,.bg-inactive-sel :height 0.9 :weight normal)))
     `(flycheck-overlay-info ((t :background ,.info-muted :foreground ,.bg-inactive-sel :height 0.9 :weight normal)))

     ;; Overlays
     `(overlay ((t (:background ,.bg-contrast))))
     `(pos-tip-overlay ((t (:background ,.bg-popup :foreground ,.fg-main))))
     `(eldoc-highlight-function-argument-overlay ((t (:foreground ,.purple :weight bold))))
     `(eldoc-box-border ((t (:background ,.bg-popup))))
     `(eldoc-box-body ((t (:background ,.bg-popup :foreground ,.fg-main))))
     `(eldoc-highlight-function-argument ((t (:foreground ,.purple :weight bold))))
     `(mc/cursor-overlay ((t (:background ,.purple :foreground ,.bg-main))))
     `(mc/cursor-bar-overlay ((t (:background ,.purple :height 1))))
     `(rectangle-overlay ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(semantic-highlight-func-overlay ((t (:background ,.bg-selection))))
     `(semantic-tag-overlay ((t (:background ,.bg-contrast :box t))))
     `(highlight-changes ((t (:foreground ,.yellow :background ,.bg-contrast :underline t))))
     `(highlight-changes-delete ((t (:foreground ,.magenta :background ,.bg-contrast :strike-through t))))
     `(transient-mark-overlay ((t (:background ,.bg-selection))))
     `(secondary-selection-overlay ((t (:background ,.bg-dim :foreground ,.fg-bright))))
     `(avy-overlay-lead-face ((t (:background ,.purple :foreground ,.bg-main :weight bold))))
     `(avy-overlay-background-face ((t (:background ,.bg-code))))
     `(pulse-highlight-face ((t (:background ,.secondary-selection))))
     `(pulse-highlight-start-face ((t (:background ,.visual-selection))))
     `(multiple-cursors-cursor ((t (:background ,.magenta :foreground ,.bg-main))))
     `(highlight-symbol-face ((t (:background ,.focus-bg :foreground ,.lavender))))

     ;; Enhanced Blamer
     `(blamer-face ((t (:foreground ,.grey-subtle :background unspecified :italic t :height 0.8 :weight light))))
     `(blamer-pretty-border-face ((t (:foreground ,.grey-border))))
     `(blamer-pretty-commit-message-face ((t (:foreground ,.grey-docstring :slant italic))))
     `(blamer-pretty-meta-keywords-face ((t (:foreground ,.purple :weight bold))))
     `(blamer-pretty-meta-data-face ((t (:foreground ,.yellow))))

     ;; centaur-tabs
     `(centaur-tabs-active-bar-face ((t (:background ,.purple))))
     `(centaur-tabs-close-mouse-face ((t (:foreground ,.magenta))))
     `(centaur-tabs-close-selected ((t (:inherit centaur-tabs-selected :foreground ,.fg-bright))))
     `(centaur-tabs-close-unselected ((t (:inherit centaur-tabs-unselected :foreground ,.grey-subtle))))
     `(centaur-tabs-default ((t (:background ,.bg-main :foreground ,.fg-main :height 0.9))))
     `(centaur-tabs-group-face ((t (:foreground ,.purple))))
     `(centaur-tabs-modified-marker-selected ((t (:inherit centaur-tabs-selected :foreground ,.magenta :italic t))))
     `(centaur-tabs-modified-marker-unselected ((t (:inherit centaur-tabs-unselected :foreground ,.magenta :italic t))))
     `(centaur-tabs-name-mouse-face ((t (:foreground ,.fg-bright :bold t))))
     `(centaur-tabs-selected ((t (:background ,.bg-alt :foreground ,.fg-bright))))
     `(centaur-tabs-selected-modified ((t (:background ,.bg-alt :foreground ,.yellow))))
     `(centaur-tabs-unselected ((t (:background ,.bg-contrast :foreground ,.grey-subtle))))
     `(centaur-tabs-unselected-modified ((t (:background ,.bg-contrast :foreground ,.yellow))))
     `(centaur-tabs-bar ((t (:background ,.bg-main))))
     `(tab-line ((t (:background ,.bg-dim))))

     ;; LSP mode
     `(lsp-face-highlight-read ((t (:background ,.focus-bg))))
     `(lsp-face-highlight-textual ((t (:background ,.focus-bg))))
     `(lsp-face-highlight-write ((t (:background ,.focus-bg))))
     `(lsp-headerline-breadcrumb-path-error-face ((t (:foreground ,.magenta))))
     `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,.grey-subtle))))
     `(lsp-headerline-breadcrumb-path-hint-face ((t (:foreground ,.green))))
     `(lsp-headerline-breadcrumb-path-info-face ((t (:foreground ,.purple))))
     `(lsp-headerline-breadcrumb-path-warning-face ((t (:foreground ,.yellow))))
     `(lsp-headerline-breadcrumb-project-prefix-face ((t (:foreground ,.purple))))
     `(lsp-headerline-breadcrumb-symbols-face ((t (:foreground ,.yellow))))
     `(lsp-modeline-code-actions-face ((t (:foreground ,.yellow))))
     `(lsp-ui-doc-background ((t (:background ,.bg-popup))))
     `(lsp-ui-doc-border ((t (:background ,.grey-border))))
     `(lsp-ui-doc-header ((t (:background ,.bg-alt :foreground ,.fg-bright))))
     `(lsp-ui-peek-filename ((t (:foreground ,.purple))))
     `(lsp-ui-peek-footer ((t (:background ,.bg-alt))))
     `(lsp-ui-peek-header ((t (:background ,.bg-alt :foreground ,.fg-bright))))
     `(lsp-ui-peek-highlight ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(lsp-ui-peek-line-number ((t (:foreground ,.grey-subtle))))
     `(lsp-ui-peek-list ((t (:background ,.bg-popup))))
     `(lsp-ui-peek-peek ((t (:background ,.bg-popup))))
     `(lsp-ui-peek-selection ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(lsp-ui-sideline-code-action ((t (:foreground ,.yellow))))
     `(lsp-ui-sideline-current-symbol ((t (:foreground ,.purple :weight bold))))
     `(lsp-ui-sideline-symbol ((t (:foreground ,.purple))))
     `(lsp-ui-sideline-symbol-info ((t (:foreground ,.grey-subtle :slant italic))))
     `(lsp-inlay-hint-face ((t (:foreground ,.hint-fg :slant italic :height 0.9))))
     `(lsp-inlay-hint-type-face ((t (:foreground ,.grey-subtle :slant italic :height 0.9))))
     `(lsp-inline-completion-overlay-face ((t (:foreground ,.grey-subtle :background ,.completion-bg :slant italic))))
     `(lsp-signature-highlight-function-argument ((t (:foreground ,.purple :weight bold :underline (:style line :color ,.purple)))))

     ;; DAP (Debug Adapter Protocol) faces
     `(dap-ui-breakpoint-verified-fringe ((t (:foreground ,.green))))
     `(dap-ui-pending-breakpoint-face ((t (:foreground ,.yellow))))
     `(dap-ui-marker-face ((t (:background ,.bg-selection))))
     `(dap-ui-compile-errline ((t (:foreground ,.magenta))))
     `(dap-ui-compile-warnline ((t (:foreground ,.yellow))))

     ;; Corfu, Vertico, and friends
     `(corfu-default ((t (:background ,.bg-popup :foreground ,.fg-popup :height 0.9))))
     `(corfu-current ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(corfu-bar ((t (:background ,.grey-border))))
     `(corfu-border ((t (:background ,.bg-special))))
     `(corfu-deprecated ((t (:foreground ,.grey-subtle :strike-through t))))
     `(corfu-annotation ((t (:foreground ,.grey-subtle :slant italic))))
     `(corfu-echo ((t (:foreground ,.grey-subtle :slant italic))))
     `(corfu-metadata-face ((t (:foreground ,.grey-subtle))))
     `(corfu-documentation-face ((t (:foreground ,.grey-subtle))))
     `(corfu-quick-face ((t (:foreground ,.yellow))))
     `(corfu-quick1-face ((t (:foreground ,.purple))))
     `(corfu-quick2-face ((t (:foreground ,.magenta))))
     `(corfu-quick1 ((t (:foreground ,.purple :weight bold))))
     `(corfu-quick2 ((t (:foreground ,.magenta :weight bold))))
     `(corfu-quick3 ((t (:foreground ,.yellow :weight bold))))

     ;; Enhanced Vertico
     `(vertico-current ((t (:box (:line-width (-1 . -1) :color ,.bg-selection)))))
     `(vertico-group-title ((t (:foreground ,.purple :weight bold :height 1.1 :underline t))))
     `(vertico-group-separator ((t (:foreground ,.grey-border :strike-through t :height 0.8))))
     `(vertico-multiline ((t (:foreground ,.grey-subtle :slant italic))))
     `(vertico-quick1 ((t (:foreground ,.purple :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.purple)))))
     `(vertico-quick2 ((t (:foreground ,.magenta :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.magenta)))))
     `(vertico-quick3 ((t (:foreground ,.yellow :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.yellow)))))
     `(vertico-posframe-border ((t (:background ,.bg-popup :foreground ,.grey-border))))
     `(vertico-posframe ((t (:background ,.bg-popup))))

     ;; Enhanced Marginalia
     `(marginalia-key ((t (:foreground ,.purple :weight bold))))
     `(marginalia-type ((t (:foreground ,.magenta :slant italic))))
     `(marginalia-modified ((t (:foreground ,.yellow :weight bold))))
     `(marginalia-file-name ((t (:foreground ,.fg-main))))
     `(marginalia-file-owner ((t (:foreground ,.grey-subtle :slant italic))))
     `(marginalia-file-priv-no ((t (:foreground ,.grey-subtle))))
     `(marginalia-file-priv-dir ((t (:foreground ,.purple :weight bold))))
     `(marginalia-file-priv-exec ((t (:foreground ,.green :weight bold))))
     `(marginalia-file-priv-link ((t (:foreground ,.yellow :underline t))))
     `(marginalia-file-priv-read ((t (:foreground ,.blue))))
     `(marginalia-file-priv-write ((t (:foreground ,.magenta))))
     `(marginalia-size ((t (:foreground ,.grey-docstring))))
     `(marginalia-number ((t (:foreground ,.purple :weight bold))))
     `(marginalia-char ((t (:foreground ,.lavender))))
     `(marginalia-date ((t (:foreground ,.grey-docstring :slant italic))))
     `(marginalia-documentation ((t (:foreground ,.grey-subtle :slant italic :height 0.9))))
     `(marginalia-function ((t (:foreground ,.purple :weight normal))))
     `(marginalia-mode ((t (:foreground ,.magenta :slant italic))))
     `(marginalia-variable ((t (:foreground ,.lavender))))
     `(marginalia-version ((t (:foreground ,.green :weight bold))))

     ;; Nerd Icons Integration
     `(nerd-icons-completion-dir-face ((t (:foreground ,.purple))))
     `(nerd-icons-completion-file-face ((t (:foreground ,.fg-main))))
     `(nerd-icons-corfu-dir-face ((t (:foreground ,.purple))))
     `(nerd-icons-corfu-file-face ((t (:foreground ,.fg-main))))

     `(orderless-match-face-0 ((t (:foreground ,.purple :weight bold))))
     `(orderless-match-face-1 ((t (:foreground ,.magenta :weight bold))))
     `(orderless-match-face-2 ((t (:foreground ,.green :weight bold))))
     `(orderless-match-face-3 ((t (:foreground ,.yellow :weight bold))))

     ;; xref
     `(xref-file-header ((t (:foreground ,.purple :weight bold))))
     `(xref-line-number ((t (:foreground ,.grey-subtle))))
     `(xref-match ((t (:background ,.bg-selection :foreground ,.fg-bright))))

     ;; Treemacs
     `(treemacs-root-face ((t (:foreground ,.purple :weight bold :height 1.2))))
     `(treemacs-root-unreadable-face ((t (:foreground ,.grey-subtle :strike-through t))))
     `(treemacs-directory-face ((t (:foreground ,.fg-bright))))
     `(treemacs-directory-collapsed-face ((t (:foreground ,.fg-bright))))
     `(treemacs-file-face ((t (:foreground ,.fg-main))))
     `(treemacs-git-modified-face ((t (:foreground ,.magenta))))
     `(treemacs-git-added-face ((t (:foreground ,.green))))
     `(treemacs-git-conflict-face ((t (:foreground ,.magenta))))
     `(treemacs-git-untracked-face ((t (:foreground ,.yellow))))
     `(treemacs-git-renamed-face ((t (:foreground ,.purple))))
     `(treemacs-git-ignored-face ((t (:foreground ,.grey-subtle))))
     `(treemacs-term-node-face ((t (:foreground ,.purple))))
     `(treemacs-tags-face ((t (:foreground ,.yellow))))
     `(treemacs-help-title-face ((t (:foreground ,.purple :weight bold))))
     `(treemacs-help-column-face ((t (:foreground ,.grey-subtle))))
     `(treemacs-on-success-pulse-face ((t (:background ,.green :foreground ,.bg-main))))
     `(treemacs-on-failure-pulse-face ((t (:background ,.magenta :foreground ,.bg-main))))
     `(treemacs-fringe-indicator-face ((t (:foreground ,.purple))))
     `(treemacs-header-button-face ((t (:foreground ,.grey-subtle))))
     `(treemacs-marked-file-face ((t (:foreground ,.yellow :weight bold))))
     `(treemacs-file-extension-face ((t (:foreground ,.fg-main))))
     `(treemacs-modeline-selected-face ((t (:foreground ,.purple :weight bold))))
     `(treemacs-modeline-active-face ((t (:foreground ,.fg-bright))))
     `(treemacs-git-commit-diff-face ((t (:foreground ,.magenta))))
     `(treemacs-git-push-face ((t (:foreground ,.green))))
     `(treemacs-git-pull-face ((t (:foreground ,.yellow))))

     ;; doom-modeline
     `(doom-modeline-bar ((t (:background ,.bg-alt :foreground ,.fg-bright))))
     `(doom-modeline-buffer-file ((t (:foreground ,.fg-bright))))
     `(doom-modeline-buffer-major-mode ((t (:foreground ,.purple :weight bold))))
     `(doom-modeline-buffer-minor-mode ((t (:foreground ,.grey-subtle))))
     `(doom-modeline-buffer-modified ((t (:foreground ,.magenta :weight bold))))
     `(doom-modeline-buffer-path ((t (:foreground ,.purple))))
     `(doom-modeline-buffer-timemachine ((t (:foreground ,.yellow))))
     `(doom-modeline-debug ((t (:foreground ,.yellow))))
     `(doom-modeline-error ((t (:foreground ,.magenta))))
     `(doom-modeline-evil-emacs-state ((t (:foreground ,.magenta))))
     `(doom-modeline-evil-insert-state ((t (:foreground ,.green))))
     `(doom-modeline-evil-motion-state ((t (:foreground ,.grey-subtle))))
     `(doom-modeline-evil-normal-state ((t (:foreground ,.purple))))
     `(doom-modeline-evil-visual-state ((t (:foreground ,.yellow))))
     `(doom-modeline-inactive-bar ((t (:background ,.bg-contrast :foreground ,.fg-bright))))
     `(doom-modeline-info ((t (:foreground ,.green))))
     `(doom-modeline-lsp-error ((t (:foreground ,.magenta))))
     `(doom-modeline-lsp-success ((t (:foreground ,.green))))
     `(doom-modeline-lsp-warning ((t (:foreground ,.yellow))))
     `(doom-modeline-persp-name ((t (:foreground ,.purple))))
     `(doom-modeline-project-dir ((t (:foreground ,.yellow :weight bold))))
     `(doom-modeline-project-parent-dir ((t (:foreground ,.grey-subtle))))
     `(doom-modeline-project-root ((t (:foreground ,.purple))))
     `(doom-modeline-repl-error ((t (:foreground ,.magenta))))
     `(doom-modeline-repl-success ((t (:foreground ,.green))))
     `(doom-modeline-repl-warning ((t (:foreground ,.yellow))))
     `(doom-modeline-warning ((t (:foreground ,.yellow))))
     `(doom-modeline-workspace-number ((t (:foreground ,.magenta))))

     ;; mood line
     `(mood-line-buffer-name ((t (:foreground ,.fg-bright :weight light))))
     `(mood-line-buffer-status-modified ((t (:foreground ,.rose))))
     `(mood-line-buffer-status-read-only ((t (:foreground ,.yellow))))
     `(mood-line-buffer-status-narrowed ((t (:foreground ,.purple))))
     `(mood-line-major-mode ((t (:foreground ,.lavender :weight light))))
     `(mood-line-minor-mode ((t (:foreground ,.grey-subtle))))
     `(mood-line-process ((t (:foreground ,.teal :slant italic))))
     `(mood-line-client ((t (:foreground ,.blue :weight bold))))
     `(mood-line-frame ((t (:foreground ,.purple))))
     `(mood-line-status-neutral ((t (:foreground ,.grey-docstring))))
     `(mood-line-status-info ((t (:foreground ,.teal :weight semi-bold))))
     `(mood-line-status-success ((t (:foreground ,.green :weight bold))))
     `(mood-line-status-warning ((t (:foreground ,.yellow :weight bold))))
     `(mood-line-status-error ((t (:foreground ,.magenta :weight bold))))
     `(mood-line-encoding ((t (:foreground ,.grey-subtle :weight light))))
     `(mood-line-unimportant ((t (:foreground ,.grey-subtle :weight light))))
     `(mood-line-segment-modal ((t (:foreground ,.purple :weight light))))
     `(mood-line-segment-buffer ((t (:foreground ,.fg-bright))))
     `(mood-line-segment-position ((t (:foreground ,.blue))))
     `(mood-line-segment-misc ((t (:foreground ,.grey-docstring :weight light))))

     ;; Line numbers
     `(line-number ((t (:foreground ,.bg-inactive-sel :background ,.bg-main :box (:line-width (2 . 3) :color ,.bg-main)))))
     `(line-number-current-line ((t (:foreground ,.purple :box (:line-width (1 . 3) :color ,.bg-main)))))
     `(line-number-major-tick ((t (:foreground ,.yellow :background ,.bg-main :weight bold))))
     `(line-number-minor-tick ((t (:foreground ,.grey-subtle :background ,.bg-main :weight normal :box (:line-width (-1 . 3))))))
     ;;`(linum ((t (:foreground ,.grey-subtle :background ,.bg-main :slant normal :height 0.9))))
     ;;`(linum-relative-current-face ((t (:foreground ,.purple :background ,.bg-contrast :weight bold :height 1.0))))
     ;;`(line-number-current-line-hl ((t (:foreground ,.purple :background ,.bg-selection :weight bold :height 1.0))))
     ;;`(line-number-relative ((t (:foreground ,.grey-subtle :background ,.bg-main :height 0.9))))
     `(line-number-relative-current ((t (:foreground ,.purple :background ,.bg-contrast :weight bold :height 1.0))))

     ;; Parentheses
     `(show-paren-match ((t (:foreground ,.green :weight bold))))
     `(show-paren-match-expression ((t (:background ,.bg-contrast))))
     `(show-paren-mismatch ((t (:background ,.error-muted :foreground ,.fg-popup :weight bold :strike-through t))))
     `(sp-pair-overlay-face ((t (:background ,.bg-contrast))))
     `(sp-show-pair-match-face ((t (:foreground ,.visual-selection :weight ultra-bold))))
     `(sp-show-pair-mismatch-face ((t (:background ,.magenta :foreground ,.fg-bright :weight bold))))
     `(sp-wrap-overlay-face ((t (:background ,.bg-contrast))))
     `(sp-wrap-tag-overlay-face ((t (:background ,.bg-contrast))))
     `(rainbow-delimiters-depth-1-face ((t (:foreground ,.magenta))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,.purple))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,.green))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,.yellow))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,.blue))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,.magenta))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,.purple))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,.green))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,.yellow))))
     `(rainbow-delimiters-unmatched-face ((t (:background ,.error-muted :foreground ,.fg-bright :strike-through t))))
     `(rainbow-delimiters-mismatched-face ((t (:background ,.error-muted :foreground ,.fg-bright :strike-through t))))

     ;; Sideline packages
     `(sideline-flycheck-error-face ((t (:foreground ,.magenta))))
     `(sideline-flycheck-warning-face ((t (:foreground ,.yellow))))
     `(sideline-flycheck-info-face ((t (:foreground ,.purple))))
     `(sideline-lsp-face ((t (:foreground ,.hint-fg))))

     ;; Avy
     `(avy-highlight-face ((t (:foreground ,.magenta :weight bold))))

     ;; Treesit-jump
     `(treesit-jump-hl-line-flash-face ((t (:background ,.visual-selection))))

     ;; Embark
     `(embark-target-face ((t (:foreground ,.purple :weight bold))))
     `(embark-keybinding-face ((t (:foreground ,.yellow))))
     `(embark-collect-match-face ((t (:background ,.focus-bg))))

     ;; Breadcrumb package faces
     `(breadcrumb-face ((t (:foreground ,.grey-subtle :height 0.9 :weight light :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-imenu-crumbs-face ((t (:foreground ,.lavender :height 0.9 :weight normal :slant italic :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-imenu-leaf-face ((t (:foreground ,.purple :height 0.85 :weight semi-bold :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-project-crumbs-face ((t (:foreground ,.grey-docstring :height 0.9 :weight light :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-project-base-face ((t (:foreground ,.blue :height 0.9 :weight normal :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-project-leaf-face ((t (:foreground ,.fg-popup :height 0.85 :weight semi-bold :box (:line-width (2 . 8) :color ,.bg-alt)))))

     ;; Enhanced Breadcrumb Context
     `(breadcrumb-separator ((t (:foreground ,.grey-border :height 0.8))))
     `(breadcrumb-function-face ((t (:foreground ,.purple :weight bold))))
     `(breadcrumb-class-face ((t (:foreground ,.blue :weight bold))))
     `(breadcrumb-method-face ((t (:foreground ,.magenta :slant italic))))
     `(breadcrumb-variable-face ((t (:foreground ,.lavender))))
     `(breadcrumb-property-face ((t (:foreground ,.teal))))

     ;; Misc Packages
     `(yas-field-highlight-face ((t (:background ,.bg-selection))))
     `(yas-snippet-field ((t (:background ,.focus-bg :foreground ,.purple :box (:line-width -1 :color ,.purple)))))

     `(consult-preview-line ((t (:background ,.bg-contrast))))
     `(consult-highlight-match-face ((t (:foreground ,.purple :weight bold))))
     `(consult-file ((t (:foreground ,.fg-main))))
     `(consult-line-number ((t (:foreground ,.grey-subtle))))
     `(consult-separator ((t (:foreground ,.grey-border))))
     `(consult-async-separator ((t (:foreground ,.purple))))
     `(consult-narrow-indicator ((t (:foreground ,.purple :weight bold))))
     `(consult-bookmark ((t (:foreground ,.magenta))))
     `(copilot-ghost-text-face ((t (:foreground ,.grey-border :slant italic))))
     `(copilot-highlight-face ((t (:background ,.focus-bg))))
     `(aidermacs-diff-face-added ((t (:inherit diff-added))))
     `(aidermacs-diff-face-removed ((t (:inherit diff-removed))))
     `(aidermacs-diff-face-changed ((t (:inherit diff-changed))))
     `(aidermacs-chat-prompt-face ((t (:foreground ,.purple :weight bold))))
     `(aidermacs-chat-user-prompt-face ((t (:foreground ,.teal :weight bold))))
     `(aidermacs-chat-response-face ((t (:foreground ,.fg-main))))
     `(aidermacs-chat-model-face ((t (:foreground ,.yellow :slant italic))))
     `(aidermacs-architect-mode-line-face ((t (:foreground ,.teal :weight bold))))

     `(hl-todo ((t (:foreground ,.yellow :weight bold))))
     `(hl-fixme ((t (:foreground ,.magenta :weight bold))))
     `(hl-note ((t (:foreground ,.blue :weight bold))))
     `(hl-done ((t (:foreground ,.green :weight bold))))

     `(deft-current-line-face ((t (:background ,.bg-contrast))))
     `(deft-file-face ((t (:foreground ,.purple))))
     `(deft-title-face ((t (:foreground ,.fg-main :weight bold))))
     `(deft-summary-face ((t (:foreground ,.grey-subtle :slant italic))))


     `(forge-topic-title-face ((t (:foreground ,.fg-main :weight bold))))
     `(forge-pr-author-face ((t (:foreground ,.purple))))
     `(forge-label-face ((t (:background ,.bg-selection :foreground ,.fg-main :box (:line-width -1 :color ,.bg-selection :style released-button) :height 0.9))))

     `(github-review-comment-face ((t (:background ,.bg-contrast))))
     `(github-review-suggestion-face ((t (:background ,.diff-changed-bg :foreground ,.diff-changed-fg))))

     `(expand-region ((t (:background ,.visual-selection))))
     `(er--highlight-face ((t (:background ,.visual-selection))))
     `(symbol-overlay-default-face ((t (:background ,.focus-bg))))
     `(symbol-overlay-current-face ((t (:background ,.purple :foreground ,.bg-main))))
     `(treesitter-context-face ((t (:background ,.bg-code :foreground ,.grey-subtle :height 0.9))))
     `(treesit-fold-face ((t (:foreground ,.grey-border))))
     `(cognitive-complexity-highlight-face ((t (:background ,.warning-muted :foreground ,.bg-main))))
     `(vr/match-face ((t (:background ,.purple :foreground ,.bg-main))))
     `(better-jumper-marker-face ((t (:foreground ,.rose :underline t))))
     `(consoli-config-rust-macro-face ((t (:inherit font-lock-function-call-face :foreground ,.magenta :slant italic :weight bold :underline (:style line :color ,.magenta)))))
     `(consoli-config-rust-attribute-face ((t (:inherit font-lock-preprocessor-face :foreground ,.lavender :slant italic))))
     `(jinx-correcting-face ((t (:underline (:style wave :color ,.yellow)))))
     `(jinx-suggestion-face ((t (:foreground ,.green :weight bold))))
     `(undo-tree-visualizer-current-face ((t (:foreground ,.green :weight bold))))
     `(undo-tree-visualizer-active-branch-face ((t (:foreground ,.purple))))
     `(undo-tree-visualizer-register-face ((t (:foreground ,.yellow))))
     `(undo-fu-session ((t (:foreground ,.green :background ,.bg-contrast :slant italic))))
     `(annotate-highlight-face ((t (:underline (:style line :color ,.purple)))))
     `(annotate-annotation-text-face ((t (:foreground ,.grey-subtle :slant italic :height 0.9))))
     `(ibuffer-current-buffer-face ((t (:background ,.bg-contrast :weight bold))))
     `(ibuffer-marked-face ((t (:foreground ,.magenta :weight bold))))

     ;; Dashboard faces
     `(dashboard-banner ((t (:foreground ,.purple))))
     `(dashboard-heading ((t (:foreground ,.yellow :bold t))))
     `(dashboard-project-title ((t (:foreground ,.blue :bold t))))
     `(dashboard-item-highlight ((t (:background ,.focus-bg))))

     ;; Eglot diagnostic faces
     `(eglot-highlight-symbol-face ((t (:background ,.focus-bg))))
     `(eglot-mode-line ((t (:foreground ,.purple :weight bold))))
     `(eglot-diagnostic-tag-deprecated-face ((t (:strike-through t :foreground ,.grey-subtle))))
     `(eglot-diagnostic-tag-unnecessary-face ((t (:foreground ,.grey-subtle :slant italic))))
     ;; Eglot inlay hints
     `(eglot-inlay-hint-face ((t (:foreground ,.hint-fg :background unspecified :slant italic :height 0.9))))
     `(eglot-type-hint-face ((t (:foreground ,.hint-fg :background unspecified :slant italic :height 0.9))))
     `(eglot-parameter-hint-face ((t (:foreground ,.hint-fg :background unspecified :slant italic :height 0.9))))
     ;; Eglot code actions (appears in modeline and overlays)
     `(eglot-code-action-indicator-face ((t (:foreground ,.yellow :background ,.bg-main))))

     ;; Calendar faces
     `(calendar-today ((t (:foreground ,.yellow :weight bold :underline t))))
     `(diary ((t (:foreground ,.rose :slant italic))))

     ;; Multimedia faces
     `(pdf-view-link ((t (:foreground ,.blue :underline t))))
     `(emms-playlist-selected-face ((t (:background ,.bg-selection :foreground ,.fg-bright :weight bold))))

     ;; Modern Package Enhancements
     ;; Eglot Booster
     `(eglot-booster-mode-line ((t (:foreground ,.green :weight bold))))

     ;; Claude Code IDE
     `(claude-code-ide-header ((t (:foreground ,.purple :weight bold))))
     `(claude-code-ide-prompt ((t (:foreground ,.yellow :weight bold))))
     `(claude-code-ide-response ((t (:foreground ,.fg-main))))
     `(claude-code-ide-error ((t (:foreground ,.magenta :weight bold))))

     ;; Treesit-Auto
     `(treesit-auto-install-progress ((t (:foreground ,.blue :slant italic))))
     `(treesit-auto-install-success ((t (:foreground ,.green :weight bold))))
     `(treesit-auto-install-error ((t (:foreground ,.magenta :weight bold))))

     ;; Tabspaces
     `(tabspaces-active-tab ((t (:background ,.bg-selection :foreground ,.purple :weight bold))))
     `(tabspaces-inactive-tab ((t (:background ,.bg-contrast :foreground ,.grey-subtle))))
     `(tabspaces-workspace-indicator ((t (:foreground ,.yellow :weight bold))))

     ;; Project Tab Groups
     `(project-tab-group-current ((t (:foreground ,.purple :weight bold))))
     `(project-tab-group-inactive ((t (:foreground ,.grey-subtle))))

     ;; Clipetty (terminal clipboard)
     `(clipetty-success ((t (:foreground ,.green :weight bold))))
     `(clipetty-error ((t (:foreground ,.magenta :weight bold))))

     ;; Exec Path From Shell
     `(exec-path-from-shell-info ((t (:foreground ,.blue :slant italic))))
     `(exec-path-from-shell-warn ((t (:foreground ,.yellow :weight bold))))

     ;; Additional Modern Enhancements
     ;; Drag Stuff
     `(drag-stuff-preview-face ((t (:background ,.bg-selection :foreground ,.fg-bright))))

     ;; Multiple Cursors Enhanced
     `(mc/cursor-face ((t (:background ,.purple :foreground ,.bg-main :weight bold))))
     `(mc/region-face ((t (:background ,.visual-selection))))

     ;; Symbol Overlay Enhanced
     `(symbol-overlay-default-face ((t (:background ,.focus-bg :box (:line-width 1 :color ,.purple)))))
     `(symbol-overlay-face-1 ((t (:background ,.purple :foreground ,.bg-main))))
     `(symbol-overlay-face-2 ((t (:background ,.magenta :foreground ,.bg-main))))
     `(symbol-overlay-face-3 ((t (:background ,.green :foreground ,.bg-main))))
     `(symbol-overlay-face-4 ((t (:background ,.yellow :foreground ,.bg-main))))
     `(symbol-overlay-face-5 ((t (:background ,.blue :foreground ,.bg-main))))
     `(symbol-overlay-face-6 ((t (:background ,.teal :foreground ,.bg-main))))
     `(symbol-overlay-face-7 ((t (:background ,.rose :foreground ,.bg-main))))
     `(symbol-overlay-face-8 ((t (:background ,.lavender :foreground ,.bg-main))))

     ;; Smartparens Enhanced
     `(sp-pair-overlay-face ((t (:background ,.bg-selection))))
     `(sp-wrap-overlay-opening-pair ((t (:background ,.purple :foreground ,.bg-main))))
     `(sp-wrap-overlay-closing-pair ((t (:background ,.purple :foreground ,.bg-main))))
     `(sp-wrap-overlay-face ((t (:background ,.bg-contrast))))

     ;; Better Jumper
     `(better-jumper-visited-face ((t (:background ,.focus-bg :foreground ,.purple))))

     ;; Treesit-Fold
     `(treesit-fold-replacement-face ((t (:foreground ,.grey-border :box (:line-width 1 :color ,.grey-border)))))
     `(treesit-fold-fringe-face ((t (:foreground ,.purple)))))))

(defun tale-themes--set-variables (theme-name palette)
  "Set theme variables for THEME-NAME using PALETTE."
  (let-alist palette
    (custom-theme-set-variables
     theme-name
     `(linum-format " %3i ")
     `(hl-todo-keyword-faces
       ',(list (cons "TODO"  (list :foreground .yellow :weight 'bold))
               (cons "FIXME" (list :foreground .magenta :weight 'bold))
               (cons "DEBUG" (list :foreground .purple :weight 'bold))
               (cons "NOTE"  (list :foreground .blue :weight 'bold))
               (cons "DONE"  (list :foreground .green :weight 'bold)))))))

(defun tale-themes--hooks-function (palette)
  "Apply special face properties using PALETTE."
  (let-alist palette
    ;; special fringes
    (setq-local left-fringe-width 30
                right-fringe-width 30
                default-text-properties '(line-spacing 0 line-height 1)
                truncate-lines t)
    (face-remap-add-relative 'fringe :background .bg-special)

    ;; special background
    (setq-local buffer-face-mode-face
                `(:background ,.bg-special :foreground ,.fg-bright :height 0.9))
    (buffer-face-mode 1)))

(defgroup tale-themes-group nil
  "Customization for the Tale Themes."
  :group 'faces
  :prefix "tale-themes-")

(defcustom tale-themes-special-modes
  '(aidermacs-comint-mode-hook
    aidermacs-vterm-mode-hook
    vterm-mode-hook
    compilation-mode-hook
    git-commit-mode-hook
    magit-mode-hook
    magit-post-refresh-hook
    magit-post-stage-hook
    magit-post-unstage-hook
    magit-post-commit-hook
    messages-buffer-mode-hook
    message-mode-hook
    special-mode-hook
    transient-setup-buffer-hook)
  "Modes to setup special customization."
  :type '(repeat symbol)
  :group 'tale-themes-group)

(defun tale-themes--setup-hooks (theme-name palette)
  "Set up autoload hooks with PALETTE colors.
THEME-NAME is used to store cleanup functions in the appropriate variable."
  (when load-file-name
    (let ((hooks-function (lambda () (tale-themes--hooks-function palette))))
      (dolist (hook tale-themes-special-modes)
        (add-hook hook hooks-function)
        ;; Store cleanup function
        (when (boundp (intern (concat (symbol-name theme-name) "-theme--cleanup-functions")))
          (push (lambda () (remove-hook hook hooks-function))
                (symbol-value (intern (concat (symbol-name theme-name) "-theme--cleanup-functions"))))))

      (add-to-list 'custom-theme-load-path
                   (file-name-as-directory (file-name-directory load-file-name))))))

;; == UTILITY FUNCTIONS ==

(defun tale-themes-reload-current-theme ()
  "Reload the currently active Tale theme."
  (interactive)
  (when-let ((current-theme (car custom-enabled-themes)))
    (when (string-match-p "tale" (symbol-name current-theme))
      (disable-theme current-theme)
      (load-theme current-theme t)
      (message "Reloaded %s theme" current-theme))))

(defun tale-themes-switch-variant ()
  "Switch between Tale theme variants."
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (next-theme (if current-theme
                        (let ((current-name (symbol-name current-theme)))
                          (cond
                           ((string-match-p "dark-tale" current-name) 'bright-tale)
                           ((string-match-p "bright-tale" current-name) 'witch-tale)
                           ((string-match-p "witch-tale" current-name) 'vampire-tale)
                           ((string-match-p "vampire-tale" current-name) 'dark-tale)
                           (t 'dark-tale)))
                      'dark-tale)))
    (when current-theme
      (disable-theme current-theme))
    (load-theme next-theme t)
    (message "Switched to %s theme" next-theme)))

(defun tale-themes-toggle-dark-light ()
  "Toggle between dark-tale and bright-tale themes."
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (next-theme (if (and current-theme
                             (string-match-p "bright-tale" (symbol-name current-theme)))
                        'dark-tale
                      'bright-tale)))
    (when current-theme
      (disable-theme current-theme))
    (load-theme next-theme t)
    (message "Switched to %s theme" next-theme)))

(provide 'tale-themes-common)
;;; tale-themes-common.el ends here
