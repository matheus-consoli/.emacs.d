;;; tale-themes-common.el --- common theme generator for Tale themes -*- lexical-binding: t; -*-

;;; Commentary:

;; Tale themes common generator with improved color science,
;; accessibility, and comprehensive face coverage for modern Emacs.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'seq)

(defface tale-themes-special-buffer '((t (:height 0.9)))
  "Default face remapping for special buffers."
  :group 'tale-themes-group)

(defface tale-themes-special-fringe '((t nil))
  "Fringe face remapping for special buffers."
  :group 'tale-themes-group)

(defun tale-themes--create-theme (theme-name palette)
  "Create a theme with THEME-NAME using PALETTE."
  (put theme-name 'tale-theme t)
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
     `(window-divider ((t (:foreground ,.bg-main))))
     `(window-divider-last-pixel ((t (:foreground ,.bg-alt))))
     `(window-divider-first-pixel ((t (:foreground ,.bg-alt))))
     `(success ((t (:foreground ,.success-fg :weight bold))))
     `(warning ((t (:foreground ,.warning-fg :weight bold))))
     `(error ((t (:foreground ,.error-fg :weight bold))))
     `(shadow ((t (:foreground ,.grey-subtle))))

     ;; == UI ELEMENTS ==
     `(tooltip ((t (:background ,.bg-popup :foreground ,.fg-main :box (:line-width 1 :color ,.grey-border)))))
     `(button ((t (:foreground ,.purple :underline t))))
     `(header-line ((t (:background ,.bg-alt :foreground ,.grey-docstring :weight normal))))
     `(header-line-highlight ((t (:background ,.hover-bg :foreground ,.fg-main))))
     `(widget-field ((t (:background ,.bg-contrast :foreground ,.fg-main))))
     `(help-key-binding ((t (:background ,.bg-special  :foreground ,.hint-fg :box (:line-width (2 . 2) :color ,.bg-special)))))

     ;; Menu system faces
     `(menu ((t (:background ,.bg-popup :foreground ,.fg-popup))))
     `(tty-menu-enabled-face ((t (:background ,.bg-popup :foreground ,.fg-popup))))
     `(tty-menu-disabled-face ((t (:background ,.bg-popup :foreground ,.grey-subtle))))
     `(tty-menu-selected-face ((t (:background ,.bg-selection :foreground ,.fg-bright))))

     `(tale-themes-special-buffer ((t (:background ,.bg-special :foreground ,.fg-bright :height 0.9))))
     `(tale-themes-special-fringe ((t (:background ,.bg-special))))

     ;; Interface elements
     `(scroll-bar ((t (:background ,.bg-contrast :foreground ,.grey-border))))
     `(tool-bar ((t (:background ,.bg-alt :foreground ,.fg-main :box (:line-width 1 :style released-button)))))
     `(mouse ((t (:background ,.purple))))

     `(colorful-base ((t (:box nil))))

     ;; Selection-related faces
     `(lazy-highlight ((t (:background ,.visual-selection :foreground ,.fg-bright))))
     `(isearch ((t (:background ,.purple :foreground ,.bg-main :bold t))))
     `(isearch-fail ((t (:background ,.diff-removed-bg :foreground ,.error-fg))))
     `(query-replace ((t (:background ,.yellow :foreground ,.bg-main :weight bold))))
     `(match ((t (:background ,.diff-added-bg :foreground ,.success-fg :weight bold))))
     `(rectangle-preview ((t (:background ,.primary-selection))))
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
     `(trailing-whitespace ((t (:background ,.diff-removed-bg :foreground ,.fg-bright))))

     ;; == MODELINE ==
     `(mode-line ((t (:background ,.bg-main :foreground ,.grey-subtle :weight light
                                  :box (:line-width (1 . 6) :style flat-button) :inherit nil))))
     `(mode-line-active ((t (:background ,.bg-main :foreground ,.grey-docstring :weight normal
                                         :box (:line-width (1 . 6) :style flat-button) :inherit nil))))
     `(mode-line-inactive ((t (:background ,.bg-main :foreground ,.grey-subtle :weight light :overline ,.bg-special
                                           :box (:line-width (1 . 6) :style flat-button) :inherit nil))))
     `(mode-line-highlight ((t (:background ,.hover-bg :foreground ,.fg-main))))
     `(mode-line-buffer-id ((t (:foreground ,.purple :weight bold))))
     `(mode-line-emphasis ((t (:foreground ,.green :weight bold))))

     ;; == FONT LOCK & SYNTAX HIGHLIGHTING ==
     `(font-lock-builtin-face ((t (:foreground ,.magenta :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,.grey-subtle :slant italic))))
     `(font-lock-comment-face ((t (:foreground ,.grey-comment-blue :slant italic))))
     `(font-lock-constant-face ((t (:foreground ,.purple :weight bold))))
     `(font-lock-doc-face ((t (:foreground ,.grey-docstring :slant italic))))
     `(font-lock-function-name-face ((t (:foreground ,.purple :weight bold))))
     `(font-lock-keyword-face ((t (:foreground ,.magenta :weight bold :slant italic))))
     `(font-lock-negation-char-face ((t (:foreground ,.rose :weight bold))))
     `(font-lock-preprocessor-face ((t (:foreground ,.teal))))
     `(font-lock-regexp-grouping-backslash ((t (:foreground ,.yellow))))
     `(font-lock-regexp-grouping-construct ((t (:foreground ,.magenta))))
     `(font-lock-string-face ((t (:foreground ,.yellow))))
     `(font-lock-type-face ((t (:foreground ,.blue))))
     `(font-lock-variable-name-face ((t (:foreground ,.lavender))))
     `(font-lock-warning-face ((t (:foreground ,.warning-fg :weight bold))))

     ;; == TREE-SITTER SUPPORT ==
     `(font-lock-function-call-face ((t (:foreground ,.purple))))
     `(font-lock-variable-use-face ((t (:foreground ,.lavender))))
     `(font-lock-property-name-face ((t (:foreground ,.teal))))
     `(font-lock-property-use-face ((t (:foreground ,.blue))))
     `(font-lock-number-face ((t (:foreground ,.purple :weight bold))))
     `(font-lock-operator-face ((t (:foreground ,.magenta))))
     `(font-lock-bracket-face ((t (:foreground ,.grey-subtle))))
     `(font-lock-delimiter-face ((t (:foreground ,.grey-subtle))))
     `(font-lock-escape-face ((t (:foreground ,.yellow :weight bold))))

     ;; Whitespace
     `(whitespace-space ((t (:foreground ,.grey-border))))
     `(whitespace-tab ((t (:foreground ,.grey-border))))
     `(whitespace-newline ((t (:foreground ,.grey-border))))
     `(whitespace-trailing ((t (:background ,.diff-removed-bg))))
     `(whitespace-empty ((t (:background ,.diff-changed-bg))))

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

     ;; Org-mode
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
     `(org-quote ((t (:background ,.bg-contrast :foreground ,.fg-main :slant italic :extend t))))
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
     `(org-imminent-deadline ((t (:foreground ,.magenta :weight bold))))
     `(org-time-grid ((t (:foreground ,.grey-border))))
     `(org-warning ((t (:foreground ,.error-fg :weight bold))))

     `(org-superstar-leading ((t (:foreground ,.grey-subtle))))
     `(org-superstar-item ((t (:foreground ,.purple))))

     ;; Org-Roam
     `(org-roam-preview-heading-selection ((t (:background ,.focus-bg :foreground ,.lavender :weight bold))))
     `(org-roam-preview-heading-highlight ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(org-roam-title ((t (:foreground ,.lavender :weight bold :height 1.2))))

     ;; Org-Modern
     `(org-modern-tag ((t (:foreground ,.blue :background ,.bg-contrast :box (:line-width -1 :color ,.bg-contrast :style released-button) :height 0.9))))
     `(org-modern-priority ((t (:foreground ,.yellow :weight bold :box (:line-width 1 :color ,.yellow)))))
     `(org-modern-date-active ((t (:foreground ,.green :background ,.bg-contrast :box (:line-width 1 :color ,.green)))))
     `(org-modern-date-inactive ((t (:foreground ,.grey-subtle :background ,.bg-alt))))
     `(org-modern-time-active ((t (:foreground ,.blue :background ,.bg-contrast :weight bold))))
     `(org-modern-time-inactive ((t (:foreground ,.grey-subtle :background ,.bg-alt))))
     `(org-modern-done ((t (:foreground ,.green :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.green)))))
     `(org-modern-todo ((t (:foreground ,.magenta :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.magenta)))))

     ;; Magit and VC
     `(magit-section-heading ((t (:foreground ,.purple :weight semi-bold :height 1.2 :box (:line-width 4 :style flat-button)))))
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

     `(magit-filename ((t (:background ,.bg-main :foreground ,.fg-main :height 1.1 :box (:line-width 2 :style flat-button)))))

     `(magit-diff-file-heading ((t (:background ,.bg-main :foreground ,.fg-main :height 1.1 :box (:line-width 2 :style flat-button)))))
     `(magit-diff-file-heading-highlight ((t (:background ,.bg-selection :weight bold))))
     `(magit-diff-file-heading-selection ((t (:background ,.visual-selection :weight bold))))

     `(magit-diff-hunk-heading ((t (:background ,.bg-contrast :foreground ,.blue :weight bold :overline ,.grey-border))))
     `(magit-diff-hunk-heading-highlight ((t (:background ,.hover-bg :foreground ,.blue :weight bold :overline ,.grey-border))))
     `(magit-diff-hunk-heading-selection ((t (:background ,.bg-selection :foreground ,.blue :weight bold :overline ,.grey-border))))

     `(magit-diff-added-highlight ((t (:background ,.diff-added-bg :foreground ,.diff-added-fg :weight bold))))
     `(magit-diff-removed-highlight ((t (:background ,.diff-removed-bg :foreground ,.diff-removed-fg :weight bold))))
     `(magit-diff-context-highlight ((t (:background ,.bg-dim :foreground ,.fg-main))))
     `(magit-blame-highlight ((t (:background ,.bg-contrast :foreground ,.fg-bright))))
     `(magit-blame-heading ((t (:foreground ,.grey-subtle :weight bold :overline ,.grey-border))))
     `(magit-blame-name ((t (:foreground ,.purple :weight bold))))
     `(magit-blame-date ((t (:foreground ,.yellow))))
     `(magit-blame-summary ((t (:foreground ,.fg-bright))))
     `(magit-log-author ((t (:foreground ,.blue))))
     `(magit-log-date ((t (:foreground ,.grey-docstring))))
     `(magit-log-graph ((t (:foreground ,.grey-border))))
     `(magit-signature-good ((t (:foreground ,.green :weight bold))))
     `(magit-signature-bad ((t (:foreground ,.magenta :weight bold))))
     `(magit-signature-untrusted ((t (:foreground ,.yellow))))
     `(magit-signature-expired ((t (:foreground ,.grey-subtle))))
     `(magit-signature-revoked ((t (:foreground ,.magenta :strike-through t))))

     ;; Diffs and merge conflicts
     `(diff-added ((t (:background ,.diff-added-bg :foreground ,.diff-added-fg))))
     `(diff-removed ((t (:background ,.diff-removed-bg :foreground ,.diff-removed-fg))))
     `(diff-changed ((t (:background ,.diff-changed-bg :foreground ,.diff-changed-fg))))
     `(diff-refine-added ((t (:background ,.green :foreground ,.bg-main :weight bold))))
     `(diff-refine-removed ((t (:background ,.magenta :foreground ,.bg-main :weight bold))))
     `(diff-refine-changed ((t (:background ,.yellow :foreground ,.bg-main :weight bold))))

     `(smerge-upper ((t (:background ,.diff-removed-bg :extend t))))
     `(smerge-lower ((t (:background ,.diff-added-bg :extend t))))
     `(smerge-base ((t (:background ,.diff-changed-bg :extend t))))
     `(smerge-markers ((t (:background ,.bg-selection :foreground ,.fg-main :weight bold :extend t))))
     `(smerge-refined-added ((t (:inherit diff-refine-added))))
     `(smerge-refined-removed ((t (:inherit diff-refine-removed))))
     `(smerge-refined-changed ((t (:inherit diff-refine-changed))))
     `(diff-hl-insert ((t (:foreground ,.diff-added-fg :background ,.diff-added-bg))))
     `(diff-hl-delete ((t (:foreground ,.diff-removed-fg :background ,.diff-removed-bg))))
     `(diff-hl-change ((t (:foreground ,.diff-changed-fg :background ,.diff-changed-bg))))
     `(ediff-current-diff-A ((t (:background ,.diff-removed-bg :foreground ,.diff-removed-fg))))
     `(ediff-current-diff-B ((t (:background ,.diff-added-bg :foreground ,.diff-added-fg))))
     `(ediff-current-diff-C ((t (:background ,.diff-changed-bg :foreground ,.diff-changed-fg))))
     `(ediff-fine-diff-A ((t (:background ,.magenta :foreground ,.bg-main))))
     `(ediff-fine-diff-B ((t (:background ,.green :foreground ,.bg-main))))
     `(ediff-fine-diff-C ((t (:background ,.yellow :foreground ,.bg-main))))
     `(ediff-even-diff-A ((t (:inherit default))))
     `(ediff-even-diff-B ((t (:inherit default))))
     `(ediff-even-diff-C ((t (:inherit default))))
     `(ediff-even-diff-Ancestor ((t (:inherit default))))
     `(ediff-odd-diff-A ((t (:background ,.bg-code))))
     `(ediff-odd-diff-B ((t (:background ,.bg-code))))
     `(ediff-odd-diff-C ((t (:background ,.bg-code))))
     `(ediff-odd-diff-Ancestor ((t (:background ,.bg-code))))

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
     `(bookmark-menu-bookmark ((t (:foreground ,.magenta))))

     ;; Quick-peek faces
     `(quick-peek-background-face ((t (:background ,.bg-popup))))
     `(quick-peek-border-face ((t (:background ,.grey-border))))
     `(quick-peek-padding-face ((t (:background ,.bg-popup))))

     ;; == COMPILATION AND GREP FACES ==
     `(compilation-error ((t (:foreground ,.magenta :weight bold))))
     `(compilation-warning ((t (:foreground ,.yellow :weight bold))))
     `(compilation-info ((t (:foreground ,.green :weight bold))))
     `(compilation-line-number ((t (:foreground ,.grey-subtle))))
     `(compilation-column-number ((t (:foreground ,.grey-subtle))))
     `(compilation-mode-line-exit ((t (:foreground ,.green :weight bold))))
     `(compilation-mode-line-fail ((t (:foreground ,.magenta :weight bold))))
     `(compilation-mode-line-run ((t (:foreground ,.yellow :weight bold))))

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
     `(flycheck-inline-error ((t (:background ,.diff-removed-bg :foreground ,.error-fg :box (:line-width -1 :color ,.error-fg)))))
     `(flycheck-inline-warning ((t (:background ,.diff-changed-bg :foreground ,.warning-fg :box (:line-width -1 :color ,.warning-fg)))))
     `(flycheck-inline-info ((t (:background ,.bg-contrast :foreground ,.purple :box (:line-width -1 :color ,.purple)))))
     `(flycheck-color-mode-line-error-face ((t (:foreground ,.magenta :weight bold))))
     `(flycheck-color-mode-line-warning-face ((t (:foreground ,.yellow :weight bold))))
     `(flycheck-color-mode-line-info-face ((t (:foreground ,.purple :weight bold))))
     `(flycheck-color-mode-line-running-face ((t (:foreground ,.grey-subtle :weight bold))))
     `(flycheck-posframe-face ((t (:foreground ,.fg-bright))))
     `(flycheck-posframe-background-face ((t (:background ,.bg-popup))))
     `(flycheck-posframe-error-face ((t (:foreground ,.magenta :weight bold))))
     `(flycheck-posframe-warning-face ((t (:foreground ,.yellow :weight bold))))
     `(flycheck-posframe-info-face ((t (:foreground ,.purple :weight bold))))
     `(flycheck-error-list-group-header ((t (:background ,.bg-alt :foreground ,.fg-bright :weight bold))))

     ;; == FLYMAKE FACES ==
     `(flymake-error ((t (:underline (:style wave :color ,.magenta)))))
     `(flymake-warning ((t (:underline (:style wave :color ,.yellow)))))
     `(flymake-note ((t (:underline (:style wave :color ,.blue)))))
     `(flymake-error-echo ((t (:foreground ,.magenta :weight bold))))
     `(flymake-warning-echo ((t (:foreground ,.yellow :weight bold))))
     `(flymake-note-echo ((t (:foreground ,.blue :weight bold))))

     ;; Overlays
     `(eldoc-box-border ((t (:background ,.bg-popup))))
     `(eldoc-box-body ((t (:background ,.bg-popup :foreground ,.fg-main))))
     `(eldoc-highlight-function-argument ((t (:foreground ,.purple :weight bold))))
     `(mc/cursor-bar-face ((t (:background ,.purple :height 1))))
     `(semantic-highlight-func-current-tag-face ((t (:background ,.bg-selection))))
     `(highlight-changes ((t (:foreground ,.yellow :background ,.bg-contrast :underline t))))
     `(highlight-changes-delete ((t (:foreground ,.magenta :background ,.bg-contrast :strike-through t))))
     `(pulse-highlight-face ((t (:background ,.secondary-selection))))
     `(pulse-highlight-start-face ((t (:background ,.visual-selection))))
     `(highlight-symbol-face ((t (:background ,.focus-bg :foreground ,.lavender))))

     ;; Blamer
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
     `(centaur-tabs-modified-marker-selected ((t (:inherit centaur-tabs-selected :foreground ,.magenta :italic t))))
     `(centaur-tabs-modified-marker-unselected ((t (:inherit centaur-tabs-unselected :foreground ,.magenta :italic t))))
     `(centaur-tabs-name-mouse-face ((t (:foreground ,.fg-bright :bold t))))
     `(centaur-tabs-selected ((t (:background ,.bg-alt :foreground ,.fg-bright))))
     `(centaur-tabs-selected-modified ((t (:background ,.bg-alt :foreground ,.yellow))))
     `(centaur-tabs-unselected ((t (:background ,.bg-contrast :foreground ,.grey-subtle))))
     `(centaur-tabs-unselected-modified ((t (:background ,.bg-contrast :foreground ,.yellow))))
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

     ;; Dape
     `(dape-source-line-face ((t (:background ,.focus-bg :extend t))))
     `(dape-breakpoint-face ((t (:foreground ,.error-fg :weight bold))))
     `(dape-breakpoint-until-face ((t (:foreground ,.warning-fg :weight bold))))

     ;; Corfu, Vertico, and friends
     `(corfu-default ((t (:background ,.bg-popup :foreground ,.fg-popup :height 0.9))))
     `(corfu-current ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(corfu-bar ((t (:background ,.grey-border))))
     `(corfu-border ((t (:background ,.bg-special))))
     `(corfu-deprecated ((t (:foreground ,.grey-subtle :strike-through t))))
     `(corfu-annotations ((t (:foreground ,.grey-subtle :slant italic))))
     `(corfu-echo ((t (:foreground ,.grey-subtle :slant italic))))
     `(corfu-quick1 ((t (:foreground ,.purple :weight bold))))
     `(corfu-quick2 ((t (:foreground ,.magenta :weight bold))))

     ;; Vertico
     `(vertico-current ((t (:background ,.bg-alt))))
     `(vertico-group-title ((t (:foreground ,.purple :weight bold :height 0.9 :box (:line-width -1 :color ,.grey-subtle)))))
     `(vertico-group-separator ((t (:foreground ,.grey-subtle :strike-through t))))
     `(vertico-multiline ((t (:foreground ,.grey-subtle :slant italic))))
     `(vertico-quick1 ((t (:foreground ,.purple :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.purple)))))
     `(vertico-quick2 ((t (:foreground ,.magenta :background ,.bg-contrast :weight bold :box (:line-width 1 :color ,.magenta)))))
     `(vertico-posframe-border ((t (:background ,.bg-popup :foreground ,.grey-border))))
     `(vertico-posframe ((t (:background ,.bg-popup))))

     ;; Marginalia
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
     `(marginalia-value ((t (:foreground ,.lavender))))
     `(marginalia-version ((t (:foreground ,.green :weight bold))))

     ;; Nerd Icons Integration
     `(nerd-icons-completion-dir-face ((t (:foreground ,.purple))))

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
     `(treemacs-git-commit-diff-face ((t (:foreground ,.magenta))))

     ;; doom-modeline
     `(doom-modeline-bar ((t (:background ,.bg-alt :foreground ,.fg-bright))))
     `(doom-modeline-buffer-file ((t (:foreground ,.fg-bright))))
     `(doom-modeline-buffer-major-mode ((t (:foreground ,.purple :weight bold))))
     `(doom-modeline-buffer-minor-mode ((t (:foreground ,.grey-subtle))))
     `(doom-modeline-buffer-modified ((t (:foreground ,.magenta :weight bold))))
     `(doom-modeline-buffer-path ((t (:foreground ,.purple))))
     `(doom-modeline-debug ((t (:foreground ,.yellow))))
     `(doom-modeline-urgent ((t (:foreground ,.magenta))))
     `(doom-modeline-evil-emacs-state ((t (:foreground ,.magenta))))
     `(doom-modeline-evil-insert-state ((t (:foreground ,.green))))
     `(doom-modeline-evil-motion-state ((t (:foreground ,.grey-subtle))))
     `(doom-modeline-evil-normal-state ((t (:foreground ,.purple))))
     `(doom-modeline-evil-visual-state ((t (:foreground ,.yellow))))
     `(doom-modeline-bar-inactive ((t (:background ,.bg-contrast :foreground ,.fg-bright))))
     `(doom-modeline-info ((t (:foreground ,.green))))
     `(doom-modeline-lsp-error ((t (:foreground ,.magenta))))
     `(doom-modeline-lsp-success ((t (:foreground ,.green))))
     `(doom-modeline-lsp-warning ((t (:foreground ,.yellow))))
     `(doom-modeline-persp-name ((t (:foreground ,.purple))))
     `(doom-modeline-project-dir ((t (:foreground ,.yellow :weight bold))))
     `(doom-modeline-project-parent-dir ((t (:foreground ,.grey-subtle))))
     `(doom-modeline-project-root-dir ((t (:foreground ,.purple))))
     `(doom-modeline-repl-success ((t (:foreground ,.green))))
     `(doom-modeline-repl-warning ((t (:foreground ,.yellow))))
     `(doom-modeline-warning ((t (:foreground ,.yellow))))

     ;; mood line
     `(mood-line-buffer-name ((t (:foreground ,.fg-bright :weight light))))
     `(mood-line-buffer-status-modified ((t (:foreground ,.rose))))
     `(mood-line-buffer-status-read-only ((t (:foreground ,.yellow))))
     `(mood-line-buffer-status-narrowed ((t (:foreground ,.purple))))
     `(mood-line-major-mode ((t (:foreground ,.lavender :weight light))))
     `(mood-line-frame-status-client ((t (:foreground ,.blue :weight bold))))
     `(mood-line-status-neutral ((t (:foreground ,.grey-docstring))))
     `(mood-line-status-info ((t (:foreground ,.teal :weight semi-bold))))
     `(mood-line-status-success ((t (:foreground ,.green :weight bold))))
     `(mood-line-status-warning ((t (:foreground ,.yellow :weight bold))))
     `(mood-line-status-error ((t (:foreground ,.magenta :weight bold))))
     `(mood-line-encoding ((t (:foreground ,.grey-subtle :weight light))))
     `(mood-line-unimportant ((t (:foreground ,.grey-subtle :weight light))))

     ;; Line numbers
     `(line-number ((((class color) (min-colors 16))
                     (:foreground ,.grey-docstring :background ,.bg-main
                      :box (:line-width (-1 . 4) :color ,.bg-main)))
                    (t (:inherit default))))
     `(line-number-current-line ((((class color) (min-colors 16))
                                  (:foreground ,.purple :background ,.bg-main
                                   :box (:line-width (-1 . 4) :color ,.bg-main)))
                                 (t (:inherit default :weight bold :inverse-video t))))

     ;; Parentheses
     `(show-paren-match ((t (:foreground ,.green :weight bold))))
     `(show-paren-match-expression ((t (:background ,.bg-contrast))))
     `(show-paren-mismatch ((t (:background ,.diff-removed-bg :foreground ,.fg-popup :weight bold :strike-through t))))
     `(sp-pair-overlay-face ((t (:background ,.bg-contrast))))
     `(sp-show-pair-match-face ((t (:foreground ,.visual-selection :weight ultra-bold))))
     `(sp-show-pair-mismatch-face ((t (:background ,.magenta :foreground ,.bg-main :weight bold))))
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
     `(rainbow-delimiters-unmatched-face ((t (:background ,.diff-removed-bg :foreground ,.fg-bright :strike-through t))))
     `(rainbow-delimiters-mismatched-face ((t (:background ,.diff-removed-bg :foreground ,.fg-bright :strike-through t))))

     ;; Sideline packages
     `(sideline-flycheck-error ((t (:foreground ,.magenta))))
     `(sideline-flycheck-warning ((t (:foreground ,.yellow))))
     `(sideline-flycheck-info ((t (:foreground ,.purple))))

     ;; Avy
     `(avy-lead-face ((t (:background ,.purple :foreground ,.bg-main :weight bold))))
     `(avy-lead-face-0 ((t (:background ,.blue :foreground ,.bg-main :weight bold))))
     `(avy-lead-face-1 ((t (:background ,.magenta :foreground ,.bg-main :weight bold))))
     `(avy-lead-face-2 ((t (:background ,.yellow :foreground ,.bg-main :weight bold))))
     `(avy-background-face ((t (:foreground ,.grey-subtle))))

     ;; Embark
     `(embark-target ((t (:foreground ,.purple :weight bold))))
     `(embark-keybinding ((t (:foreground ,.yellow))))

     ;; Breadcrumb package faces
     `(breadcrumb-face ((t (:foreground ,.grey-subtle :height 0.9 :weight light
                                        :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-imenu-crumbs-face ((t (:foreground ,.lavender :height 0.9 :weight normal :slant italic
                                                     :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-imenu-leaf-face ((t (:foreground ,.purple :height 0.85 :weight semi-bold
                                                   :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-project-crumbs-face ((t (:foreground ,.grey-docstring :height 0.9 :weight light
                                                       :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-project-base-face ((t (:foreground ,.blue :height 0.9 :weight normal
                                                     :box (:line-width (2 . 8) :color ,.bg-alt)))))
     `(breadcrumb-project-leaf-face ((t (:foreground ,.fg-popup :height 0.85 :weight semi-bold
                                                     :box (:line-width (2 . 8) :color ,.bg-alt)))))

     ;; Misc Packages
     `(yas-field-highlight-face ((t (:background ,.bg-selection))))

     `(consult-preview-line ((t (:background ,.bg-contrast))))
     `(consult-highlight-match ((t (:foreground ,.purple :weight bold))))
     `(consult-file ((t (:foreground ,.fg-main))))
     `(consult-line-number ((t (:foreground ,.grey-subtle :inherit nil))))
     `(consult-line-number-prefix ((t (:box nil))))
     `(consult-async-split ((t (:foreground ,.purple))))
     `(consult-narrow-indicator ((t (:foreground ,.purple :weight bold))))
     `(consult-bookmark ((t (:foreground ,.magenta))))

     `(copilot-overlay-face ((t (:foreground ,.grey-subtle :slant italic))))

     `(hl-todo ((t (:foreground ,.yellow :weight bold))))

     `(deft-title-face ((t (:foreground ,.fg-main :weight bold))))
     `(deft-summary-face ((t (:foreground ,.grey-subtle :slant italic))))

     `(forge-topic-header-line ((t (:foreground ,.fg-main :weight bold))))
     `(forge-post-author ((t (:foreground ,.purple))))
     `(forge-topic-label ((t (:background ,.bg-selection :foreground ,.fg-main :box (:line-width -1 :color ,.bg-selection :style released-button) :height 0.9))))

     `(symbol-overlay-default-face ((t (:background ,.focus-bg))))

     ;; == VISUAL-REGEXP FACES ==
     `(vr/match-0 ((t (:background ,.purple :foreground ,.bg-main :weight bold))))
     `(vr/match-1 ((t (:background ,.magenta :foreground ,.bg-main :weight bold))))
     `(vr/group-0 ((t (:background ,.bg-selection :foreground ,.purple :weight bold :box (:line-width 1 :color ,.purple)))))
     `(vr/group-1 ((t (:background ,.bg-selection :foreground ,.magenta :weight bold :box (:line-width 1 :color ,.magenta)))))
     `(vr/group-2 ((t (:background ,.bg-selection :foreground ,.green :weight bold :box (:line-width 1 :color ,.green)))))

     ;; Rust, Jinx, undo-tree, and Ibuffer
     `(consoli-config-rust-macro-face ((t (:foreground ,.yellow :weight bold :underline (:style line :color ,.yellow)))))
     `(consoli-config-rust-unwrap-face ((t (:inherit error))))
     `(consoli-config-rust-debug-face ((t (:foreground ,.lavender :slant italic))))
     `(consoli-config-rust-propagation-face ((t (:foreground ,.teal :weight bold))))
     `(consoli-config-rust-unsafe-face ((t (:inherit warning :weight bold))))
     `(consoli-config-rust-attribute-face ((t (:inherit font-lock-preprocessor-face :foreground ,.lavender :slant italic))))
     `(jinx-misspelled ((t (:underline (:style wave :color ,.yellow)))))
     `(undo-tree-visualizer-current-face ((t (:foreground ,.green :weight bold))))
     `(undo-tree-visualizer-active-branch-face ((t (:foreground ,.purple))))
     `(undo-tree-visualizer-register-face ((t (:foreground ,.yellow))))
     `(ibuffer-marked ((t (:foreground ,.magenta :weight bold))))
     `(ibuffer-deletion ((t (:foreground ,.magenta :weight bold :strike-through t))))
     `(ibuffer-filter-group-name ((t (:foreground ,.purple :weight bold))))
     `(ibuffer-title ((t (:foreground ,.yellow :weight bold))))

     ;; == GNUS FACES ==
     `(gnus-group-mail-1 ((t (:foreground ,.purple :weight bold))))
     `(gnus-group-mail-2 ((t (:foreground ,.blue :weight bold))))
     `(gnus-group-mail-3 ((t (:foreground ,.teal :weight bold))))
     `(gnus-group-mail-low ((t (:foreground ,.grey-subtle))))
     `(gnus-group-news-1 ((t (:foreground ,.magenta :weight bold))))
     `(gnus-group-news-2 ((t (:foreground ,.rose :weight bold))))
     `(gnus-group-news-3 ((t (:foreground ,.yellow :weight bold))))
     `(gnus-group-news-low ((t (:foreground ,.grey-docstring))))
     `(gnus-header-content ((t (:foreground ,.fg-main :slant italic))))
     `(gnus-header-from ((t (:foreground ,.purple :weight bold))))
     `(gnus-header-subject ((t (:foreground ,.yellow :weight bold))))
     `(gnus-header-newsgroups ((t (:foreground ,.blue :slant italic))))
     `(gnus-summary-selected ((t (:background ,.bg-selection :foreground ,.fg-bright))))
     `(gnus-summary-normal-unread ((t (:foreground ,.fg-bright :weight bold))))
     `(gnus-summary-normal-read ((t (:foreground ,.grey-subtle))))
     `(gnus-summary-cancelled ((t (:foreground ,.magenta :strike-through t))))
     `(message-cited-text-1 ((t (:foreground ,.blue :slant italic))))
     `(message-header-name ((t (:foreground ,.purple :weight bold))))
     `(message-header-subject ((t (:foreground ,.yellow :weight bold))))
     `(message-header-to ((t (:foreground ,.fg-main :weight bold))))
     `(message-separator ((t (:foreground ,.grey-border))))

     ;; Dashboard faces
     `(dashboard-text-banner ((t (:foreground ,.purple))))
     `(dashboard-heading ((t (:foreground ,.yellow :bold t))))

     ;; Eglot diagnostic faces
     `(eglot-highlight-symbol-face ((t (:background ,.focus-bg))))
     `(eglot-mode-line ((t (:foreground ,.purple :weight bold))))
     `(eglot-diagnostic-tag-deprecated-face ((t (:strike-through t :foreground ,.grey-subtle))))
     `(eglot-diagnostic-tag-unnecessary-face ((t (:foreground ,.grey-subtle :slant italic))))
     ;; Eglot inlay hints
     `(eglot-inlay-hint-face ((t (:foreground ,.hint-fg :background unspecified :slant italic :height 0.9))))
     `(eglot-type-hint-face ((t (:foreground ,.hint-fg :background unspecified :slant italic :height 0.9))))
     `(eglot-parameter-hint-face ((t (:foreground ,.hint-fg :background unspecified :slant italic :height 0.9))))
     ;; Code actions in bundled Eglot and the newer refactor package.
     `(eglot-code-action-indicator-face ((t (:foreground ,.yellow :background ,.bg-main))))
     `(refactor-indicator-face ((t (:foreground ,.yellow :background ,.bg-main))))

     ;; == CALENDAR AND DATE FACES ==
     `(calendar-today ((t (:foreground ,.yellow :weight bold :underline t))))
     `(calendar-month-header ((t (:foreground ,.purple :weight bold))))
     `(calendar-weekday-header ((t (:foreground ,.blue :weight bold))))
     `(calendar-weekend-header ((t (:foreground ,.rose :weight bold))))
     `(holiday ((t (:foreground ,.green :weight bold))))
     `(diary ((t (:foreground ,.rose :slant italic))))
     `(diary-anniversary ((t (:foreground ,.magenta :weight bold))))
     `(diary-time ((t (:foreground ,.grey-docstring))))

     ;; Multimedia faces
     `(emms-playlist-selected-face ((t (:background ,.bg-selection :foreground ,.fg-bright :weight bold))))

     ;; Multiple Cursors Enhanced
     `(mc/cursor-face ((t (:background ,.purple :foreground ,.bg-main :weight bold))))
     `(mc/region-face ((t (:background ,.visual-selection))))

     ;; Symbol Overlay Enhanced
     `(symbol-overlay-face-1 ((t (:background ,.purple :foreground ,.bg-main))))
     `(symbol-overlay-face-2 ((t (:background ,.magenta :foreground ,.bg-main))))
     `(symbol-overlay-face-3 ((t (:background ,.green :foreground ,.bg-main))))
     `(symbol-overlay-face-4 ((t (:background ,.yellow :foreground ,.bg-main))))
     `(symbol-overlay-face-5 ((t (:background ,.blue :foreground ,.bg-main))))
     `(symbol-overlay-face-6 ((t (:background ,.teal :foreground ,.bg-main))))
     `(symbol-overlay-face-7 ((t (:background ,.rose :foreground ,.bg-main))))
     `(symbol-overlay-face-8 ((t (:background ,.lavender :foreground ,.bg-main))))

     ;; Smartparens Enhanced
     `(sp-wrap-overlay-opening-pair ((t (:background ,.purple :foreground ,.bg-main))))
     `(sp-wrap-overlay-closing-pair ((t (:background ,.purple :foreground ,.bg-main))))

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

(defgroup tale-themes-group nil
  "Customization for the Tale Themes."
  :group 'faces
  :prefix "tale-themes-")

(defvar tale-themes--installed-hooks nil
  "Alist of special buffer hooks and the functions Tale installed on them.")

(defvar-local tale-themes--buffer-hooks nil
  "Configured hooks that have run in this buffer's current major mode.")

(defvar-local tale-themes--special-buffer-settings nil
  "Saved local variable bindings from before Tale styling.")

(defvar-local tale-themes--special-remaps nil
  "Face remapping cookies owned by Tale.")

(defvar-local tale-themes--special-window-fringes nil
  "Original fringe widths of windows styled by Tale.")

(defun tale-themes--style-special-buffer ()
  "Apply Tale styling once to the current buffer."
  (unless tale-themes--special-remaps
    (dolist (variable '(left-fringe-width right-fringe-width
                        default-text-properties truncate-lines))
      (push (list variable (local-variable-p variable) (symbol-value variable))
            tale-themes--special-buffer-settings))
    (setq-local left-fringe-width 30
                right-fringe-width 30
                default-text-properties '(line-spacing 0 line-height 1)
                truncate-lines t)
    (setq tale-themes--special-remaps
          (list (face-remap-add-relative 'default 'tale-themes-special-buffer)
                (face-remap-add-relative 'fringe 'tale-themes-special-fringe)))
    (add-hook 'change-major-mode-hook #'tale-themes--unstyle-special-buffer nil t)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (push (cons window (window-fringes window))
            tale-themes--special-window-fringes)
      (set-window-fringes window 30 30))))

(defun tale-themes--unstyle-special-buffer ()
  "Remove Tale's remaps and restore the buffer's previous layout."
  (when tale-themes--special-remaps
    (remove-hook 'change-major-mode-hook #'tale-themes--unstyle-special-buffer t)
    (mapc #'face-remap-remove-relative tale-themes--special-remaps)
    (pcase-dolist (`(,variable ,local-p ,value) tale-themes--special-buffer-settings)
      (if local-p
          (set variable value)
        (kill-local-variable variable)))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (if-let* ((saved (assq window tale-themes--special-window-fringes)))
          (apply #'set-window-fringes window (cdr saved))
        (set-window-fringes window left-fringe-width right-fringe-width)))
    (setq tale-themes--special-remaps nil
          tale-themes--special-buffer-settings nil
          tale-themes--special-window-fringes nil)))

(defun tale-themes--update-special-buffers (hooks)
  "Reconcile installed hooks and styled buffers with configured HOOKS."
  (let ((wanted (and (seq-some (lambda (theme) (get theme 'tale-theme))
                               custom-enabled-themes)
                     hooks)))
    (dolist (entry tale-themes--installed-hooks)
      (unless (memq (car entry) wanted)
        (remove-hook (car entry) (cdr entry))
        (setq tale-themes--installed-hooks (delq entry tale-themes--installed-hooks))))
    (dolist (hook wanted)
      (unless (assq hook tale-themes--installed-hooks)
        (let ((handler (lambda ()
                         (cl-pushnew hook tale-themes--buffer-hooks)
                         (tale-themes--style-special-buffer))))
          (add-hook hook handler)
          (push (cons hook handler) tale-themes--installed-hooks))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (if (and wanted
                 (or (seq-intersection tale-themes--buffer-hooks wanted)
                     (seq-some (lambda (mode)
                                 (memq (intern-soft (concat (symbol-name mode) "-hook"))
                                       wanted))
                               (derived-mode-all-parents major-mode))))
            (tale-themes--style-special-buffer)
          (tale-themes--unstyle-special-buffer))))))

(defcustom tale-themes-special-modes
  '(aidermacs-comint-mode-hook
    aidermacs-vterm-mode-hook
    vterm-mode-hook
    compilation-mode-hook
    git-commit-mode-hook
    magit-mode-hook
    magit-refresh-buffer-hook
    magit-post-refresh-hook
    magit-post-stage-hook
    magit-post-unstage-hook
    magit-post-commit-hook
    messages-buffer-mode-hook
    message-mode-hook
    special-mode-hook
    transient-setup-buffer-hook)
  "Hooks that apply Tale styling to the current buffer."
  :set (lambda (option hooks)
         (set-default option hooks)
         (tale-themes--update-special-buffers hooks))
  :type '(repeat symbol)
  :group 'tale-themes-group)

(defun tale-themes--sync-special-buffers (theme)
  "Synchronize special buffer styling after a change to THEME."
  (when (get theme 'tale-theme)
    (tale-themes--update-special-buffers tale-themes-special-modes)))

(add-hook 'enable-theme-functions #'tale-themes--sync-special-buffers)
(add-hook 'disable-theme-functions #'tale-themes--sync-special-buffers)

;; == UTILITY FUNCTIONS ==

(defun tale-themes-reload-current-theme ()
  "Reload the currently active Tale theme."
  (interactive)
  (when-let* ((current-theme (car custom-enabled-themes)))
    (when (get current-theme 'tale-theme)
      (disable-theme current-theme)
      (load-theme current-theme t)
      (message "Reloaded %s theme" current-theme))))

(defconst tale-themes--variants
  '(dark-tale bright-tale witch-tale vampire-tale
    honed-tale cloud-tale sophisticated-dark-tale sophisticated-light-tale)
  "Tale themes in cycling order.")

(defun tale-themes-switch-variant ()
  "Cycle through the Tale themes, starting with dark-tale."
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (next-theme (or (cadr (memq current-theme tale-themes--variants))
                         (car tale-themes--variants))))
    (load-theme next-theme t)
    (when current-theme
      (disable-theme current-theme))
    (message "Switched to %s theme" next-theme)))

(defun tale-themes-toggle-dark-light ()
  "Toggle between dark-tale and bright-tale themes."
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (next-theme (if (eq current-theme 'bright-tale)
                         'dark-tale
                       'bright-tale)))
    (when current-theme
      (disable-theme current-theme))
    (load-theme next-theme t)
    (message "Switched to %s theme" next-theme)))

(provide 'tale-themes-common)
;;; tale-themes-common.el ends here
