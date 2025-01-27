;;; vercel-2024-theme.el --- A theme inspired by One Hunter Vercel 2024 -*- lexical-binding: t; -*-
(deftheme vercel-2024 "A nice dark theme.")
(let* (
       ;; Base colors
       (color0 "#000000")
       (color1 "#EDEDED")
       (color2 "#141414")
       (color4 "#171717")
       (color3 "#2d2d2d")
       (color5 "#ffffff")
       (color6 "#282828")
       (color7 "#4b4b4b")
       (color8 "#C372FC")
       (color9 "#A3A3A3")
       (color10 "#FF4C8D")
       (color11 "#00CA51")
       (color12 "#E5B800")
       (color13 "#2B2B2B")
       (color14 "#0f0f0f")
       (color15 "#fcfcfc")
       (color16 "#0b0b0b")
       (color17 "#f8f8f8")

       ;; Additional semantic colors
       (focus-bg "#363636")
       (hover-bg "#404040")
       (success-muted "#009A3D")
       (warning-muted "#B89F00")
       (error-muted "#C44B71")
       (info-muted "#5A91C8")

       ;; New selection-specific colors
       (primary-selection "#141414")
       (secondary-selection "#2D4B4B")
       (visual-selection "#4B3B66")
       (inactive-selection "#2B2B2B")

       ;; LSP-specific colors
       (hint-fg "#878787")
       (hint-type-fg "#4b4b4b") ;; #7E72FC
       (completion-bg "#1A1A1A")
       )

  (custom-theme-set-faces
   'vercel-2024
   ;; BASIC FACES
   `(cursor ((t (:background ,color5))))
   `(default ((t (:background ,color0 :foreground ,color1))))
   `(fringe ((t (:background ,color0))))
   ;; `(highlight ((t (:background ,focus-bg))))
   `(highlight ((t (:background ,hover-bg))))
   `(hl-line ((t (:background ,color2))))
   `(link ((t (:foreground ,color8 :underline t))))
   `(minibuffer-prompt ((t (:background ,color0 :foreground ,color8 :bold t))))
   ;; `(region ((t (:background ,color3))))
   `(region ((t (:background ,primary-selection :extend t))))
   ;; `(secondary-selection ((t (:background ,color4))))
   `(secondary-selection ((t (:background ,secondary-selection :extend t))))
   `(vertical-border ((t (:foreground ,color7))))
   `(success ((t (:foreground ,success-muted :weight bold))))
   `(warning ((t (:foreground ,warning-muted :weight bold))))
   `(error ((t (:foreground ,error-muted :weight bold))))

   ;; Selection-related faces
   `(lazy-highlight ((t (:background ,visual-selection :foreground ,color5))))  ; Search matches
   `(isearch ((t (:background ,color8 :foreground ,color0 :bold t))))  ; Active search term
   `(isearch-fail ((t (:background ,error-muted :foreground ,color10))))  ; Failed search
   `(match ((t (:background ,success-muted :foreground ,color11))))  ; Matched text
   ;; Search and Match Overlays
   `(isearch-overlay ((t (:background ,color8 :foreground ,color0 :weight bold))))
   `(lazy-highlight-overlay ((t (:background ,color4 :box (:line-width -1 :color ,color8)))))
   `(match-overlay ((t (:background ,color3 :foreground ,color11))))

   ;; Rectangle selection
   `(rectangle-preview ((t (:background ,primary-selection))))
   `(rectangle-preview-face ((t (:background ,primary-selection))))

   ;; Selection in inactive windows
   `(region-inactive ((t (:background ,inactive-selection))))

   ;; modeline
   `(mode-line ((t (:background ,color6 :foreground ,color5 :box (:line-width 1 :color ,color7)))))
   ;; `(mode-line ((t (:background ,color6 :foreground ,color5))))
   `(mode-line-buffer-id ((t (:foreground ,color8 :bold t))))
   `(mode-line-emphasis ((t (:foreground ,color11))))
   ;; `(mode-line-inactive ((t (:background ,color2 :foreground ,color5))))
   `(mode-line-inactive ((t (:background ,color2 :foreground ,color9 :box (:line-width 1 :color ,color7)))))

   ;; Specific minibuffer-related faces
   `(completions-common-part ((t (:foreground ,color8))))
   `(completions-first-difference ((t (:foreground ,color10))))

   ;; FONT LOCK FACES
   `(font-lock-builtin-face ((t (:foreground ,color10))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,color9 :slant italic))))
   `(font-lock-comment-face ((t (:foreground ,color9 :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,color8 :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,color9 :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,color8))))
   `(font-lock-keyword-face ((t (:foreground ,color10 :slant italic))))
   `(font-lock-preprocessor-face ((t (:foreground ,color12 :weight bold))))
   `(font-lock-string-face ((t (:foreground ,color12))))
   `(font-lock-type-face ((t (:foreground ,color12))))
   `(font-lock-variable-name-face ((t (:foreground ,color1 :slant italic))))
   `(font-lock-warning-face ((t (:foreground ,warning-muted :bold t))))

   `(whitespace-space ((t (:foreground ,color7))))
   `(whitespace-tab ((t (:foreground ,color7))))
   `(whitespace-newline ((t (:foreground ,color7))))
   `(whitespace-trailing ((t (:background ,error-muted))))
   `(whitespace-empty ((t (:background ,warning-muted))))

   ;; tab-bar-mode
   `(tab-bar ((t (:background ,color0 :foreground ,color1))))
   `(tab-bar-tab ((t (:background ,color6 :foreground ,color5))))
   `(tab-bar-tab-inactive ((t (:background ,color2 :foreground ,color9))))
   `(tab-bar-tab-ungrouped ((t (:background ,color2 :foreground ,color9))))
   `(tab-bar-tab-group-current ((t (:background ,color6 :foreground ,color8))))
   `(tab-bar-tab-group-inactive ((t (:background ,color2 :foreground ,color9))))

   ;; THIRD PARTY PACKAGE FACES
   ;; web-mode
   `(web-mode-string-face ((t (:foreground ,color11))))
   `(web-mode-html-tag-face ((t (:foreground ,color10))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,color10))))
   `(web-mode-html-attr-name-face ((t (:foreground ,color12))))
   `(web-mode-css-property-name-face ((t (:foreground ,color8))))
   `(web-mode-css-selector-face ((t (:foreground ,color10))))
   `(web-mode-json-key-face ((t (:foreground ,color8))))
   `(web-mode-json-string-face ((t (:foreground ,color11))))

   ;; company-mode
   `(company-tooltip ((t (:background ,color14 :foreground ,color15))))
   `(company-tooltip-common ((t (:foreground ,color8))))
   `(company-tooltip-selection ((t (:background ,color3))))
   `(company-scrollbar-fg ((t (:background ,color7))))
   `(company-scrollbar-bg ((t (:background ,color14))))

   ;; org-mode
   `(org-block ((t (:background ,color16 :foreground ,color17))))
   `(org-block-begin-line ((t (:foreground ,color9))))
   `(org-level-1 ((t (:foreground ,color8))))
   `(org-level-2 ((t (:foreground ,color10))))
   `(org-level-3 ((t (:foreground ,color12))))
   `(org-headline-done ((t (:foreground ,color11))))
   `(org-todo ((t (:foreground ,color10))))
   `(org-done ((t (:foreground ,color11))))

   ;; magit
   `(magit-section-heading ((t (:foreground ,color8))))
   `(magit-branch-current ((t (:foreground ,color12))))
   `(magit-diff-added ((t (:foreground ,color11))))
   `(magit-diff-removed ((t (:foreground ,color10))))
   `(magit-diff-context ((t (:foreground ,color9))))
   `(magit-blame-highlight ((t (:background ,color2 :foreground ,color5))))
   `(magit-blame-heading ((t (:background ,color4 :foreground ,color9))))
   `(magit-blame-name ((t (:foreground ,color8))))
   `(magit-blame-date ((t (:foreground ,color12))))
   `(magit-blame-summary ((t (:foreground ,color5))))

   ;; git
   `(diff-added ((t (:background ,success-muted :foreground ,color11))))
   `(diff-removed ((t (:background ,error-muted :foreground ,color10))))
   `(diff-changed ((t (:background ,warning-muted :foreground ,color12))))
   `(git-gutter:added ((t (:foreground ,color11))))
   `(git-gutter:deleted ((t (:foreground ,color10))))
   `(git-gutter:modified ((t (:foreground ,color12))))
   `(git-gutter-fr:added ((t (:foreground ,color11))))
   `(git-gutter-fr:deleted ((t (:foreground ,color10))))
   `(git-gutter-fr:modified ((t (:foreground ,color12))))

   ;; markdown-mode
   `(markdown-header-face ((t (:foreground ,color8))))
   `(markdown-code-face ((t (:background ,color16))))
   `(markdown-inline-code-face ((t (:foreground ,color12))))
   `(markdown-link-face ((t (:foreground ,color8))))

   ;; dired
   `(dired-directory ((t (:foreground ,color8))))
   `(dired-marked ((t (:foreground ,color12))))
   `(dired-flagged ((t (:foreground ,color10))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,color8))))
   `(eshell-ls-directory ((t (:foreground ,color8))))
   `(eshell-ls-executable ((t (:foreground ,color11))))
   `(eshell-ls-symlink ((t (:foreground ,color12))))

   ;; Terminal colors
   `(term-color-black ((t (:foreground ,color0 :background ,color0))))
   `(term-color-red ((t (:foreground ,color10 :background ,color10))))
   `(term-color-green ((t (:foreground ,color11 :background ,color11))))
   `(term-color-yellow ((t (:foreground ,color12 :background ,color12))))
   `(term-color-blue ((t (:foreground ,color8 :background ,color8))))
   `(term-color-magenta ((t (:foreground ,color8 :background ,color8))))
   `(term-color-cyan ((t (:foreground ,color11 :background ,color11))))
   `(term-color-white ((t (:foreground ,color5 :background ,color5))))

   ;; Which-key
   `(which-key-key-face ((t (:foreground ,color8))))
   `(which-key-separator-face ((t (:foreground ,color9))))
   `(which-key-note-face ((t (:foreground ,color9))))
   `(which-key-command-description-face ((t (:foreground ,color5))))
   `(which-key-group-description-face ((t (:foreground ,color12))))
   `(which-key-special-key-face ((t (:foreground ,color10))))

   ;; ivy/helm
   `(ivy-current-match ((t (:background ,color3))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,color8))))
   `(helm-selection ((t (:background ,color3))))
   `(helm-match ((t (:foreground ,color8))))

   ;; Bookmarks
   `(bookmark-face ((t (:foreground ,color8 :background ,color2))))
   `(bookmark-menu-heading ((t (:foreground ,color8 :weight bold))))
   `(bookmark-menu-bookmark ((t (:foreground ,color10))))

   ;;;
   ;;;
   ;;; ERRORS
   ;;;
   ;;;
   ;; Quick-peek faces
   `(quick-peek-background-face ((t (:background ,color14))))
   `(quick-peek-border-face ((t (:background ,color7))))
   `(quick-peek-padding-face ((t (:background ,color14))))
   `(quick-peek-header-face ((t (:background ,color6 :foreground ,color5 :height 1.1))))

   ;; Flycheck comprehensive faces
   `(flycheck-error ((t (:underline (:style wave :color ,color10)))))
   `(flycheck-info ((t (:underline (:style wave :color ,color8)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,color12)))))
   `(flycheck-fringe-error ((t (:foreground ,color10))))
   `(flycheck-fringe-warning ((t (:foreground ,color12))))
   `(flycheck-fringe-info ((t (:foreground ,color8))))
   `(flycheck-error-list-error ((t (:foreground ,color10))))
   `(flycheck-error-list-warning ((t (:foreground ,color12))))
   `(flycheck-error-list-info ((t (:foreground ,color8))))
   `(flycheck-error-list-line-number ((t (:foreground ,color9))))
   `(flycheck-error-list-column-number ((t (:foreground ,color9))))
   `(flycheck-error-list-filename ((t (:foreground ,color8))))
   `(flycheck-error-list-highlight ((t (:background ,color3))))
   `(flycheck-verify-select-checker ((t (:background ,color3))))
   `(flycheck-error-list-checker-name ((t (:foreground ,color12))))
   `(flycheck-error-list-id ((t (:foreground ,color8))))
   `(flycheck-error-list-id-with-explainer ((t (:foreground ,color8 :box (:line-width -1 :color ,color8)))))

   ;; Flycheck inline faces
   `(flycheck-inline-error ((t (:background ,error-muted :foreground ,color10 :box (:line-width -1 :color ,color10)))))
   `(flycheck-inline-warning ((t (:background ,warning-muted :foreground ,color12 :box (:line-width -1 :color ,color12)))))
   `(flycheck-inline-info ((t (:background ,color2 :foreground ,color8 :box (:line-width -1 :color ,color8)))))

   ;; Flycheck indicators
   `(flycheck-color-mode-line-error-face ((t (:foreground ,color10 :weight bold))))
   `(flycheck-color-mode-line-warning-face ((t (:foreground ,color12 :weight bold))))
   `(flycheck-color-mode-line-info-face ((t (:foreground ,color8 :weight bold))))
   `(flycheck-color-mode-line-running-face ((t (:foreground ,color9 :weight bold))))
   `(flycheck-color-mode-line-success-face ((t (:foreground ,color11 :weight bold))))

   ;; Flycheck posframe faces
   `(flycheck-posframe-face ((t (:foreground ,color5))))
   `(flycheck-posframe-background-face ((t (:background ,color14))))
   `(flycheck-posframe-error-face ((t (:foreground ,color10 :weight bold))))
   `(flycheck-posframe-warning-face ((t (:foreground ,color12 :weight bold))))
   `(flycheck-posframe-info-face ((t (:foreground ,color8 :weight bold))))
   `(flycheck-posframe-border-face ((t (:foreground ,color7))))

   ;; Flycheck error list header
   `(flycheck-error-list-header ((t (:background ,color6 :foreground ,color5 :weight bold))))
   `(flycheck-error-list-separator ((t (:foreground ,color7))))
   `(flycheck-error-list-checkername-face ((t (:foreground ,color8))))

   ;; Flycheck error list highlighting
   `(flycheck-error-list-error-message ((t (:foreground ,color10))))
   `(flycheck-error-list-warning-message ((t (:foreground ,color12))))
   `(flycheck-error-list-info-message ((t (:foreground ,color8))))

   ;; Flycheck inline message styles
   `(flycheck-inline-message ((t (:inherit default :slant italic))))
   `(flycheck-inline-suggestion ((t (:foreground ,color11 :slant italic))))
   `(flycheck-inline-explanation ((t (:foreground ,color9 :slant italic))))

   ;; Flycheck checker status indicators
   `(flycheck-checker-disabled-face ((t (:foreground ,color9 :strike-through t))))
   `(flycheck-checker-running-face ((t (:foreground ,color12 :slant italic))))
   `(flycheck-checker-success-face ((t (:foreground ,color11 :weight bold))))
   `(flycheck-checker-error-face ((t (:foreground ,color10 :weight bold))))
   `(flycheck-checker-warning-face ((t (:foreground ,color12 :weight bold))))
   `(flycheck-checker-info-face ((t (:foreground ,color8 :weight bold))))

   ;; Error and Warning Overlays
   `(flycheck-error-overlay ((t (:underline (:style wave :color ,color10)
                                            :background ,color2))))
   `(flycheck-warning-overlay ((t (:underline (:style wave :color ,color12)
                                              :background ,color2))))
   `(flycheck-info-overlay ((t (:underline (:style wave :color ,color8)
                                           :background ,color2))))

   ;; https://github.com/konrad1977/flycheck-overlay
   `(flycheck-overlay-error ((t :background ,error-muted
                                :foreground ,color13
                                :height 0.9
                                :weight normal)))
   `(flycheck-overlay-warning ((t :background ,warning-muted
                                  :foreground ,color13
                                  :height 0.9
                                  :weight normal)))
   `(flycheck-overlay-info ((t :background ,info-muted
                               :foreground ,color13
                               :height 0.9
                               :weight normal)))

   ;;
   ;; / ERRORS
   ;;
   ;;
   ;;

   ;; basic overlay faces
   `(overlay ((t (:background ,color2))))

   ;; Documentation Overlays
   `(pos-tip-overlay ((t (:background ,color14 :foreground ,color1))))
   `(eldoc-highlight-function-argument-overlay ((t (:foreground ,color8 :weight bold))))

   ;; Multiple Cursors Overlays
   `(mc/cursor-overlay ((t (:background ,color8 :foreground ,color0))))
   `(mc/cursor-bar-overlay ((t (:background ,color8 :height 1))))

   ;; custom overlay
   `(rectangle-overlay ((t (:background ,color3 :foreground ,color5))))

   ;; Semantic Overlays
   `(semantic-highlight-func-overlay ((t (:background ,color3))))
   `(semantic-tag-overlay ((t (:background ,color2 :box t))))

   ;; General Purpose Overlays
   `(highlight-changes ((t (:foreground ,color12 :background ,color2 :underline t))))
   `(highlight-changes-delete ((t (:foreground ,color10 :background ,color2 :strike-through t))))

   ;; Transient Mark and Selection Overlays
   `(transient-mark-overlay ((t (:background ,color3))))
   `(secondary-selection-overlay ((t (:background ,color4 :foreground ,color5))))

   ;; Navigation Overlays
   `(avy-overlay-lead-face ((t (:background ,color8 :foreground ,color0 :weight bold))))
   `(avy-overlay-background-face ((t (:background ,color16))))

   `(pulse-highlight-face ((t (:background ,secondary-selection))))
   `(pulse-highlight-start-face ((t (:background ,visual-selection))))

   ;; blamer-mode
   `(blamer-face ((t (:foreground ,color9 :italic t :height 0.9 :weight light))))

   ;; centaur-tabs
   `(centaur-tabs-active-bar-face ((t (:background ,color8))))
   `(centaur-tabs-close-mouse-face ((t (:foreground ,color10))))
   `(centaur-tabs-close-selected ((t (:inherit 'centaur-tabs-selected :foreground ,color5))))
   `(centaur-tabs-close-unselected ((t (:inherit 'centaur-tab-sunselected :foreground ,color9))))
   `(centaur-tabs-close-mouse-face ((t (:foreground ,color10))))
   `(centaur-tabs-default ((t (:background ,color0 :foreground ,color1))))
   `(centaur-tabs-group-face ((t (:foreground ,color8 :bold t))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected :foreground ,color10))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected :foreground ,color10))))
   `(centaur-tabs-name-mouse-face ((t (:foreground ,color5 :bold t))))
   `(centaur-tabs-selected ((t (:background ,color6 :foreground ,color5))))
   `(centaur-tabs-selected-modified ((t (:background ,color6 :foreground ,color12))))
   `(centaur-tabs-unselected ((t (:background ,color2 :foreground ,color9))))
   `(centaur-tabs-unselected-modified ((t (:background ,color2 :foreground ,color12))))
   `(centaur-tabs-bar ((t (:background ,color0))))

   ;; LSP mode
   `(lsp-face-highlight-read ((t (:background ,focus-bg))))
   `(lsp-face-highlight-textual ((t (:background ,focus-bg))))
   `(lsp-face-highlight-write ((t (:background ,focus-bg))))
   `(lsp-headerline-breadcrumb-path-error-face ((t (:foreground ,color10))))
   `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,color9))))
   `(lsp-headerline-breadcrumb-path-hint-face ((t (:foreground ,color11))))
   `(lsp-headerline-breadcrumb-path-info-face ((t (:foreground ,color8))))
   `(lsp-headerline-breadcrumb-path-warning-face ((t (:foreground ,color12))))
   `(lsp-headerline-breadcrumb-project-prefix-face ((t (:foreground ,color8))))
   `(lsp-headerline-breadcrumb-symbols-face ((t (:foreground ,color12))))
   `(lsp-modeline-code-actions-face ((t (:foreground ,color12))))
   `(lsp-ui-doc-background ((t (:background ,color14))))
   `(lsp-ui-doc-border ((t (:background ,color7))))
   `(lsp-ui-doc-header ((t (:background ,color6 :foreground ,color5))))
   `(lsp-ui-peek-filename ((t (:foreground ,color8))))
   `(lsp-ui-peek-footer ((t (:background ,color6))))
   `(lsp-ui-peek-header ((t (:background ,color6 :foreground ,color5))))
   `(lsp-ui-peek-highlight ((t (:background ,color3 :foreground ,color5))))
   `(lsp-ui-peek-line-number ((t (:foreground ,color9))))
   `(lsp-ui-peek-list ((t (:background ,color14))))
   `(lsp-ui-peek-peek ((t (:background ,color14))))
   `(lsp-ui-peek-selection ((t (:background ,color3 :foreground ,color5))))
   `(lsp-ui-sideline-code-action ((t (:foreground ,color12))))
   `(lsp-ui-sideline-current-symbol ((t (:foreground ,color8 :weight bold))))
   `(lsp-ui-sideline-symbol ((t (:foreground ,color8))))
   ;; `(lsp-ui-sideline-symbol ((t (:foreground ,color9))))
   `(lsp-ui-sideline-symbol-info ((t (:foreground ,color9 :slant italic))))

   ;; General inlay hint face (base style for all hints)
   `(lsp-inlay-hint-face ((t (:foreground ,hint-fg :slant italic :height 0.9))))
   ;; Type hints (e.g., variable types)
   `(lsp-inlay-hint-type-face ((t (:foreground ,hint-type-fg :slant italic :height 0.9))))
   ;; Inline completion suggestions
   `(lsp-inline-completion-overlay-face ((t (:foreground ,color9 :background ,completion-bg :slant italic))))
   ;; Function argument highlighting in signature help
   `(lsp-signature-highlight-function-argument ((t (:foreground ,color8 :weight bold :underline (:style line :color ,color8)))))


   ;; DAP (Debug Adapter Protocol) faces
   `(dap-ui-breakpoint-verified-fringe ((t (:foreground ,color11))))
   `(dap-ui-pending-breakpoint-face ((t (:foreground ,color12))))
   `(dap-ui-marker-face ((t (:background ,color3))))
   `(dap-ui-compile-errline ((t (:foreground ,color10))))
   `(dap-ui-compile-warnline ((t (:foreground ,color12))))
   ;; Corfu completion
   `(corfu-default ((t (:background ,color14 :foreground ,color15))))
   `(corfu-current ((t (:background ,color3 :foreground ,color5))))
   `(corfu-bar ((t (:background ,color7))))
   `(corfu-border ((t (:background ,color7))))
   `(corfu-deprecated ((t (:foreground ,color9 :strike-through t))))
   `(corfu-annotation ((t (:foreground ,color9))))
   `(corfu-echo ((t (:foreground ,color9))))

   ;; LSP completion via Corfu
   `(corfu-metadata-face ((t (:foreground ,color9))))
   `(corfu-documentation-face ((t (:foreground ,color9))))
   `(corfu-quick-face ((t (:foreground ,color12))))
   `(corfu-quick1-face ((t (:foreground ,color8))))
   `(corfu-quick2-face ((t (:foreground ,color10))))

   ;; Vertico-specific faces
   `(vertico-current ((t (:background ,color3 :foreground ,color5))))
   `(vertico-group-title ((t (:foreground ,color8 :weight bold))))
   `(vertico-group-separator ((t (:foreground ,color7 :strike-through t))))
   `(vertico-multiline ((t (:foreground ,color9))))

   ;; Vertico quick keys (1-9 shortcuts)
   `(vertico-quick1 ((t (:foreground ,color8 :weight bold))))
   `(vertico-quick2 ((t (:foreground ,color10 :weight bold))))

   ;; Vertico documentation/annotation
   `(vertico-posframe ((t (:background ,color14))))
   `(vertico-posframe-border ((t (:background ,color7))))

   ;; Marginalia annotations for Vertico
   `(marginalia-key ((t (:foreground ,color8))))
   `(marginalia-type ((t (:foreground ,color10))))
   `(marginalia-modified ((t (:foreground ,color12))))
   `(marginalia-file-name ((t (:foreground ,color9))))
   `(marginalia-file-owner ((t (:foreground ,color9))))
   `(marginalia-file-priv-no ((t (:foreground ,color9))))
   `(marginalia-file-priv-dir ((t (:foreground ,color8))))
   `(marginalia-file-priv-exec ((t (:foreground ,color11))))
   `(marginalia-file-priv-link ((t (:foreground ,color12))))
   `(marginalia-file-priv-read ((t (:foreground ,color10))))
   `(marginalia-file-priv-write ((t (:foreground ,color10))))
   `(marginalia-size ((t (:foreground ,color9))))
   `(marginalia-number ((t (:foreground ,color9))))
   `(marginalia-char ((t (:foreground ,color8))))
   `(marginalia-date ((t (:foreground ,color9))))
   `(marginalia-documentation ((t (:foreground ,color9 :slant italic))))
   `(marginalia-function ((t (:foreground ,color8))))
   `(marginalia-mode ((t (:foreground ,color10))))
   `(marginalia-variable ((t (:foreground ,color8))))
   `(marginalia-version ((t (:foreground ,color11))))

   ;; Orderless for Vertico
   `(orderless-match-face-0 ((t (:foreground ,color8 :weight bold))))
   `(orderless-match-face-1 ((t (:foreground ,color10 :weight bold))))
   `(orderless-match-face-2 ((t (:foreground ,color11 :weight bold))))
   `(orderless-match-face-3 ((t (:foreground ,color12 :weight bold))))

   ;; Treemacs faces
   `(treemacs-root-face ((t (:foreground ,color8 :weight bold :height 1.2))))
   `(treemacs-root-unreadable-face ((t (:foreground ,color9 :strike-through t))))
   `(treemacs-directory-face ((t (:foreground ,color5))))
   `(treemacs-directory-collapsed-face ((t (:foreground ,color5))))
   `(treemacs-file-face ((t (:foreground ,color1))))
   `(treemacs-git-modified-face ((t (:foreground ,color10))))
   `(treemacs-git-added-face ((t (:foreground ,color11))))
   `(treemacs-git-conflict-face ((t (:foreground ,color10))))
   `(treemacs-git-untracked-face ((t (:foreground ,color12))))
   `(treemacs-git-renamed-face ((t (:foreground ,color8))))
   `(treemacs-git-ignored-face ((t (:foreground ,color9))))
   `(treemacs-term-node-face ((t (:foreground ,color8))))
   `(treemacs-tags-face ((t (:foreground ,color12))))
   `(treemacs-help-title-face ((t (:foreground ,color8 :weight bold))))
   `(treemacs-help-column-face ((t (:foreground ,color9))))
   `(treemacs-on-success-pulse-face ((t (:background ,color11 :foreground ,color0))))
   `(treemacs-on-failure-pulse-face ((t (:background ,color10 :foreground ,color0))))
   `(treemacs-fringe-indicator-face ((t (:foreground ,color8))))
   `(treemacs-header-button-face ((t (:foreground ,color9))))
   `(treemacs-marked-file-face ((t (:foreground ,color12 :weight bold))))
   `(treemacs-file-extension-face ((t (:foreground ,color1))))

   ;; Treemacs modeline
   `(treemacs-modeline-selected-face ((t (:foreground ,color8 :weight bold))))
   `(treemacs-modeline-active-face ((t (:foreground ,color5))))

   ;; Treemacs git status faces
   `(treemacs-git-commit-diff-face ((t (:foreground ,color10))))
   `(treemacs-git-push-face ((t (:foreground ,color11))))
   `(treemacs-git-pull-face ((t (:foreground ,color12))))

   ;; doom-modeline
   `(doom-modeline-bar ((t (:background ,color6 :foreground ,color5))))
   `(doom-modeline-buffer-file ((t (:foreground ,color5))))
   `(doom-modeline-buffer-major-mode ((t (:foreground ,color8 :weight bold))))
   `(doom-modeline-buffer-minor-mode ((t (:foreground ,color9))))
   `(doom-modeline-buffer-modified ((t (:foreground ,color10 :weight bold))))
   `(doom-modeline-buffer-path ((t (:foreground ,color8))))
   `(doom-modeline-buffer-timemachine ((t (:foreground ,color12))))
   `(doom-modeline-debug ((t (:foreground ,color12))))
   `(doom-modeline-error ((t (:foreground ,color10))))
   `(doom-modeline-evil-emacs-state ((t (:foreground ,color10))))
   `(doom-modeline-evil-insert-state ((t (:foreground ,color11))))
   `(doom-modeline-evil-motion-state ((t (:foreground ,color9))))
   `(doom-modeline-evil-normal-state ((t (:foreground ,color8))))
   `(doom-modeline-evil-visual-state ((t (:foreground ,color12))))
   `(doom-modeline-inactive-bar ((t (:background ,color2 :foreground ,color5))))
   `(doom-modeline-info ((t (:foreground ,color11))))
   `(doom-modeline-lsp-error ((t (:foreground ,color10))))
   `(doom-modeline-lsp-success ((t (:foreground ,color11))))
   `(doom-modeline-lsp-warning ((t (:foreground ,color12))))
   `(doom-modeline-persp-name ((t (:foreground ,color8))))
   `(doom-modeline-project-dir ((t (:foreground ,color12 :weight bold))))
   `(doom-modeline-project-parent-dir ((t (:foreground ,color9))))
   `(doom-modeline-project-root ((t (:foreground ,color8))))
   `(doom-modeline-repl-error ((t (:foreground ,color10))))
   `(doom-modeline-repl-success ((t (:foreground ,color11))))
   `(doom-modeline-repl-warning ((t (:foreground ,color12))))
   `(doom-modeline-warning ((t (:foreground ,color12))))
   `(doom-modeline-workspace-number ((t (:foreground ,color10))))

   ;; Line number faces (comprehensive)
   ;; Native display-line-numbers-mode
   `(line-number ((t (:foreground ,color9 :background ,color0 :slant normal))))
   `(line-number-current-line ((t (:foreground ,color8 :weight bold))))
   `(line-number-major-tick ((t (:foreground ,color12 :background ,color0 :weight bold))))
   `(line-number-minor-tick ((t (:foreground ,color9 :background ,color0 :weight normal))))

   ;; Legacy linum-mode support
   `(linum ((t (:foreground ,color9 :background ,color0 :slant normal :height 0.9))))
   `(linum-relative-current-face ((t (:foreground ,color8 :background ,color2 :weight bold :height 1.0))))

   ;; Line highlighting integration
   `(hl-line ((t (:background ,color2))))
   `(line-number-current-line-hl ((t (:foreground ,color8 :background ,color3 :weight bold :height 1.0))))

   ;; Relative line numbers (when enabled)
   `(line-number-relative ((t (:foreground ,color9 :background ,color0 :height 0.9))))
   `(line-number-relative-current ((t (:foreground ,color8 :background ,color2 :weight bold :height 1.0))))

   ;; Parentheses and delimiter faces
   ;; Built-in show-paren-mode
   `(show-paren-match ((t (:foreground "#be1558" :bold t))))
   `(show-paren-match-expression ((t (:background ,color2))))
   `(show-paren-mismatch ((t (:background ,error-muted :foreground ,color15 :weight bold :strike-through t))))



   ;; Smartparens integration
   `(sp-pair-overlay-face ((t (:background ,color2))))
   `(sp-show-pair-match-face ((t (:foreground ,color11 :weight bold))))
   `(sp-show-pair-mismatch-face ((t (:background ,color10 :foreground ,color5 :weight bold))))
   `(sp-wrap-overlay-face ((t (:background ,color2))))
   `(sp-wrap-tag-overlay-face ((t (:background ,color2))))

   ;; Rainbow-delimiters faces
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,color10))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,color8))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,color11))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,color12))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,color8))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,color10))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,color11))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,color12))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,color8))))
   `(rainbow-delimiters-unmatched-face ((t (:background ,error-muted :foreground ,color5 :strike-through t))))
   `(rainbow-delimiters-mismatched-face ((t (:background ,error-muted :foreground ,color5 :strike-through t))))

   ))

(custom-theme-set-variables
 'vercel-2024
 '(linum-format " %3i "))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun vercel-2024-theme()
  "Apply the vercel-2024-theme."
  (interactive)
  (load-theme 'vercel-2024 t))

(provide-theme 'vercel-2024)
