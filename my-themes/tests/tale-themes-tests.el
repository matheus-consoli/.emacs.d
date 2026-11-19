;;; tale-themes-tests.el --- Tale theme regression tests -*- lexical-binding: t; -*-

;; From the configuration root:
;; emacs -Q --batch -L my-themes -l my-themes/tests/tale-themes-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'color)
(require 'tale-themes-common)

(add-to-list 'custom-theme-load-path (file-name-directory (locate-library "tale-themes-common")))

(ert-deftest tale-themes-cycle-includes-every-palette ()
  (let ((available (seq-filter (lambda (name) (string-suffix-p "-tale" (symbol-name name)))
                               (custom-available-themes)))
        seen)
    (unwind-protect
        (progn
          (dolist (_ available)
            (tale-themes-switch-variant)
            (should (= (length custom-enabled-themes) 1))
            (let ((active (car custom-enabled-themes)))
              (should (memq active available))
              (should-not (memq active seen))
              (push active seen)))
          (tale-themes-switch-variant)
          (should (eq (car custom-enabled-themes) 'dark-tale)))
      (mapc #'disable-theme custom-enabled-themes))))

(defvar tale-themes-test-event-hook nil)

(ert-deftest tale-themes-hook-cleanup-after-option-edits ()
  (let ((original tale-themes-special-modes))
    (unwind-protect
        (with-temp-buffer
          (setopt tale-themes-special-modes '(tale-themes-test-event-hook))
          (load-theme 'dark-tale t)
          (run-hooks 'tale-themes-test-event-hook)
          (should tale-themes--special-remaps)
          (setopt tale-themes-special-modes '(compilation-mode-hook))
          (should-not tale-themes-test-event-hook)
          (should-not tale-themes--special-remaps)
          (compilation-mode)
          (should tale-themes--special-remaps)
          (setq tale-themes-special-modes nil)
          (disable-theme 'dark-tale)
          (should-not tale-themes--special-remaps)
          (with-temp-buffer
            (compilation-mode)
            (should-not tale-themes--special-remaps)))
      (disable-theme 'dark-tale)
      (setopt tale-themes-special-modes original))))

(defun tale-themes-test--luminance (hex)
  "Return relative luminance from HEX, independent of the display's color depth."
  (let ((rgb (mapcar (lambda (value) (/ value 65535.0))
                     (color-values-from-color-spec hex))))
    (cadr (apply #'color-srgb-to-xyz rgb))))

(ert-deftest tale-themes-readable-syntax ()
  (dolist (theme tale-themes--variants)
    (unwind-protect
        (progn
          (load-theme theme t)
          (let ((bg (tale-themes-test--luminance (face-background 'default))))
            (dolist (face '(font-lock-keyword-face font-lock-string-face font-lock-number-face
                            font-lock-type-face font-lock-variable-name-face font-lock-operator-face
                            font-lock-comment-face font-lock-doc-face font-lock-negation-char-face
                            font-lock-property-name-face font-lock-bracket-face font-lock-delimiter-face))
              (ert-info ((format "%s / %s" theme face))
                (let ((fg (tale-themes-test--luminance (face-foreground face))))
                  (should (>= (/ (+ (max fg bg) 0.05) (+ (min fg bg) 0.05)) 4.5)))))))
      (disable-theme theme))))

(ert-deftest tale-themes-rust-tokens-use-themed-faces ()
  (require 'rust-ts-mode)
  (let ((treesit-extra-load-path (cons (expand-file-name "../tree-sitter"
                                                        (file-name-directory (locate-library "tale-themes-common")))
                                       treesit-extra-load-path))
        (treesit-font-lock-level 4))
    (skip-unless (treesit-ready-p 'rust t))
    (unwind-protect
        (with-temp-buffer
          (load-theme 'dark-tale t)
          (insert "fn main() { let n = 42 + 1; let s = \"a\\n\"; }")
          (rust-ts-mode)
          (font-lock-ensure)
          (goto-char (point-min))
          (dolist (entry '(("(" font-lock-bracket-face shadow)
                           ("42" font-lock-number-face font-lock-constant-face)
                           ("+" font-lock-operator-face font-lock-keyword-face)
                           (";" font-lock-delimiter-face shadow)
                           ("\\n" font-lock-escape-face font-lock-string-face)))
            (search-forward (car entry))
            (let ((face (get-text-property (1- (point)) 'face)))
              (should (eq face (cadr entry)))
              (should (equal (face-foreground face) (face-foreground (nth 2 entry)))))))
      (disable-theme 'dark-tale))))

(ert-deftest tale-themes-rust-review-highlighting ()
  (require 'rust-ts-mode)
  (let* ((root (expand-file-name ".." (file-name-directory (locate-library "tale-themes-common"))))
         (load-path (cons (expand-file-name "lisp" root) load-path))
         (treesit-extra-load-path (cons (expand-file-name "tree-sitter" root) treesit-extra-load-path))
         (treesit-font-lock-level 4))
    (skip-unless (treesit-ready-p 'rust t))
    (require 'consoli-rust-faces)
    (let ((rust-ts-mode-hook '(consoli-config/rust-ts-mode-highlighting)))
      (with-temp-buffer
        (insert "#![allow(dead_code)]\n#[test] unsafe fn example<T: ?Sized>() {\n"
                "let _ = object.unwrap; value.unwrap(); result.expect(\"reason\");\n"
                "value.expect::<T>(); core::panic!(); todo!(); core::unimplemented!(); dbg!(x);\n"
                "let _ = read()?; unsafe { f() }; let _ = \"unsafe todo! ?\"; }")
        (rust-ts-mode)
        (consoli-config/rust-ts-mode-highlighting)
        (should (= 1 (cl-count 'consoli-rust (apply #'append treesit-font-lock-feature-list))))
        (font-lock-ensure)
        (goto-char (point-min))
        (dolist (entry '(("allow" consoli-config-rust-attribute-face)
                         ("test" consoli-config-rust-attribute-face)
                         ("unsafe" consoli-config-rust-unsafe-face)
                         ("?" nil)
                         ("object.unwrap" nil)
                         ("value.unwrap" consoli-config-rust-unwrap-face)
                         ("result.expect" consoli-config-rust-unwrap-face)
                         ("value.expect" consoli-config-rust-unwrap-face)
                         ("core::panic" consoli-config-rust-unwrap-face)
                         ("todo" consoli-config-rust-macro-face)
                         ("core::unimplemented" consoli-config-rust-macro-face)
                         ("dbg" consoli-config-rust-debug-face)
                         ("read()?" consoli-config-rust-propagation-face)
                         ("unsafe" consoli-config-rust-unsafe-face)
                         ("unsafe todo! ?" font-lock-string-face)))
          (search-forward (car entry))
          (let ((face (get-text-property (1- (point)) 'face)))
            (ert-info ((car entry))
              (if (cadr entry)
                  (should (eq face (cadr entry)))
                (should-not (memq face consoli-rust--faces))))))
        (let ((treesit-font-lock-level 3))
          (treesit-font-lock-recompute-features)
          (font-lock-flush)
          (font-lock-ensure)
          (goto-char (point-min))
          (search-forward "value.unwrap")
          (should-not (memq (get-text-property (1- (point)) 'face) consoli-rust--faces)))))))

(ert-deftest tale-themes-integrations-use-package-faces ()
  (let* ((theme-dir (file-name-directory (locate-library "tale-themes-common")))
         (build-dir (expand-file-name "../straight/build" theme-dir))
         (load-path (append (when (file-directory-p build-dir)
                              (directory-files build-dir t "^[^.]"))
                            load-path))
         (packages '(avy breadcrumb consult copilot corfu corfu-echo corfu-quick
                     dape diff-mode embark jinx smerge-mode)))
    (skip-unless (seq-every-p (lambda (pkg) (locate-library (symbol-name pkg))) packages))
    ;; The theme must also work when the owning packages load later.
    (unwind-protect
        (progn
          (load-theme 'dark-tale t)
          (mapc #'require packages)
          (let ((names (cl-loop for (kind name) in (get 'dark-tale 'theme-settings)
                                when (eq kind 'theme-face) collect name)))
            (dolist (face names)
              (when (string-match-p
                     (rx string-start (or "avy-" "breadcrumb-" "consult-" "copilot-" "corfu-"
                                          "dape-" "diff-refine-" "embark-" "jinx-" "smerge-"))
                     (symbol-name face))
                (should (facep face))))
            (dolist (face '(avy-lead-face breadcrumb-imenu-leaf-face consult-highlight-match
                            copilot-overlay-face dape-source-line-face dape-breakpoint-face
                            embark-target jinx-misspelled diff-refine-added smerge-refined-added))
              (should (memq face names))))
          (should (equal (face-attribute 'smerge-refined-added :inherit) 'diff-refine-added))
          (should (equal (face-foreground 'copilot-overlay-face) (face-foreground 'shadow)))
          (should (stringp (face-background 'dape-source-line-face))))
      (disable-theme 'dark-tale))))

;;; tale-themes-tests.el ends here
