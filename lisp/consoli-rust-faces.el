;;; consoli-rust-faces.el --- Rust syntax highlights -*- lexical-binding: t; -*-

(require 'treesit)
(require 'font-lock)
(require 'dash)

(defface consoli-config-rust-unwrap-face '((t (:inherit error)))
  "Face for panic-prone method calls and panic!."
  :group 'rust-ts)
(defface consoli-config-rust-macro-face '((t (:inherit warning :underline t)))
  "Face for unfinished Rust macros."
  :group 'rust-ts)
(defface consoli-config-rust-debug-face '((t (:inherit font-lock-builtin-face :slant italic)))
  "Face for the dbg! macro."
  :group 'rust-ts)
(defface consoli-config-rust-propagation-face '((t (:inherit font-lock-operator-face :weight bold)))
  "Face for the try operator."
  :group 'rust-ts)
(defface consoli-config-rust-unsafe-face '((t (:inherit warning :weight bold)))
  "Face for the unsafe keyword."
  :group 'rust-ts)
(defface consoli-config-rust-attribute-face '((t (:inherit font-lock-preprocessor-face :slant italic)))
  "Face for Rust attribute names."
  :group 'rust-ts)

(defconst consoli-rust--faces
  '(consoli-config-rust-unwrap-face consoli-config-rust-macro-face
    consoli-config-rust-propagation-face consoli-config-rust-unsafe-face
    consoli-config-rust-attribute-face consoli-config-rust-debug-face))

(defun consoli-rust--prefer-syntax-faces (beg end &optional _loudly)
  "Keep targeted Rust faces ahead of semantic faces added between BEG and END."
  (when (bound-and-true-p eglot-semantic-tokens-mode)
    (with-silent-modifications
      (while (< beg end)
        (let* ((faces (get-text-property beg 'face))
               (next (next-single-property-change beg 'face nil end))
               (special (and (consp faces)
                             (-first (lambda (f) (memq f consoli-rust--faces)) faces))))
          (when (and special (not (eq special (car faces))))
            (put-text-property beg next 'face (cons special (remq special faces))))
          (setq beg next))))))

(defun consoli-config/rust-ts-mode-highlighting ()
  "Enable targeted Rust syntax faces at Tree-sitter highlighting level four."
  (when (and treesit-primary-parser
             (not (memq 'consoli-rust (-flatten-n 1 treesit-font-lock-feature-list))))
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (treesit-font-lock-rules
                         :language 'rust :feature 'consoli-rust :override t
                         `(((call_expression
                             function: [(field_expression
                                         field: (field_identifier) @consoli-config-rust-unwrap-face)
                                        (generic_function
                                         function: (field_expression
                                          field: (field_identifier) @consoli-config-rust-unwrap-face))])
                            (:match ,(rx bos (or "unwrap" "expect") eos) @consoli-config-rust-unwrap-face))
                           ((macro_invocation
                             macro: [(identifier) @consoli-config-rust-unwrap-face
                                     (scoped_identifier name: (identifier) @consoli-config-rust-unwrap-face)])
                            (:equal "panic" @consoli-config-rust-unwrap-face))
                           ((macro_invocation
                             macro: [(identifier) @consoli-config-rust-macro-face
                                     (scoped_identifier name: (identifier) @consoli-config-rust-macro-face)])
                            (:match ,(rx bos (or "todo" "unimplemented") eos) @consoli-config-rust-macro-face))
                           ((macro_invocation
                             macro: [(identifier) @consoli-config-rust-debug-face
                                     (scoped_identifier name: (identifier) @consoli-config-rust-debug-face)])
                            (:equal "dbg" @consoli-config-rust-debug-face))
                           (try_expression "?" @consoli-config-rust-propagation-face)
                           "unsafe" @consoli-config-rust-unsafe-face
                           (attribute :anchor [(identifier) @consoli-config-rust-attribute-face
                                         (scoped_identifier name: (identifier) @consoli-config-rust-attribute-face)])))))
    (setq-local treesit-font-lock-feature-list
                (treesit-merge-font-lock-feature-list
                 treesit-font-lock-feature-list '(nil nil nil (consoli-rust))))
    (treesit-font-lock-recompute-features)
    (add-function :after (local 'font-lock-fontify-region-function)
                  #'consoli-rust--prefer-syntax-faces)
    (font-lock-flush)))

(provide 'consoli-rust-faces)
;;; consoli-rust-faces.el ends here
