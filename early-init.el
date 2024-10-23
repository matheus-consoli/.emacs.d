(setenv "LSP_USE_PLISTS" "true")
;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; The default is 800 kilobytes.  Measured in bytes.
;; emacs-lsp performance improvements
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold (* 200 1024 1024)) ;; 200mb
;; Optimizations for improving IO performance. Increase max bytes read from a sub-process in a single op
;; default value: 3145728 (3M)
(setq read-process-output-max (* 5 1024 1024 )) ;; 5mb

;; keep the eln cache clean
(setq load-prefer-newer t)

(setq native-comp-deferred-compilation t)
(setq native-comp-jit-compilation t)
(setq native-comp-prune-cache t)

(provide 'early-init)
