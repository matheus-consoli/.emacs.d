(setenv "LSP_USE_PLISTS" "true")
(setq package-enable-at-startup nil)

;; The default is 800 kilobytes.  Measured in bytes.
;; emacs-lsp performance improvements
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold (* 200 1024 1024)) ;; 200mb
(setq read-process-output-max (* 3 1024 1024 )) ;; 3mb

;; keep the eln cache clean
(setq load-prefer-newer noninteractive)

(setq comp-deferred-compilation t)
(setq native-comp-jit-compilation t)

(provide 'early-init)
