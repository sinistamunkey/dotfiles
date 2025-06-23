;;; development.el --- Development tools configuration -*- lexical-binding: t; -*-

;; Version control
(use-package magit)

;; Fill column indicator
(use-package fill-column-indicator
  :config
  (setq fci-rule-column 88))

;; Eglot LSP configuration
(use-package eglot
  :config
  (setq tab-always-indent 'complete)
  
  ;; Enable completion at point for eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                         (cons #'eglot-completion-at-point
                               completion-at-point-functions)))))

;; Company mode configuration (minimal to avoid conflicts)
(when (require 'company nil 'noerror)
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 2))

(provide 'development)
;;; development.el ends here