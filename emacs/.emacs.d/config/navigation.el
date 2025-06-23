;;; navigation.el --- Navigation and completion configuration -*- lexical-binding: t; -*-

;; IDO mode configuration
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Better defaults
(use-package better-defaults)

;; Snippets
(use-package yasnippet-snippets
  :config
  (when (fboundp 'yas-global-mode)
    (yas-global-mode 1)))

(provide 'navigation)
;;; navigation.el ends here