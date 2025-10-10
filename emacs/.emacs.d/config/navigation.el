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

;; Projectile - project management
(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Treemacs - modern file tree browser
(use-package treemacs
  :config
  (setq treemacs-width 35
        treemacs-follow-mode t
        treemacs-filewatch-mode t
        treemacs-fringe-indicator-mode 'always-visible
        treemacs-git-mode 'deferred)
  ;; Disable line numbers in treemacs
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))
  :bind
  (("<f8>" . treemacs)
   ("C-c t" . treemacs)))

;; Treemacs-Projectile integration
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Balance window sizes
(global-set-key (kbd "C-c =") 'balance-windows)

(provide 'navigation)
;;; navigation.el ends here