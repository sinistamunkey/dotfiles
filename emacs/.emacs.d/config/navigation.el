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
  ;; Hide files from .gitignore
  (treemacs-hide-gitignored-files-mode 1)
  ;; Disable line numbers in treemacs
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))
  :bind
  (("<f8>" . treemacs)
   ("C-c t" . treemacs)))

;; Treemacs-Projectile integration
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Treemacs-Magit integration for better git support
(use-package treemacs-magit
  :after (treemacs magit))

;; Display git branch in treemacs modeline
(defun my-treemacs-show-git-branch ()
  "Display git branch in treemacs modeline."
  (when (and (eq major-mode 'treemacs-mode)
             (treemacs-current-workspace))
    (let* ((project (treemacs-project-at-point))
           (path (when project (treemacs-project->path project))))
      (when (and path (vc-git-root path))
        (let ((default-directory path))
          (condition-case nil
              (format " [%s]" (car (vc-git-branches)))
            (error "")))))))

(add-hook 'treemacs-mode-hook
          (lambda ()
            (setq mode-line-format
                  (append mode-line-format
                          '(" " (:eval (my-treemacs-show-git-branch)))))))

;; Balance window sizes
(global-set-key (kbd "C-c =") 'balance-windows)

(provide 'navigation)
;;; navigation.el ends here