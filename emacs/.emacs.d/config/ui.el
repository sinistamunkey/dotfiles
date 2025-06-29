;;; ui.el --- UI and visual configuration -*- lexical-binding: t; -*-

;; Basic UI settings
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(global-display-line-numbers-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq window-divider-mode nil)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Frame title configuration
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Window splitting behavior
(setq split-height-threshold nil
      split-width-threshold 120)

;; Time display
(display-time-mode 1)
(setq display-time-format "%a %d %b %H:%M")

;; Theme configuration
(use-package nord-theme
  :config
  (setq custom-safe-themes t)
  (add-hook 'after-init-hook (lambda () 
                               (load-theme 'nord t))))

;; Fun packages
(use-package nyan-mode
  :config
  (when (fboundp 'nyan-mode)
    (nyan-mode 1)))

(use-package auto-dim-other-buffers
  :config
  (add-hook 'after-init-hook (lambda ()
    (when (fboundp 'auto-dim-other-buffers-mode)
      (auto-dim-other-buffers-mode t)))))

(provide 'ui)
;;; ui.el ends here