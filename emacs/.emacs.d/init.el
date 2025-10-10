;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;; ===================================
;; Package Management
;; ===================================
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install use-package if not available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Enable auto-refresh of buffers when files change on disk
(global-auto-revert-mode 1)

;; Save and restore Emacs sessions (open files, window config)
(setq desktop-save t)  ; Always save without prompting
(desktop-save-mode 1)

;; Persistent scratch buffer
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (setq persistent-scratch-save-file
        (expand-file-name ".emacs-scratch" user-emacs-directory)))

;; ===================================
;; Load Custom File First
;; ===================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ===================================
;; Load Configuration Modules
;; ===================================
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'ui)
(require 'navigation)
(require 'development)
(require 'python-config)
(require 'languages)
(require 'org-mode)
(require 'platform)
(require 'functions)
(require 'images)
(require 'xwidget-config)
(require 'sql-config)

;;; init.el ends here