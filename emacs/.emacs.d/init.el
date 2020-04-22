;; init.el -- emacs configuration
(menu-bar-mode -1)

;; INSTALL PACKAGES
;; ------------------------------------------------------
(require 'package)
(require 'ido)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(better-defaults
    material-theme
    elpy
    pyenv-mode
    blacken
    yasnippet-snippets
    protobuf-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Initialise packages
(setq inhibit-startup-message t)   ;; hide the startup message
(ido-mode t)                       ;; enable ido mode
(load-theme 'material)             ;; enable the material theme

;; BASIC CONFIGURATION
;; ----------------------------------------------------
(global-linum-mode)
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(setq fci-rule-column 120)
(global-fci-mode t)

;; Custom helpers
(defun uuid ()
  (interactive)
  (shell-command "uuidgen" t)
  (let ((beg (point)))
    (forward-word 5)
    (downcase-region beg (point)))
)

;; Hooks
(add-hook 'python-mode-hook #'elpy-enable)
(add-hook 'python-mode-hook #'pyenv-mode)
(add-hook 'python-mode-hook #'blacken-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'blacken-buffer)

;; Key mapping
(global-set-key (kbd "C-x <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <down>") 'shrink-window)
(global-set-key (kbd "C-x <up>") 'enlarge-window)
(global-set-key (kbd "C-c s") 'py-isort-buffer)
(global-set-key (kbd "C-c i") 'yas-insert-snippet)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(package-selected-packages
   (quote
    (protobuf-mode yasnippet-snippets json-mode jinja2-mode magit yaml-mode py-isort blacken pyenv-mode pipenv elpy material-theme better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
