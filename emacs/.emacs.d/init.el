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
  '(ag
    better-defaults
    blacken
    editorconfig
    elpy
    fill-column-indicator
    imenu-list
    json-mode
    pyenv-mode
    py-isort
    python-pytest
    yaml-mode
    yasnippet-snippets
    tree-sitter
    tree-sitter-langs))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Initialise packages
(setq inhibit-startup-message t)   ;; hide the startup message
(ido-mode t)                       ;; enable ido mode

;; BASIC CONFIGURATION
;; ----------------------------------------------------
(setq frame-background-mode 'dark)
(setq x-select-enable-clipboard t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq linum-format "%d ")

;; Enable and configure editing modes
(global-linum-mode)
(editorconfig-mode 1)
(imenu-list-minor-mode)
(require 'fill-column-indicator)
(require 'yasnippet)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(yas-global-mode 1)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(setq fci-rule-column 90)
(global-fci-mode t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Custom helpers
(defun uuid ()
  (interactive)
  (shell-command "uuidgen" t)
  (let ((beg (point)))
    (forward-word 5)
    (downcase-region beg (point)))
)
;; Support copy and paste to osx clipboard
;; https://gist.github.com/the-kenny/267162
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Hooks
(add-hook 'python-mode-hook #'elpy-enable)
(add-hook 'python-mode-hook #'pyenv-mode)
(add-hook 'python-mode-hook #'blacken-mode)
(add-hook 'python-mode-hook #'tree-sitter-mode)
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'delete-trailing-whitespace nil 'make-it-local)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'blacken-buffer nil 'make-it-local)))
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
(add-hook 'after-save-hook #'editorconfig-apply)

;; Key mapping
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c '") #'imenu-list-smart-toggle)
(global-set-key (kbd "C-x <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <down>") 'shrink-window)
(global-set-key (kbd "C-x <up>") 'enlarge-window)
(global-set-key (kbd "C-c s") 'py-isort-buffer)
(global-set-key (kbd "C-c i") 'yas-insert-snippet)


;; Custom settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tree-sitter-langs tree-sitter ag python-pytest pbcopy imenu-list py-isort json-mode editorconfig yasnippet-snippets blacken pyenv-mode elpy better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
