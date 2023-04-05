;; ===================================
;; Packaging
;; ===================================
;; Configure MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Custom packages
(defvar my-packages
  '(auto-dim-other-buffers
    better-defaults
    blacken
    darcula-theme
    elpy
    fill-column-indicator
    nyan-mode
    python-isort
    yasnippet-snippets
    tree-sitter
    tree-sitter-langs
    )
  )

;; Install new packages defined above
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; ===================================
;; Basic Customization
;; ===================================
(setq create-lockfiles nil)         ;; Disable lock files
(setq make-backup-files nil)        ;; Disable backup files
(setq inhibit-startup-message t)    ;; Hide the startup message
(global-linum-mode t)               ;; Enable line numbers globally
(add-hook 'after-init-hook (lambda () (load-theme 'darcula)))

;; Configure IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(nyan-mode 1)
(yas-global-mode 1)

(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

;; ===================================
;; Custom key mapping
;; ===================================
(global-set-key (kbd "C-c i") 'yas-insert-snippet)

;; ====================================
;; Development Setup
;; ====================================
;; Fill column editor config
(require 'fill-column-indicator)
(setq fci-rule-column 88)           ;; Set the fill culumn indicator to 88 characters

;; treesitter config
(require 'tree-sitter)
(require 'tree-sitter-langs)

;; Python mode hooks
(add-hook 'python-mode-hook #'fci-mode)
(add-hook 'python-mode-hook #'elpy-enable)
(add-hook 'python-mode-hook #'tree-sitter-mode)
(add-hook 'python-mode-hook #'blacken-mode)
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'delete-trailing-whitespace nil 'make-it-local)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'blacken-buffer nil 'make-it-local)))
(add-hook 'python-mode-hook #'python-isort-on-save-mode)

;; ====================================
;; Custom helpers
;; ====================================
(defun uuid ()
  (interactive)
  (shell-command "uuidgen" t)
  (let ((beg (point)))
    (forward-word 5)
    (downcase-region beg (point)))
  )

;; ====================================
;; OSX modifications
;; ====================================
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

;; ====================================
;; Automatically generated custom
;; ====================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-mode t)
 '(custom-safe-themes
   '("79586dc4eb374231af28bbc36ba0880ed8e270249b07f814b0e6555bdcb71fab" default))
 '(package-selected-packages
   '(auto-dim-other-buffers nyan-mode blacken tree-sitter-langs fill-column-indicator elpy better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
