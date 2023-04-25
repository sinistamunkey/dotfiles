;; ===================================
;; Packaging
;; ===================================
(require 'package)
(setq package-archives
'(
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Custom packages
(setq my-packages
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
    use-package
    )
  )

;; Install new packages defined above
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))


;; ===================================
;; Basic Customization
;; ===================================
(setq create-lockfiles nil)         ;; Disable lock files
(setq make-backup-files nil)        ;; Disable backup files
(setq inhibit-startup-message t)    ;; Hide the startup message
(global-linum-mode t)               ;; Enable line numbers globally
(setq linum-format "%d ")           ;; Add a bit of padding to the line numbers
(add-hook 'after-init-hook (lambda () (load-theme 'darcula)))

;; Show full path of buffer in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

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
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; ====================================
;; Development Setup
;; ====================================
;; Fill column editor config
(require 'fill-column-indicator)
(setq fci-rule-column 88)           ;; Set the fill culumn indicator to 88 characters

(setq elpy-rpc-virtualenv-path 'current) ;; Enforce elpy to use current virtualenv

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
   '(org-mode org-modern cython-mode auto-dim-other-buffers nyan-mode blacken tree-sitter-langs fill-column-indicator elpy better-defaults))
 '(python-shell-interpreter "python"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
