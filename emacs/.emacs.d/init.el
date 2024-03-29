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
    json-mode
    magit
    plantuml-mode
    nyan-mode
    python-isort
    yaml-mode
    yasnippet-snippets
    tree-sitter
    tree-sitter-langs
    typescript-mode
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
(tool-bar-mode -1)                  ;; Disable tool bar
(setq linum-format "%d ")           ;; Add a bit of padding to the line numbers
(setq window-divider-mode nil)      ;; Disable the window divider
(scroll-bar-mode -1)                ;; Disable scroll bar (we have nyancat yo!)
(add-hook 'after-init-hook (lambda () (load-theme 'darcula)))
(add-to-list 'image-types 'svg)

;; Show full path of buffer in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Configure IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Configure PlantUML mode
(setq org-plantuml-jar-path "~/plantuml.jar")
(setq plantuml-jar-path "~/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

;; Configure Org mode babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

;; Auto display inline images within org mode
(setq org-startup-with-inline-images t)

;; Enforce PNG as the output type of plantuml
(setq plantuml-output-type "png")

;; Configure time display
(display-time-mode 1)
(setq display-time-format "%a %d %b %H:%M")

;; Nyan mode for reasons
(nyan-mode 1)

;; Enable yasnippet everywhere
(yas-global-mode 1)

;; Stop rgrep and others opening up lots of panes
(setq split-height-threshold nil
      split-width-threshold nil)

;; COnfigure auto dim buffers
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
(setq elpy-rpc-timeout 10)  ;; Increase timeout due to slow virtualenvs and big codebases


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

;; Elpy hooks
(add-hook 'elpy-mode-hook
	  (lambda ()
	    (define-key elpy-mode-map (kbd "C-<return>") nil)))  ;; I keep pressing this by accident

;; JSON mode hooks
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; PlantUML mode hooks
(add-hook 'plantuml-mode-hook
	  (lambda ()
	    (make-local-variable 'plantuml-indent-level)
	    (setq plantuml-indent-level 2)))

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
 '(display-time-mode t)
 '(elpy-rpc-python-command "python")
 '(package-selected-packages
   '(plantuml-mode ob-mermaid magit yaml-mode json-mode typescript-mode org-mode org-modern cython-mode auto-dim-other-buffers nyan-mode blacken tree-sitter-langs fill-column-indicator elpy better-defaults))
 '(python-shell-interpreter "python")
 '(tool-bar-mode nil)
 '(warning-suppress-types '(((python python-shell-completion-native-turn-on-maybe)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2B2B2B" :foreground "#a9b7c6" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))
