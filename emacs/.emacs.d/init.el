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
    blacken
    editorconfig
    elpy
    fill-column-indicator
    imenu-list
    json-mode
    pyenv-mode
    py-isort
    yaml-mode
    yasnippet-snippets
    go-autocomplete
    go-guru))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Initialise packages
(setq inhibit-startup-message t)   ;; hide the startup message
(ido-mode t)                       ;; enable ido mode

;; BASIC CONFIGURATION
;; ----------------------------------------------------
(setq frame-background-mode 'dark)

;; Enable and configure editing modes
(global-linum-mode)
(editorconfig-mode 1)
(imenu-list-minor-mode)
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


;; GO settings
(setenv "GOPATH" (getenv "GOPATH"))
(defun auto-complete-for-go ()

(auto-complete-mode 1))
 (add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ;; guru settings
  (go-guru-hl-identifier-mode)

  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (require 'go-guru)
  ;; (add-to-list 'load-path (concat (getenv "GOPATH") "src/github.com/dougm/goflymake"))
  ;; (require 'go-flycheck)
)

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Custom settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (go-guru go-autocomplete go-mode imenu-list py-isort json-mode editorconfig protobuf-mode yasnippet-snippets blacken pyenv-mode elpy better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
