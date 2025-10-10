;;; development.el --- Development tools configuration -*- lexical-binding: t; -*-

;; Version control
(use-package magit)

;; Fill column indicator
(use-package fill-column-indicator
  :config
  (setq fci-rule-column 88))

;; Eglot LSP configuration
(use-package eglot
  :config
  (setq tab-always-indent 'complete)

  ;; Enable completion at point for eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                         (cons #'eglot-completion-at-point
                               completion-at-point-functions)))))

;; Company mode configuration (minimal to avoid conflicts)
(when (require 'company nil 'noerror)
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 2))

;; realgud - visual debugger interface
(use-package realgud
  :config
  ;; Add keybinding for pdb in Python mode
  (with-eval-after-load 'python
    (define-key python-mode-map (kbd "C-c d b") 'realgud:pdb)))

;; vterm - full terminal emulator
(use-package vterm
  :ensure t
  :defer t
  :bind (("C-c t" . (lambda () (interactive) (vterm t)))
         ("C-c C-t" . my-vterm-toggle))
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-shell "/bin/zsh")
  ;; Disable line numbers in vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (setq-local left-margin-width 2)
              (setq-local left-fringe-width 0)
              (set-window-buffer nil (current-buffer))
              (set-window-margins nil 2)))

  (defun my-vterm-toggle ()
    "Toggle vterm in bottom split."
    (interactive)
    (let ((vterm-buf (get-buffer "*vterm*")))
      (if (and vterm-buf (get-buffer-window vterm-buf))
          ;; If visible, hide it
          (delete-window (get-buffer-window vterm-buf))
        ;; Otherwise show it
        (progn
          (when (one-window-p)
            (split-window-below -15))
          (if (< (window-height) 20)
              (other-window 1)
            (progn
              (split-window-below -15)
              (other-window 1)))
          (if vterm-buf
              (switch-to-buffer vterm-buf)
            (vterm))))))

  ;; Auto-start vterm on GUI Emacs startup
  (defun my-vterm-startup ()
    "Start vterm in bottom pane on GUI Emacs startup."
    (when (and (display-graphic-p)
               (featurep 'vterm))
      (split-window-below -15)
      (other-window 1)
      (vterm)
      (other-window 1)))

  (add-hook 'vterm-mode-hook
            (lambda ()
              (remove-hook 'emacs-startup-hook #'my-vterm-startup))))

(provide 'development)
;;; development.el ends here
