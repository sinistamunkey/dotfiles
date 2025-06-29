;;; languages.el --- Language mode configurations -*- lexical-binding: t; -*-

;; JSON mode
(use-package json-mode
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

;; YAML mode
(use-package yaml-mode)

;; Markdown mode
(use-package markdown-mode
  :config
  ;; Use GitHub-flavored markdown for README files
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  
  ;; Set markdown command
  (cond
   ((executable-find "pandoc")
    (setq markdown-command "pandoc"))
   ((executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))
   (t (setq markdown-command "markdown")))
  
  ;; Configure live preview
  (when (boundp 'markdown-mode-command-map)
    (define-key markdown-mode-command-map "g" 'markdown-live-preview-mode))
  
  ;; Markdown mode hooks
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'gfm-mode-hook 'visual-line-mode))

(provide 'languages)
;;; languages.el ends here