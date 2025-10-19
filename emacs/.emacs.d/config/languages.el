;;; languages.el --- Language mode configurations -*- lexical-binding: t; -*-

;; ===================================
;; Add languages subdirectory to load path
;; ===================================
(add-to-list 'load-path (expand-file-name "languages" (file-name-directory load-file-name)))

;; ===================================
;; Load Language-Specific Configurations
;; ===================================
(let ((lang-dir (expand-file-name "languages" (file-name-directory load-file-name))))
  (load (expand-file-name "python" lang-dir))
  (load (expand-file-name "lua" lang-dir))
  (load (expand-file-name "sql-config" lang-dir))
  (load (expand-file-name "typescript-config" lang-dir))
  (load (expand-file-name "jenkins" lang-dir))
  (load (expand-file-name "org" lang-dir)))

;; ===================================
;; General Language Modes
;; ===================================

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

;; CSV mode
(use-package csv-mode
  :config
  ;; Don't add final newline in CSV files
  (add-hook 'csv-mode-hook
            (lambda ()
              (setq-local require-final-newline nil)
              (setq-local mode-require-final-newline nil))))

(provide 'languages)
;;; languages.el ends here