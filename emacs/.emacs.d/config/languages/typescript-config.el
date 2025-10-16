;;; typescript-config.el --- TypeScript and React development configuration -*- lexical-binding: t; -*-

;; TypeScript mode
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-indent-level 2))

;; Tree-sitter support for TypeScript and TSX
(when (and (treesit-available-p)
           (treesit-language-available-p 'typescript))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

(when (and (treesit-available-p)
           (treesit-language-available-p 'tsx))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

;; TypeScript LSP configuration with Eglot
(with-eval-after-load 'eglot
  ;; Add typescript-language-server to Eglot server programs
  ;; typescript-language-server can be installed via:
  ;; - npm install -g typescript-language-server typescript
  ;; - Or per-project: npm install --save-dev typescript-language-server typescript
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))))

;; Prettier formatting for TypeScript/React
(defun prettier-format-buffer ()
  "Format current TypeScript/React buffer with Prettier"
  (interactive)
  (if (or (eq major-mode 'typescript-mode)
          (eq major-mode 'typescript-ts-mode)
          (eq major-mode 'tsx-ts-mode)
          (eq major-mode 'js-mode)
          (eq major-mode 'js-ts-mode)
          (eq major-mode 'json-mode))
      (let* ((original-line (line-number-at-pos))
             (original-column (current-column))
             (original-window-start (window-start))
             (prettier-cmd (or (executable-find "prettier")
                             (let ((local-prettier (concat (locate-dominating-file default-directory "node_modules")
                                                          "node_modules/.bin/prettier")))
                               (when (file-exists-p local-prettier)
                                 local-prettier)))))
        (if prettier-cmd
            (unwind-protect
                (let ((exit-code (shell-command-on-region
                                 (point-min) (point-max)
                                 (format "%s --stdin-filepath %s"
                                        prettier-cmd
                                        (shell-quote-argument (or (buffer-file-name) "stdin.tsx")))
                                 t t "*prettier-error*")))
                  (if (zerop exit-code)
                      (progn
                        (set-buffer-modified-p nil)
                        (message "Buffer formatted with Prettier"))
                    (message "Prettier formatting failed - check *prettier-error* buffer")))
              ;; Always restore position in unwind-protect cleanup form
              (set-window-start (selected-window) original-window-start)
              (goto-char (point-min))
              (forward-line (1- original-line))
              (move-to-column original-column))
          (message "prettier not found - install it first (npm install -g prettier)")))
    (message "Not a JavaScript/TypeScript/JSON buffer")))

;; ESLint integration
(defun eslint-fix-file ()
  "Run ESLint --fix on current file"
  (interactive)
  (when (and buffer-file-name
             (or (eq major-mode 'typescript-mode)
                 (eq major-mode 'typescript-ts-mode)
                 (eq major-mode 'tsx-ts-mode)
                 (eq major-mode 'js-mode)
                 (eq major-mode 'js-ts-mode)))
    (let* ((eslint-cmd (or (executable-find "eslint")
                          (let ((local-eslint (concat (locate-dominating-file default-directory "node_modules")
                                                     "node_modules/.bin/eslint")))
                            (when (file-exists-p local-eslint)
                              local-eslint)))))
      (if eslint-cmd
          (progn
            (shell-command (format "%s --fix %s"
                                 eslint-cmd
                                 (shell-quote-argument buffer-file-name)))
            (revert-buffer t t t)
            (message "ESLint fix applied"))
        (message "eslint not found - install it in your project")))))

;; TypeScript mode setup
(defun my-typescript-mode-setup ()
  "Setup TypeScript/React development environment"
  ;; Enable LSP if typescript-language-server is available
  (when (or (executable-find "typescript-language-server")
            (file-exists-p (concat (locate-dominating-file default-directory "node_modules")
                                  "node_modules/.bin/typescript-language-server")))
    (eglot-ensure))
  ;; Auto-format with Prettier on save if available
  (when (or (executable-find "prettier")
            (file-exists-p (concat (locate-dominating-file default-directory "node_modules")
                                  "node_modules/.bin/prettier")))
    (add-hook 'after-save-hook 'prettier-format-buffer nil 'make-it-local))
  ;; Remove trailing whitespace on save
  (add-hook 'after-save-hook 'delete-trailing-whitespace nil 'make-it-local)
  ;; Keybindings for common actions
  (local-set-key (kbd "C-c f") 'prettier-format-buffer)
  (local-set-key (kbd "C-c e") 'eslint-fix-file))

;; Apply TypeScript mode setup hooks
(add-hook 'typescript-mode-hook #'my-typescript-mode-setup)
(when (treesit-available-p)
  (add-hook 'typescript-ts-mode-hook #'my-typescript-mode-setup)
  (add-hook 'tsx-ts-mode-hook #'my-typescript-mode-setup))

;; web-mode for better JSX/TSX support (fallback if tree-sitter not available)
(use-package web-mode
  :mode (("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.jsx?\\'")))
  (add-hook 'web-mode-hook #'my-typescript-mode-setup))

;; Import helper function for automatic imports
(defun typescript-add-import ()
  "Add import statement at top of file (use LSP code actions for better results)"
  (interactive)
  (message "Use C-c a (eglot-code-actions) for automatic imports via LSP"))

;; React snippets and helpers
(with-eval-after-load 'yasnippet
  ;; Add React-specific snippets directory if it exists
  (let ((react-snippets-dir (expand-file-name "snippets/tsx-ts-mode" user-emacs-directory)))
    (when (file-directory-p react-snippets-dir)
      (add-to-list 'yas-snippet-dirs react-snippets-dir t))))

(provide 'typescript-config)
;;; typescript-config.el ends here
