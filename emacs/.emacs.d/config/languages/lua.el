;;; lua.el --- Lua development configuration -*- lexical-binding: t; -*-

;; Lua mode
(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq lua-indent-level 2)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil))

;; Tree-sitter support for Lua
(when (and (treesit-available-p)
           (treesit-language-available-p 'lua))
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode)))

;; Lua LSP configuration with Eglot
(with-eval-after-load 'eglot
  ;; Add lua-language-server to Eglot server programs
  ;; lua-language-server can be installed via:
  ;; - macOS: brew install lua-language-server
  ;; - Linux: Install from https://github.com/LuaLS/lua-language-server
  (add-to-list 'eglot-server-programs
               '((lua-mode lua-ts-mode) . ("lua-language-server"))))

;; Lua formatting with StyLua
(defun stylua-format-buffer ()
  "Format current Lua buffer with StyLua"
  (interactive)
  (if (or (eq major-mode 'lua-mode)
          (eq major-mode 'lua-ts-mode))
      (let* ((original-line (line-number-at-pos))
             (original-column (current-column))
             (original-window-start (window-start))
             (stylua-cmd (executable-find "stylua")))
        (if stylua-cmd
            (unwind-protect
                (let ((exit-code (shell-command-on-region
                                 (point-min) (point-max)
                                 "stylua -"
                                 t t "*stylua-error*")))
                  (if (zerop exit-code)
                      (progn
                        (set-buffer-modified-p nil)
                        (message "Buffer formatted with StyLua"))
                    (message "StyLua formatting failed - check *stylua-error* buffer")))
              ;; Always restore position in unwind-protect cleanup form
              (set-window-start (selected-window) original-window-start)
              (goto-char (point-min))
              (forward-line (1- original-line))
              (move-to-column original-column))
          (message "stylua not found - install it first (e.g., brew install stylua or cargo install stylua)")))
    (message "Not a Lua buffer")))

;; Lua mode setup
(defun my-lua-mode-setup ()
  "Setup Lua development environment"
  ;; Enable LSP if lua-language-server is available
  (when (executable-find "lua-language-server")
    (eglot-ensure))
  ;; Auto-format with StyLua on save if available
  (when (executable-find "stylua")
    (add-hook 'after-save-hook 'stylua-format-buffer nil 'make-it-local))
  ;; Remove trailing whitespace on save
  (add-hook 'after-save-hook 'delete-trailing-whitespace nil 'make-it-local))

;; Apply Lua mode setup hooks
(add-hook 'lua-mode-hook #'my-lua-mode-setup)
(when (treesit-available-p)
  (add-hook 'lua-ts-mode-hook #'my-lua-mode-setup))

(provide 'lua)
;;; lua.el ends here
