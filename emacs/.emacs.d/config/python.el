;;; python.el --- Python development configuration -*- lexical-binding: t; -*-

;; Tree-sitter configuration
(setq treesit-font-lock-level 4)

;; Use python-ts-mode if available
(when (and (treesit-available-p) 
           (treesit-language-available-p 'python))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode)))

;; Poetry helper functions
(defun my-poetry-venv-path ()
  "Get the poetry virtualenv path for current project"
  (when (locate-dominating-file default-directory "pyproject.toml")
    (string-trim (shell-command-to-string "poetry env info --path"))))

(defun my-get-pylsp-command ()
  "Get pylsp command, preferring Poetry venv version"
  (if-let ((venv-path (my-poetry-venv-path)))
      (let ((venv-pylsp (concat venv-path "/bin/pylsp")))
        (if (file-exists-p venv-pylsp)
            venv-pylsp
          (executable-find "pylsp")))
    (executable-find "pylsp")))

;; Poetry environment setup
(defun my-setup-poetry-venv ()
  "Setup Poetry virtualenv for Python development"
  (when-let ((venv-path (my-poetry-venv-path)))
    (setq-local pyvenv-activate venv-path)
    (setq-local python-shell-virtualenv-root venv-path)
    ;; Auto-install pylsp if needed
    (let ((pylsp-in-venv (concat venv-path "/bin/pylsp")))
      (unless (file-exists-p pylsp-in-venv)
        (when (y-or-n-p "Install python-lsp-server in this Poetry environment? ")
          (message "Installing python-lsp-server...")
          (shell-command "poetry run pip install 'python-lsp-server[all]' python-lsp-ruff"))))
    ;; Configure pylsp with ruff
    (setq-local eglot-workspace-configuration
                `((:pylsp . (:plugins (:ruff (:enabled t)
                                      :pycodestyle (:enabled nil)
                                      :pyflakes (:enabled nil)
                                      :mccabe (:enabled nil))))))
    (message "Poetry environment configured for pylsp with ruff")))

;; Ruff formatting
(defun ruff-format-buffer ()
  "Format current Python buffer with ruff (includes import sorting)"
  (interactive)
  (if (or (eq major-mode 'python-mode)
          (eq major-mode 'python-ts-mode))
      (let* ((original-point (point))
             (ruff-cmd (cond
                        ((locate-dominating-file default-directory "pyproject.toml")
                         "poetry run ruff")
                        ((executable-find "ruff")
                         "ruff")
                        (t nil))))
        (if ruff-cmd
            (save-excursion
              (let ((exit-code (shell-command-on-region 
                               (point-min) (point-max) 
                               (format "%s format --stdin-filename %s -" 
                                       ruff-cmd
                                       (shell-quote-argument (or (buffer-file-name) "stdin.py")))
                               t t "*ruff-error*")))
                (if (zerop exit-code)
                    (progn
                      (goto-char original-point)
                      (message "Buffer formatted with ruff"))
                  (message "Ruff formatting failed - check *ruff-error* buffer"))))
          (message "ruff not found - install globally or add to your Poetry project")))
    (message "Not a Python buffer")))

;; Python mode setup
(defun my-python-mode-setup ()
  "Setup Python development environment"
  (my-setup-poetry-venv)
  (fci-mode)
  (add-hook 'after-save-hook 'delete-trailing-whitespace nil 'make-it-local)
  (add-hook 'after-save-hook 'ruff-format-buffer nil 'make-it-local))

;; Eglot contact function for Python
(defun my-eglot-python-contact (interactive)
  "Dynamic contact function for Python LSP using pylsp with ruff"
  (let ((pylsp-cmd (my-get-pylsp-command)))
    (if pylsp-cmd
        (list pylsp-cmd)
      (error "No pylsp found"))))

;; Eglot Python startup with debugging
(defun my-python-eglot-ensure ()
  "Start Eglot for Python with debugging"
  (let ((pylsp-cmd (my-get-pylsp-command)))
    (message "Starting Eglot for Python in buffer: %s" (current-buffer))
    (message "Using pylsp command: %s" pylsp-cmd)
    (message "Poetry venv path: %s" (my-poetry-venv-path))
    (condition-case err
        (progn
          (eglot-ensure)
          (message "Eglot started successfully"))
      (error (message "Failed to start Eglot: %s" err)))))

;; Configure Eglot for Python
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . my-eglot-python-contact)))

;; Enable features when LSP is available
(when (or (executable-find "ruff-lsp") (executable-find "pylsp"))
  (setq eglot-events-buffer-size 0)
  (add-hook 'python-mode-hook #'my-python-eglot-ensure)
  (add-hook 'python-ts-mode-hook #'my-python-eglot-ensure))

;; Apply Python mode setup
(add-hook 'python-mode-hook #'my-python-mode-setup)
(add-hook 'python-ts-mode-hook #'my-python-mode-setup)

(provide 'python)
;;; python.el ends here