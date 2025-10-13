;;; python.el --- Python development configuration -*- lexical-binding: t; -*-

;; Load built-in python mode first
(require 'python)

;; Jedi for Python 2.7 autocomplete and navigation
(use-package jedi
  :commands jedi:setup
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t))  ; M-. for goto-definition, M-, for go back

;; Tree-sitter configuration
(setq treesit-font-lock-level 4)

;; Python 2.7 detection
(defun my-python-is-python2 ()
  "Check if current project uses Python 2.x"
  (when-let ((python-version-file (locate-dominating-file default-directory ".python-version")))
    (with-temp-buffer
      (insert-file-contents (concat python-version-file ".python-version"))
      (string-match-p "^2\\." (buffer-string)))))

;; Use python-ts-mode if available (but not for Python 2.7)
(when (and (treesit-available-p)
           (treesit-language-available-p 'python))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode)))

;; Force python-mode for Python 2.7 projects
(defun my-python-mode-selector ()
  "Select appropriate Python mode based on Python version"
  (when (my-python-is-python2)
    (python-mode)))

(add-hook 'python-ts-mode-hook #'my-python-mode-selector)

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
          (shell-command "poetry run pip install 'python-lsp-server[all]' python-lsp-ruff rope"))))
    ;; Configure pylsp with ruff and rope for auto-imports
    (setq-local eglot-workspace-configuration
                `((:pylsp . (:plugins (:ruff (:enabled t)
                                      :rope_autoimport (:enabled t)
                                      :rope_completion (:enabled t)
                                      :pycodestyle (:enabled nil)
                                      :pyflakes (:enabled nil)
                                      :mccabe (:enabled nil))))))
    (message "Poetry environment configured for pylsp with ruff and rope")))

;; Ruff formatting and linting
(defun ruff-format-buffer ()
  "Format and fix current Python buffer with ruff (includes import sorting)"
  (interactive)
  (if (or (eq major-mode 'python-mode)
          (eq major-mode 'python-ts-mode))
      (let* ((original-line (line-number-at-pos))
             (original-column (current-column))
             (original-window-start (window-start))
             (ruff-cmd (cond
                        ((locate-dominating-file default-directory "pyproject.toml")
                         "poetry run ruff")
                        ((executable-find "ruff")
                         "ruff")
                        (t nil))))
        (if ruff-cmd
            (unwind-protect
                (progn
                  ;; First run ruff format for code formatting
                  (let ((format-exit-code (shell-command-on-region
                                          (point-min) (point-max)
                                          (format "%s format --stdin-filename %s -"
                                                  ruff-cmd
                                                  (shell-quote-argument (or (buffer-file-name) "stdin.py")))
                                          t t "*ruff-error*")))
                    (if (zerop format-exit-code)
                        ;; Then run ruff check --fix for linting fixes (including import sorting)
                        (let ((check-exit-code (shell-command-on-region
                                               (point-min) (point-max)
                                               (format "%s check --fix --stdin-filename %s -"
                                                       ruff-cmd
                                                       (shell-quote-argument (or (buffer-file-name) "stdin.py")))
                                               t t "*ruff-error*")))
                          (if (zerop check-exit-code)
                              (progn
                                (set-buffer-modified-p nil)
                                (message "Buffer formatted and fixed with ruff"))
                            (message "Ruff check failed - check *ruff-error* buffer")))
                      (message "Ruff formatting failed - check *ruff-error* buffer"))))
              ;; Always restore position in unwind-protect cleanup form
              (set-window-start (selected-window) original-window-start)
              (goto-char (point-min))
              (forward-line (1- original-line))
              (move-to-column original-column))
          (message "ruff not found - install globally or add to your Poetry project")))
    (message "Not a Python buffer")))

;; Jedi setup for Python 2.7
(defun my-setup-jedi-python2 ()
  "Setup jedi for Python 2.7, installing if necessary"
  (setq-local python-shell-interpreter "python2.7")
  ;; Check if jedi is installed for python2.7
  (let ((jedi-check (shell-command-to-string "python2.7 -c 'import jedi' 2>&1")))
    (if (string-match-p "No module named" jedi-check)
        (when (y-or-n-p "Jedi not found for Python 2.7. Install it? ")
          (message "Installing jedi for Python 2.7...")
          (let ((result (shell-command "python2.7 -m pip install jedi")))
            (if (zerop result)
                (progn
                  (jedi:setup)
                  (message "Python 2.7 project detected - jedi installed and enabled"))
              (message "Failed to install jedi - you may need to run: pip2.7 install jedi"))))
      ;; Jedi already installed
      (jedi:setup)
      (message "Python 2.7 project detected - using jedi for completion"))))

;; Python mode setup
(defun my-python-mode-setup ()
  "Setup Python development environment"
  ;; Skip modern tooling for Python 2.7 projects
  (unless (my-python-is-python2)
    (my-setup-poetry-venv)
    (add-hook 'after-save-hook 'ruff-format-buffer nil 'make-it-local))
  ;; Basic setup for all Python projects
  (fci-mode)
  (add-hook 'after-save-hook 'delete-trailing-whitespace nil 'make-it-local)
  ;; Disable automatic eldoc popups (use C-h . for on-demand docs)
  (eldoc-mode -1)
  ;; Set Python 2.7 interpreter and enable jedi if detected
  (when (my-python-is-python2)
    (my-setup-jedi-python2)))

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
  ;; Don't start LSP for Python 2.7 projects
  (unless (my-python-is-python2)
    (let ((pylsp-cmd (my-get-pylsp-command)))
      (message "Starting Eglot for Python in buffer: %s" (current-buffer))
      (message "Using pylsp command: %s" pylsp-cmd)
      (message "Poetry venv path: %s" (my-poetry-venv-path))
      (condition-case err
          (progn
            (eglot-ensure)
            (eldoc-mode -1)  ; Disable eldoc after eglot starts
            (message "Eglot started successfully"))
        (error (message "Failed to start Eglot: %s" err))))))

;; Configure Eglot for Python
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . my-eglot-python-contact))
  ;; Disable eldoc when eglot manages a Python buffer
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (or (eq major-mode 'python-mode)
                        (eq major-mode 'python-ts-mode))
                (eldoc-mode -1)
                ;; Add keybindings for code actions (auto-import, etc.)
                (local-set-key (kbd "C-c a") 'eglot-code-actions)
                (local-set-key (kbd "C-c c r") 'eglot-rename)))))

;; Enable Eglot auto-start for Python
(setq eglot-events-buffer-size 0)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)

;; Apply Python mode setup
(add-hook 'python-mode-hook #'my-python-mode-setup)
(add-hook 'python-ts-mode-hook #'my-python-mode-setup)

;; pytest integration
(use-package python-pytest
  :after python
  :bind (:map python-mode-map
              ("C-c t t" . python-pytest-dispatch)
              ("C-c t f" . python-pytest-file)
              ("C-c t d" . python-pytest-function)
              ("C-c t r" . python-pytest-repeat))
  :config
  ;; Use Poetry's pytest if available
  (when (locate-dominating-file default-directory "pyproject.toml")
    (setq python-pytest-executable "poetry run pytest"))

  ;; Add advice to always include -s flag for interactive debugging
  (defun my-python-pytest-add-capture-flag (args)
    "Add -s flag to pytest arguments for ipdb/pdb interaction."
    (if (member "-s" args)
        args
      (cons "-s" args)))

  (advice-add 'python-pytest--run :filter-args
              (lambda (kwargs)
                (plist-put kwargs :args
                          (my-python-pytest-add-capture-flag
                           (plist-get kwargs :args)))
                kwargs))

  ;; Enable completion in pytest buffer for pdb/ipdb
  ;; Use M-/ for completion instead of TAB to avoid conflicts
  (add-hook 'compilation-mode-hook
            (lambda ()
              (when (string-match-p "\\*pytest\\*" (buffer-name))
                (local-set-key (kbd "M-/") 'completion-at-point)))))

(provide 'python)
;;; python.el ends here

