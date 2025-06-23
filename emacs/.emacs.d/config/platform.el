;;; platform.el --- Platform-specific configurations -*- lexical-binding: t; -*-

;; macOS specific configuration
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; macOS clipboard integration
(when (eq system-type 'darwin)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  ;; Configure clipboard functions
  (if (boundp 'interprogram-cut-function)
      (progn
        (setq interprogram-cut-function 'paste-to-osx)
        (setq interprogram-paste-function 'copy-from-osx))
    (when (boundp 'gui-select-text)
      (setq gui-select-text 'paste-to-osx)
      (setq gui-selection-value 'copy-from-osx))))

(provide 'platform)
;;; platform.el ends here