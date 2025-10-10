;;; xwidget-config.el --- Xwidget webkit browser configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for xwidget-webkit browser for rich HTML preview in GUI Emacs

;;; Code:

;; Xwidget webkit browser configuration
(when (featurep 'xwidget-internal)
  ;; Better buffer naming
  (setq xwidget-webkit-buffer-name-format "*xwidget: %T*")

  ;; Enhanced org-mode HTML preview using xwidget
  (defun my-org-preview-xwidget ()
    "Export current org file to HTML and preview in xwidget-webkit."
    (interactive)
    (let ((html-file (org-html-export-to-html)))
      (xwidget-webkit-browse-url (concat "file://" html-file))))

  ;; Preview any HTML file in xwidget
  (defun my-html-preview-xwidget ()
    "Preview current HTML file in xwidget-webkit."
    (interactive)
    (xwidget-webkit-browse-url (concat "file://" (buffer-file-name))))

  ;; Keybindings
  (global-set-key (kbd "C-c w") #'xwidget-webkit-browse-url)
  (define-key org-mode-map (kbd "C-c C-v") #'my-org-preview-xwidget)

  ;; HTML mode keybinding
  (add-hook 'html-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-v") #'my-html-preview-xwidget)))

  ;; Disable line numbers in xwidget buffers
  (add-hook 'xwidget-webkit-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))

(provide 'xwidget-config)
;;; xwidget-config.el ends here
