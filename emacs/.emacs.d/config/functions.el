;;; functions.el --- Custom helper functions -*- lexical-binding: t; -*-

(defun uuid ()
  "Generate and insert a UUID"
  (interactive)
  (shell-command "uuidgen" t)
  (let ((beg (point)))
    (forward-word 5)
    (downcase-region beg (point))))

;; Custom key bindings
(global-set-key (kbd "C-c i") 'yas-insert-snippet)

(provide 'functions)
;;; functions.el ends here