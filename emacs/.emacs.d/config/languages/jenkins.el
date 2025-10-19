;;; jenkins.el --- Jenkins development configuration -*- lexical-binding: t; -*-

;; Jenkins mode
(use-package jenkinsfile-mode
  :config
  ;; Automatically enable for files named "Jenkinsfile"
  (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . jenkinsfile-mode))
  ;; Also match Jenkinsfile with any extension (e.g., Jenkinsfile.groovy)
  (add-to-list 'auto-mode-alist '("Jenkinsfile\\..*\\'" . jenkinsfile-mode)))

(provide 'jenkins)
;;; jenkins.el ends here
