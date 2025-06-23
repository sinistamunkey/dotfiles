;;; org-mode.el --- Org mode and PlantUML configuration -*- lexical-binding: t; -*-

;; PlantUML mode and Org integration
(use-package plantuml-mode
  :config
  ;; Configure PlantUML paths
  (setq org-plantuml-jar-path "~/plantuml.jar")
  (setq plantuml-jar-path "~/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "png")
  
  ;; Configure Org mode babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  
  ;; Auto display inline images
  (setq org-startup-with-inline-images t)
  
  ;; PlantUML mode hooks
  (add-hook 'plantuml-mode-hook
	    (lambda ()
	      (make-local-variable 'plantuml-indent-level)
	      (setq plantuml-indent-level 2))))

;; Org mode key bindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'org-mode)
;;; org-mode.el ends here