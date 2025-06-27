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
  
  ;; Disable confirmation prompts for PlantUML only
  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (string= lang "plantuml"))))
  
  ;; PlantUML mode hooks
  (add-hook 'plantuml-mode-hook
	    (lambda ()
	      (make-local-variable 'plantuml-indent-level)
	      (setq plantuml-indent-level 2))))

;; Enable markdown export backend
(require 'ox-md)

;; Org HTML preview
(use-package org-preview-html
  :config
  (setq org-preview-html-refresh-configuration 'save))

;; Org sidebar for navigation
(use-package org-sidebar
  :config
  ;; Automatically show sidebar on org-mode files
  (add-hook 'org-mode-hook #'org-sidebar-tree))

;; Enable imenu for header navigation
(add-hook 'org-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'org-imenu-get-tree)))

;; Org mode key bindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c p") #'org-preview-html-mode)
(define-key org-mode-map (kbd "C-c s") #'org-sidebar-tree)
(define-key org-mode-map (kbd "C-c i") #'imenu)
(define-key org-mode-map (kbd "C-c j") #'org-goto)

(provide 'org-mode)
;;; org-mode.el ends here