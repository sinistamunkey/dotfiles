;;; org-mode.el --- Org mode and PlantUML configuration -*- lexical-binding: t; -*-

;; Install peg (required by org-ql)
(use-package peg
  :demand t)  ; Load immediately to satisfy org-ql dependency

;; Install org-ql and org-sidebar (require peg to be loaded first)
(use-package org-ql
  :after peg)

(use-package org-sidebar
  :after org-ql)

;; Auto-insert mode for org files
(use-package autoinsert
  :config
  (auto-insert-mode 1)
  (setq auto-insert-query nil)  ; Don't ask before inserting

  ;; Define template for org-mode files
  (define-auto-insert 'org-mode
    '(nil
      "#+TITLE: " _ "\n"
      "#+AUTHOR: Gary Prescott\n\n"
      (insert-file-contents (expand-file-name "org-templates/export-setup.org" user-emacs-directory))
      "\n")))

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

;; Disable syntax highlighting in HTML export to avoid character encoding issues
(setq org-html-htmlize-output-type nil)

;; Org HTML preview
(use-package org-preview-html
  :config
  (setq org-preview-html-refresh-configuration 'save)
  ;; Use xwidget as the preview viewer
  (setq org-preview-html-viewer 'xwidget))

;; Custom function to preview org in external browser
(defun my-org-preview-in-browser ()
  "Export current org file to HTML and open in external browser."
  (interactive)
  (let ((html-file (org-html-export-to-html)))
    (browse-url-default-browser (concat "file://" html-file))))

;; Enable imenu for header navigation
(add-hook 'org-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'org-imenu-get-tree)
            (visual-line-mode 1)
            (flyspell-mode 1)))

;; Org mode key bindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c l") #'org-store-link)
  (define-key org-mode-map (kbd "C-c a") #'org-agenda)
  (define-key org-mode-map (kbd "C-c c") #'org-capture)
  (define-key org-mode-map (kbd "C-c p") #'org-preview-html-mode)
  (define-key org-mode-map (kbd "C-c i") #'imenu)
  (define-key org-mode-map (kbd "C-c j") #'org-goto))

(provide 'org-mode)
;;; org-mode.el ends here
