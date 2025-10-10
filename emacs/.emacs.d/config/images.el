;;; images.el --- Image display configuration for GUI Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for displaying images in GUI Emacs

;;; Code:

;; Enable automatic image file display
(when (and (fboundp 'image-type-available-p)
           (or (image-type-available-p 'png)
               (image-type-available-p 'jpeg)
               (image-type-available-p 'gif)))
  (auto-image-file-mode 1))

;; Image mode settings
(setq image-animate-loop t)

;; Enhanced image dired support
(when (fboundp 'image-dired)
  (setq image-dired-thumb-size 150
        image-dired-thumb-margin 2
        image-dired-thumb-relief 0
        image-dired-thumbs-per-row 4))

;; Enhanced org-mode image display
(with-eval-after-load 'org
  ;; Better image scaling in org mode
  (setq org-image-actual-width '(420))

  ;; Function to refresh images in org buffer
  (defun org-refresh-images ()
    "Refresh all images in current org buffer."
    (interactive)
    (org-remove-inline-images)
    (org-display-inline-images)))

(provide 'images)
;;; images.el ends here
