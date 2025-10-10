;;; functions.el --- Custom helper functions -*- lexical-binding: t; -*-

(defun uuid ()
  "Generate and insert a UUID"
  (interactive)
  (shell-command "uuidgen" t)
  (let ((beg (point)))
    (forward-word 5)
    (downcase-region beg (point))))

(defun my-copy-buffer-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new buffer-file-name)
        (message "Copied: %s" buffer-file-name))
    (message "Buffer is not visiting a file")))

(defun my-copy-buffer-file-name ()
  "Copy the current buffer's file name (without path) to the kill ring."
  (interactive)
  (if buffer-file-name
      (let ((filename (file-name-nondirectory buffer-file-name)))
        (kill-new filename)
        (message "Copied: %s" filename))
    (message "Buffer is not visiting a file")))

(defun my-get-github-url ()
  "Get the GitHub URL for the current file on the current branch."
  (if (not buffer-file-name)
      nil
    (let* ((git-root (vc-git-root buffer-file-name))
           (relative-path (file-relative-name buffer-file-name git-root))
           (branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
           (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
           (github-url (cond
                        ;; Convert SSH URL to HTTPS
                        ((string-match "git@github\\.com:\\(.+\\)\\.git" remote-url)
                         (concat "https://github.com/" (match-string 1 remote-url)))
                        ;; Convert HTTPS URL
                        ((string-match "https://github\\.com/\\(.+\\)\\.git" remote-url)
                         (concat "https://github.com/" (match-string 1 remote-url)))
                        ;; Already clean HTTPS URL
                        ((string-match "https://github\\.com/\\(.+\\)" remote-url)
                         (match-string 0 remote-url))
                        (t nil))))
      (when github-url
        (format "%s/blob/%s/%s" github-url branch relative-path)))))

(defun my-browse-file-on-github ()
  "Open the current file in GitHub for the current branch."
  (interactive)
  (if (not buffer-file-name)
      (message "Buffer is not visiting a file")
    (let ((url (my-get-github-url)))
      (if url
          (progn
            (browse-url url)
            (message "Opening: %s" url))
        (message "Could not determine GitHub URL from remote")))))

(defun my-copy-github-url ()
  "Copy the GitHub URL for the current file to the clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Buffer is not visiting a file")
    (let ((url (my-get-github-url)))
      (if url
          (progn
            (kill-new url)
            (message "Copied: %s" url))
        (message "Could not determine GitHub URL from remote")))))

(defun my-insert-directory-tree (directory &optional depth)
  "Insert a file tree for DIRECTORY at point using the tree command.
Optional DEPTH limits the tree depth (default: 2 levels).
With prefix argument, prompts for depth."
  (interactive "DDirectory: \nP")
  (let* ((depth (if (numberp depth) depth 2))
         (tree-output (shell-command-to-string
                       (format "tree -L %d --charset ascii %s"
                               depth
                               (shell-quote-argument (expand-file-name directory))))))
    (insert tree-output)))

(defvar my-scratch-dir (expand-file-name "scratch/" user-emacs-directory)
  "Directory for persistent scratch files.")

(defun my-new-scratch (name)
  "Create a new scratch file in the scratch directory.
If NAME is not provided, prompts for a name.
The file extension determines the major mode (e.g., .json, .py, .txt)."
  (interactive "sScratch name: ")
  (unless (file-exists-p my-scratch-dir)
    (make-directory my-scratch-dir t))
  (let ((filepath (expand-file-name name my-scratch-dir)))
    (find-file filepath)))

(defun my-list-scratches ()
  "List all scratch files and open the selected one."
  (interactive)
  (unless (file-exists-p my-scratch-dir)
    (make-directory my-scratch-dir t))
  (let* ((files (directory-files my-scratch-dir nil "^[^.]"))
         (choice (if files
                     (completing-read "Open scratch: " files nil t)
                   (read-string "No scratches found. Create new scratch: "))))
    (if (member choice files)
        (find-file (expand-file-name choice my-scratch-dir))
      (my-new-scratch choice))))

;; Custom key bindings
(global-set-key (kbd "C-c i") 'yas-insert-snippet)
(global-set-key (kbd "C-c f p") 'my-copy-buffer-file-path)
(global-set-key (kbd "C-c f n") 'my-copy-buffer-file-name)
(global-set-key (kbd "C-c g b") 'my-browse-file-on-github)
(global-set-key (kbd "C-c g c") 'my-copy-github-url)
(global-set-key (kbd "C-c d t") 'my-insert-directory-tree)
(global-set-key (kbd "C-c s n") 'my-new-scratch)
(global-set-key (kbd "C-c s l") 'my-list-scratches)

(provide 'functions)
;;; functions.el ends here
