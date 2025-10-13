;;; sql.el --- SQL database client configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for SQL mode and database connections

;;; Code:

;; Configure SQL mode
(use-package sql
  :ensure nil  ; built-in package
  :bind (:map sql-interactive-mode-map
              ("M-<return>" . newline))  ; M-RET to insert newline without submitting
  :config
  ;; Set MySQL client program (dynamically find in PATH)
  (setq sql-mysql-program (or (executable-find "mysql")
                               "/opt/homebrew/opt/mysql/bin/mysql"))

  ;; Set PostgreSQL client program (dynamically find in PATH)
  (setq sql-postgres-program (or (executable-find "psql") "psql"))

  ;; MySQL connection options
  (setq sql-mysql-options '("--protocol=tcp"))

  ;; PostgreSQL connection options
  (setq sql-postgres-options '("--no-psqlrc"))

  ;; Load private connection configurations
  (let ((connections-file (expand-file-name "sql-connections.el" user-emacs-directory)))
    (when (file-exists-p connections-file)
      (load connections-file))))

(provide 'sql-config)
;;; sql.el ends here
