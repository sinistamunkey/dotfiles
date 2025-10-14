;;; sql-config.el --- SQL database client configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for SQL mode and database connections

;;; Code:

;; Require the built-in sql package
(require 'sql)

;; Set MySQL client program (dynamically find in PATH)
(setq sql-mysql-program (or (executable-find "mysql")
                             "/opt/homebrew/opt/mysql/bin/mysql"))

;; Set PostgreSQL client program (dynamically find in PATH)
(setq sql-postgres-program (or (executable-find "psql") "psql"))

;; MySQL connection options
(setq sql-mysql-options '("--protocol=tcp"))

;; PostgreSQL connection options
(setq sql-postgres-options '("--no-psqlrc"))

;; M-RET to insert newline without submitting in SQL interactive mode
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<return>") 'newline)))

;; Load private connection configurations
(let ((connections-file (expand-file-name "sql-connections.el" user-emacs-directory)))
  (when (file-exists-p connections-file)
    (load connections-file)))

(provide 'sql-config)
;;; sql-config.el ends here
