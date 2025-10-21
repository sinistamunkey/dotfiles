;;; text-editing.el --- Text editing and spell-checking configuration -*- lexical-binding: t; -*-

;; ===================================
;; Spell Checking Configuration
;; ===================================

;; Use aspell as the spell-checking program
(setq ispell-program-name "aspell")

;; Set British English (ise variant) as the default dictionary
(setq ispell-dictionary "en_GB-ise")

;; Configure aspell for better performance and British English
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB"))

;; Enable flyspell for text modes
(use-package flyspell
  :ensure nil  ; Built-in package
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode))  ; Check comments/strings in code
  :config
  (setq flyspell-issue-message-flag nil)  ; Reduce messages
  (setq flyspell-issue-welcome-flag nil))

(provide 'text-editing)
;;; text-editing.el ends here
