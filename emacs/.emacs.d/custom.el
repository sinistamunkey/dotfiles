;;; custom.el --- Emacs custom-set-variables and faces -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-mode t)
 '(custom-safe-themes
   '("3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26"
     "79586dc4eb374231af28bbc36ba0880ed8e270249b07f814b0e6555bdcb71fab"
     default))
 '(display-time-mode t)
 '(package-selected-packages nil)
 '(python-shell-interpreter "python")
 '(tool-bar-mode nil)
 '(warning-suppress-types
   '((comp) (bytecomp) (tar)
     ((python python-shell-completion-native-turn-on-maybe)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background unspecified :foreground unspecified)))))

(provide 'custom)
;;; custom.el ends here
