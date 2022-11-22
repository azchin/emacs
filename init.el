(defun emacsd (file)
  (concat user-emacs-directory file))
(push (emacsd "core") load-path)

(require 'stagetwo)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ebdbb2" "#cc241d" "#98971a" "#d79921" "#458588" "#b16286" "#689d6a" "#3c3836"])
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(ede-project-directories '("/home/andrew/sandbox/ede"))
 '(package-selected-packages
   '(which-key company yasnippet evil-org evil-collection evil-quickscope evil-commentary evil-surround undo-fu-session undo-fu rust-mode cmake-mode markdown-mode hl-todo))
 '(tab-bar-select-tab-modifiers '(meta)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
