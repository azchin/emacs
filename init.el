(cd "~")
(defun emacsd (file)
  (concat user-emacs-directory file))
(defun eload (file)
  (load (emacsd file)))

;; Supress warnings
(when (string-match "29\\.0\\.50" (version))
  (setq warning-minimum-level :error))

(require 'server)
(setq daemon-mode-snapshot (server-running-p))
(setq home-dir default-directory)
(put 'suspend-frame 'disabled t)

(eload "core/appearance.el")
(eload "core/backup.el")
(eload "core/buffer.el")

(eload "packages.el")

(eload "core/tabs.el")
(eload "core/parens.el")
(eload "core/modeline.el")
;; (eload "core/commands.el")
(eload "core/external.el")
(eload "core/autoinsert.el")
;; (eload "core/late.el") ;; late.el triggered by load-theme (appearance.el)
(eload "core/desktop.el")
(eload "core/gnus.el")

(unless daemon-mode-snapshot (server-start))
(setq tramp-default-method "ssh")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ebdbb2" "#cc241d" "#98971a" "#d79921" "#458588" "#b16286" "#689d6a" "#3c3836"])
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(package-selected-packages
   '(which-key company counsel swiper ivy-hydra ivy yasnippet dired-subtree rust-mode cmake-mode markdown-mode openwith gruvbox-theme evil-org evil-collection evil-quickscope evil-commentary evil-surround evil undo-fu-session undo-fu org-superstar auto-package-update use-package))
 '(tab-bar-select-tab-modifiers '(meta)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
