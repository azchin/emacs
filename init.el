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

(eload "core/appearance.el")
(eload "core/backup.el")
(eload "core/buffer.el")

(eload "packages.el")

(eload "core/tabs.el")
(eload "core/modeline.el")
;; (eload "core/commands.el")
(eload "core/external.el")
(eload "core/autoinsert.el")
;; (eload "core/late.el") ;; late.el triggered by load-theme (appearance.el)
(eload "core/desktop.el")

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
   '(modus-themes yasnippet pandoc-mode modus-vivendi-theme markdown-mode ivy-hydra ivy magit auctex use-package smartparens pdf-tools monokai-pro-theme minimap latex-preview-pane haskell-mode gruvbox-theme evil-surround evil-snipe evil-quickscope evil-leader evil-commentary evil-collection doom-themes dired-subtree auto-package-update))
 '(pdf-view-midnight-colors '("#282828" . "#f9f5d7"))
 '(tab-bar-select-tab-modifiers '(meta)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
