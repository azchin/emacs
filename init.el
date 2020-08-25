(defun emacsd (file)
  (concat user-emacs-directory file))
(defun cload (file)
  (load (emacsd file)))

(cload "core/appearance.el")
(cload "core/backup.el")
(cload "core/tabs.el")
(cload "core/buffer.el")

(cload "packages.el")

(cload "core/modeline.el")
(cload "core/external.el")
(cload "core/late.el")

(when (file-exists-p (emacsd "convention.el"))
  (cload "convention.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(markdown-mode ivy-hydra ivy magit auctex use-package smartparens pdf-tools monokai-pro-theme minimap latex-preview-pane haskell-mode gruvbox-theme evil-surround evil-snipe evil-quickscope evil-leader evil-commentary evil-collection doom-themes dired-subtree auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
