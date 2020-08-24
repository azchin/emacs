(load "~/.emacs.d/core/appearance.el")
(load "~/.emacs.d/core/backup.el")
(load "~/.emacs.d/core/tabs.el")
(load "~/.emacs.d/core/buffer.el")

(load "~/.emacs.d/packages.el")

(load "~/.emacs.d/core/modeline.el")
(load "~/.emacs.d/core/external.el")
(load "~/.emacs.d/core/late.el")

(when (file-exists-p "~/.emacs.d/convention.el")
  (load "~/.emacs.d/convention.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(auctex use-package smartparens pdf-tools monokai-pro-theme minimap latex-preview-pane haskell-mode gruvbox-theme evil-surround evil-snipe evil-quickscope evil-leader evil-commentary evil-collection doom-themes dired-subtree auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
