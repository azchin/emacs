(use-package evil-leader
 :requires evil minimap tex
 :config 
 (global-evil-leader-mode)
 (evil-leader/set-leader "<SPC>")
 (evil-leader/set-key
  "m l" 'org-store-link
  "m a" 'org-agent
  "m c" 'org-capture
  "f f" 'find-file
  "f b" 'switch-to-buffer
  "f c a" (lambda () (interactive) (find-file "~/.emacs.d/core/appearance.el"))
  "f c p" (lambda () (interactive) (find-file "~/.emacs.d/packages.el"))
  "f c i" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "f c k" (lambda () (interactive) (find-file "~/.emacs.d/evil/leader.el"))
  "f c l" (lambda () (interactive) (find-file "~/.emacs.d/core/late.el"))
  "d d" 'dired-jump
  ;; dired-jump opens new window, dired uses current window
  "d h" (lambda () (interactive) (dired "~/"))
  "d c" (lambda () (interactive) (dired "~/.config/"))
  "d e" (lambda () (interactive) (dired "~/.emacs.d/"))
  "d r" (lambda () (interactive) (dired "~/drive/"))
  "d g" 'dired
  ;; "d z h" (lambda () (interactive)(setq ))
  "b b" 'list-buffers
  "b c" 'clean-buffer-list
  "b n" 'next-buffer
  "b p" 'previous-buffer
  "s" 'evil-write
  "f s" (lambda () (interactive)(switch-to-buffer "*scratch*"))
  "f e" 'eshell
  "l l" 'latex-preview-pane-mode
  "t m" 'minimap-mode
  "t u" 'undo-tree-visualize
  ;; "u" 'universal-argument
  "w q" 'evil-quit
  "w s" 'evil-window-split
  "w v" 'evil-window-vsplit
  "w h" 'evil-window-left
  "w j" 'evil-window-down
  "w k" 'evil-window-up
  "w l" 'evil-window-right
  "w w" 'evil-window-next
  "q q" 'evil-quit
  "q b" (lambda () (interactive) (kill-buffer-and-its-windows (current-buffer)))
  "q k" (lambda () (interactive) (kill-buffer-mod (current-buffer)))
  "q f" 'delete-other-windows
  "q a" 'evil-quit-all
  "q e" 'server-shutdown))
