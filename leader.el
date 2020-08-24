(use-package evil-leader
 :after (evil evil-org tex minimap)
 :config 
 (global-evil-leader-mode)
 (evil-leader/set-leader "<SPC>")
 ;; global: (o c f d h b n w q t s a : !)
 ;; local: (l i)
 (evil-leader/set-key
  "o l" 'org-store-link
  "o a" 'org-agent
  "o c" 'org-capture
  "c t t" (lambda () (interactive) (tabify (window-start) (window-end)))
  "c t u" (lambda () (interactive) (untabify (window-start) (window-end)))
  "f f" 'find-file
  "f b" 'switch-to-buffer
  "f s" (lambda () (interactive) (switch-to-buffer "*scratch*"))
  "f e" 'eshell
  "f c a" (lambda () (interactive) (find-file "~/.emacs.d/core/appearance.el"))
  "f c b" (lambda () (interactive) (find-file "~/.emacs.d/core/buffer.el"))
  "f c p" (lambda () (interactive) (find-file "~/.emacs.d/packages.el"))
  "f c i" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "f c k" (lambda () (interactive) (find-file "~/.emacs.d/leader.el"))
  "f c l" (lambda () (interactive) (find-file "~/.emacs.d/core/late.el"))
  "f c t" (lambda () (interactive) (find-file "~/.emacs.d/core/tabs.el"))
  "d d" 'dired-jump
  ;; dired-jump opens new window, dired uses current window
  "d h" (lambda () (interactive) (dired "~/"))
  "d c" (lambda () (interactive) (dired "~/.config/"))
  "d e" (lambda () (interactive) (dired "~/.emacs.d/"))
  "d r" (lambda () (interactive) (dired "~/drive/"))
  "d p" (lambda () (interactive) (dired "~/projects"))
  "d l" (lambda () (interactive) (dired "~/clone"))
  "d o" (lambda () (interactive) (dired "~/org"))
  "d g" 'dired
  ;; "d z h" (lambda () (interactive)
  ;;          (progn (setq dired-listing-switches
  ;;                  "-hlo --group-directories-first --time-style=iso")
  ;;                 (revert-buffer)))
  ;; https://stackoverflow.com/questions/22971299/conditionally-set-dired-listing-switches-locally-but-nil-remotely
  "h k" 'describe-key
  "h v" 'describe-variable
  "h f" 'describe-function
  "h m" 'describe-mode
  "h b" 'describe-bindings
  "h a" 'apropos-command
  "b b" 'list-buffers
  "b c" 'clean-buffer-list
  "b n" 'next-buffer
  "b p" 'previous-buffer
  "b g" 'switch-to-buffer
  "b s" 'write-file
  "n n" 'make-frame-command
  "n d" 'create-dired-frame
  "n D" (lambda () (interactive) (create-dired-frame "~"))
  "n o" (lambda () (interactive) (create-scratch-frame 'org-mode "*org-scratch*"))
  "n c" (lambda () (interactive) (create-scratch-frame 'c++-mode "*cpp-scratch*"))
  "n p" (lambda () (interactive) (create-scratch-frame 'python-mode "*python-scratch*"))
  "n t" (lambda () (interactive) (create-scratch-frame 'text-mode "*text-scratch*"))
  "n l" (lambda () (interactive)
          (create-scratch-frame 'lisp-interaction-mode "*lisp-scratch*"))
  "s" 'evil-write
  "a" 'evil-quit
  ":" 'eval-expression
  "!" 'shell-command
  "t m" 'minimap-mode
  "t u" 'undo-tree-visualize
  "w q" 'evil-quit
  "w s" 'evil-window-split
  "w v" 'evil-window-vsplit
  "w h" 'evil-window-left
  "w j" 'evil-window-down
  "w k" 'evil-window-up
  "w l" 'evil-window-right
  "w w" 'evil-window-next
  "q k" (lambda () (interactive) (kill-buffer-mod (current-buffer)))
  "q g" (lambda () (interactive) (kill-buffer-greedy (current-buffer)))
  "q w" 'delete-other-windows
  "q f" 'delete-other-frames
  "q q" 'delete-frame
  "q h" 'kill-regex-buffer-frame
  "q a" (lambda () (interactive) (mapc 'delete-frame (frame-list)))
  "q e" 'server-shutdown
  )
 (evil-leader/set-key-for-mode 'latex-mode
   "l l" (lambda () (interactive)
           (progn (latex-preview-pane-mode 'toggle)
                  (latex-preview-pane-update)))
   )
 (evil-leader/set-key-for-mode 'org-mode
   "i" 'org-insert-structure-template
   )
 )

;; (global-unset-key (kbd "C-SPC")) ;; set-mark-command
(evil-define-key 'normal dired-mode-map "f" 'find-file)
