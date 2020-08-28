(use-package evil-leader
  :after (evil evil-org tex minimap magit)
  :config 
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  ;; global: (o m c f d h b n w q t s a ; 1)
  ;; local: (l i)
  (evil-leader/set-key
    "o l" 'org-store-link
    "o a" 'org-agent
    "o c" 'org-capture
    "m i" 'magit-init
    "m m" 'magit-status
    "c t" 'toggle-tabs
    "c y" 'conditional-tabify
    ;; "c t u" (lambda () (interactive) (untabify (window-start) (window-end)))
    "c e" 'eval-last-sexp
    "c r" 'rename-uniquely
    "f f" 'find-file
    "f h" (lambda () (interactive) (if (equal major-mode 'dired-mode)
                                  (counsel-find-file)
                                (counsel-find-file (concat home-dir "^"))))
    "f s" (lambda () (interactive) (switch-to-buffer "*scratch*"))
    "f e" (lambda () (interactive) (let ((default-directory home-dir)) (eshell)))
    "f c a" (lambda () (interactive) (find-file (emacsd "core/appearance.el")))
    "f c b" (lambda () (interactive) (find-file (emacsd "core/buffer.el")))
    "f c p" (lambda () (interactive) (find-file (emacsd "packages.el")))
    "f c i" (lambda () (interactive) (find-file (emacsd "init.el")))
    "f c k" (lambda () (interactive) (find-file (emacsd "leader.el")))
    "f c l" (lambda () (interactive) (find-file (emacsd "core/late.el")))
    "f c t" (lambda () (interactive) (find-file (emacsd "core/tabs.el")))
    "d d" 'dired-jump
    ;; dired-jump opens new window, dired uses current window
    "d h" (lambda () (interactive) (dired home-dir))
    "d c" (lambda () (interactive) (dired (concat home-dir ".config")))
    "d e" (lambda () (interactive) (dired (emacsd "core")))
    "d r" (lambda () (interactive) (dired (concat home-dir "drive")))
    "d p" (lambda () (interactive) (dired (concat home-dir "projects")))
    "d l" (lambda () (interactive) (dired (concat home-dir "clone")))
    "d o" (lambda () (interactive) (dired (concat home-dir "org")))
    "d g" 'dired
    ;; "d z h" (lambda () (interactive)
    ;;          (progn (setq dired-listing-switches
    ;;                  "-hlo --group-directories-first --time-style=iso")
    ;;                 (revert-buffer)))
    ;; https://stackoverflow.com/questions/22971299/conditionally-set-dired-listing-switches-locally-but-nil-remotely
    ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Summary.html#Help-Summary
    "h k" 'describe-key
    "h v" 'describe-variable
    "h f" 'describe-function
    "h m" 'describe-mode
    "h b" 'describe-bindings
    "h a" 'apropos-command
    "h o" 'describe-symbol
    "b b" 'switch-to-buffer
    "b c" 'clean-buffer-list
    "b n" 'next-buffer
    "b p" 'previous-buffer
    "b s" 'write-file
    "n n" 'make-frame-command
    "n b" 'list-buffers
    "n d" 'create-dired-frame
    "n D" (lambda () (interactive) (create-dired-frame home-dir))
    "n o" (lambda () (interactive) (create-scratch-frame 'org-mode "*org-scratch*"))
    "n c" (lambda () (interactive) (create-scratch-frame 'c++-mode "*cpp-scratch*"))
    "n p" (lambda () (interactive) (create-scratch-frame 'python-mode "*python-scratch*"))
    "n t" (lambda () (interactive) (create-scratch-frame 'text-mode "*text-scratch*"))
    "n s" (lambda () (interactive)
            (create-scratch-frame 'shell-script-mode "*script-scratch*"))
    "n l" (lambda () (interactive)
            (create-scratch-frame 'lisp-interaction-mode "*lisp-scratch*"))
    "s" 'evil-write
    "a" 'evil-quit
    ";" 'eval-expression
    "1" 'shell-command
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
    "q a" (lambda () (interactive) (if server-mode (mapc 'delete-frame (frame-list))
                                (save-buffers-kill-terminal)))
    "q e" 'server-shutdown
    )
  (evil-leader/set-key-for-mode 'latex-mode
    "s" (lambda () (interactive)
          (save-buffer) (TeX-command-run-all nil))
    ;; "l l" (lambda () (interactive)
    ;;         (progn (latex-preview-pane-mode 'toggle)
    ;;                (latex-preview-pane-update)))
    )
  (evil-leader/set-key-for-mode 'org-mode
    "i" 'org-insert-structure-template
    )
  )

;; (global-unset-key (kbd "C-SPC")) ;; set-mark-command
(evil-define-key 'normal dired-mode-map "f" 'find-file)
(defun company-backspace ()
  (interactive)
  (if (and (equal company-selection 0) (equal company-selection-changed nil))
      (if tab-control-auto (backward-delete-char-untabify 1)
        (backspace-whitespace-to-tab-stop))
    (company-abort)))

(define-key company-active-map (kbd "<backspace>") 'company-backspace)
(define-key company-active-map (kbd "C-h") nil)
(evil-define-key 'insert company-mode-map (kbd "C-n") 'company-manual-begin)
(evil-define-key 'insert company-mode-map (kbd "C-p")
  'company-select-previous)
(evil-define-key 'insert company-active-map (kbd "C-n")
  'company-select-next-if-tooltip-visible-or-complete-selection)
(evil-define-key 'insert company-active-map (kbd "C-p")
  'company-select-previous-or-abort)

(evil-define-key 'normal org-mode-map (kbd "H") 'org-shiftleft)
(evil-define-key 'normal org-mode-map (kbd "L") 'org-shiftright)

(evil-define-key 'normal pdf-view-mode-map (kbd "J") 'pdf-view-next-page)
(evil-define-key 'normal pdf-view-mode-map (kbd "K") 'pdf-view-previous-page)
