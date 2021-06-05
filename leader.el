(use-package evil-leader
  :after (evil)
  :config 
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  ;; global: (o m c f d h b n w q t s a ; 1)
  ;; local: (m i)
  ;; convention: if global prefx exists then use same prefix for local; else use 'm' as prefix
  (evil-leader/set-key
    "o l" 'org-store-link
    ;; "o a" 'org-agent
    "o c" 'org-capture
    "o a" (lambda () (interactive) (create-new-frame-command 'org-agenda-list) (delete-other-windows))
    "c t" 'toggle-tabs
    "c y" 'conditional-tabify
    "c i" 'indent-whole-buffer
    ;; "c t u" (lambda () (interactive) (untabify (window-start) (window-end)))
    "c e" (lambda () (interactive)
            (unless visual-line-mode (evil-visual-line))
            (call-interactively 'eval-last-sexp)
            (evil-exit-visual-state))
    "c r" 'rename-uniquely
    "c f" 'text-scale-adjust
    "o l" 'org-store-link
    ;; "o a" 'org-agent
    "o c" 'org-capture
    "o a" (lambda () (interactive) (create-new-frame-command 'org-agenda-list) (delete-other-windows))
    "m i" 'magit-init
    "m m" 'magit-status
    "f f" 'find-file
    "f h" (lambda () (interactive) (if (equal major-mode 'dired-mode)
                                  (counsel-find-file)
                                (counsel-find-file (concat home-dir "^"))))
    "f c a" (lambda () (interactive) (find-file (emacsd "core/appearance.el")))
    "f c b" (lambda () (interactive) (find-file (emacsd "core/buffer.el")))
    "f c p" (lambda () (interactive) (find-file (emacsd "packages.el")))
    "f c i" (lambda () (interactive) (find-file (emacsd "init.el")))
    "f c k" (lambda () (interactive) (find-file (emacsd "leader.el")))
    "f c l" (lambda () (interactive) (find-file (emacsd "core/late.el")))
    "f c m" (lambda () (interactive) (find-file (emacsd "core/modeline.el")))
    "f c t" (lambda () (interactive) (find-file (emacsd "core/tabs.el")))
    "f c o" (lambda () (interactive) (find-file (emacsd "core/org.el")))
    "f o a" (lambda () (interactive) (find-file (concat org-directory "agenda.org")))
    "f o i" (lambda () (interactive) (find-file (concat org-directory "index.org")))
    "f s o" (lambda () (interactive) (create-new-frame "*org-scratch*" 'org-mode))
    "f s c" (lambda () (interactive) (create-new-frame "*cpp-scratch*" 'c++-mode))
    "f s p" (lambda () (interactive) (create-new-frame "*python-scratch*" 'python-mode))
    "f s t" (lambda () (interactive) (create-new-frame "*text-scratch*" 'text-mode))
    "f s h" (lambda () (interactive) (create-new-frame "*script-scratch*" 'shell-script-mode))
    "f s l" (lambda () (interactive) (create-new-frame "*lisp-scratch*" 'lisp-interaction-mode))
    "f s s" (lambda () (interactive) (switch-to-buffer "*scratch*"))
    "f u" (lambda () (interactive) (find-file (concat "/sudo::" (read-file-name "File (sudo): " "/"))))
    "t t" 'tab-new
    "t b" 'switch-to-buffer-other-tab
    "t f" 'find-file-other-tab
    "t d" (lambda () (interactive) (dired-other-tab default-directory))
    "t q" 'tab-close
    "t o" 'tab-close-other
    "t m" 'tab-move
    "d d" 'dired-jump
    ;; dired-jump opens new window, dired uses current window
    "d h" (lambda () (interactive) (dired home-dir))
    "d c" (lambda () (interactive) (dired (concat home-dir ".config")))
    "d e" (lambda () (interactive) (dired (emacsd "core")))
    "d r" (lambda () (interactive) (dired (concat home-dir "drive")))
    "d p" (lambda () (interactive) (dired (concat home-dir "projects")))
    "d l" (lambda () (interactive) (dired (concat home-dir "clone")))
    "d o" (lambda () (interactive) (dired (concat home-dir "org")))
    "d s" (lambda () (interactive) (dired (concat "/ssh:" (read-string "SSH: ") ":")))
    "d u" (lambda () (interactive) (dired "/sudo::/"))
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
    "n h" (lambda () (interactive) (create-dired-frame home-dir))
    "n e" (lambda () (interactive) (let ((default-directory home-dir)) (eshell)))
    "n s" (lambda () (interactive) (create-dired-frame (concat "/ssh:" (read-string "SSH: ") ":")))
    "n u" (lambda () (interactive) (create-dired-frame (concat "/sudo::" (read-directory-name "Dir (sudo): " "/"))))
    ;; "s" (lambda () (interactive) (indent-whole-buffer) (save-buffer))
    "s" 'save-buffer
    ";" 'eval-expression
    "/" 'evil-ex-nohighlight
    "1" 'shell-command
    "w q" 'delete-window
    "w s" 'evil-window-split
    "w v" 'evil-window-vsplit
    "w h" 'evil-window-left
    "w j" 'evil-window-down
    "w k" 'evil-window-up
    "w l" 'evil-window-right
    "w w" 'evil-window-next
    "w o" 'delete-other-windows
    "q k" (lambda () (interactive) (kill-buffer-mod (current-buffer)))
    "q g" (lambda () (interactive) (kill-buffer-greedy (current-buffer)))
    "q o" 'delete-other-frames
    "q f" (lambda () (interactive) (cond ((> (length (visible-frame-list)) 1) (delete-frame))
                                    (t (save-buffers-kill-terminal))))
    "q q" (lambda () (interactive) (cond ((> (length (tab-bar-tabs)) 1) (tab-close))
                                    ((> (length (visible-frame-list)) 1) (delete-frame))
                                    (t (save-buffers-kill-terminal))))
    "q h" 'kill-regex-buffer-frame
    "q a" (lambda () (interactive) (if server-mode (mapc 'delete-frame (frame-list))
                                (save-buffers-kill-terminal)))
    "q e" 'server-shutdown
    )
  (evil-leader/set-key-for-mode 'dired-mode
    "s" 'dired-jump
    )
  ;; (evil-leader/set-key-for-mode 'latex-mode
  ;;   "m c" (lambda () (interactive)
  ;;         (save-buffer) (TeX-command-run-all nil))
  ;;   ;; "l l" (lambda () (interactive)
  ;;   ;;         (progn (latex-preview-pane-mode 'toggle)
  ;;   ;;                (latex-preview-pane-update)))
  ;;   )
  (evil-leader/set-key-for-mode 'org-mode
    "o i" 'org-insert-structure-template
    "o s" 'org-schedule
    "o d" 'org-deadline
    "o e l" 'org-latex-export-to-latex
    "o e p" 'org-latex-export-to-pdf
    )
  )

(evil-define-key 'normal dired-mode-map "f" 'find-file)

;; Company start
(defun company-backspace ()
  (interactive)
  (if (equal company-selection-changed nil)
      (if tab-control-auto (backward-delete-char-untabify 1)
        (backspace-whitespace-to-tab-stop))
    (company-abort)))

(defun company-select-next-or-complete-selection (&optional arg)
  "Insert selection if appropriate, or select the next candidate."
  (interactive)
  (if (not (company-tooltip-visible-p)) (company-manual-begin))
  (cond ((> company-candidates-length 1) (company-select-next arg))
        ((equal company-candidates-length 1) (company-finish (car company-candidates)))))

(defun company-select-previous-or-complete-selection ()
  "Insert selection if appropriate, or select the previous candidate."
  (interactive)
  (company-select-next-or-complete-selection -1))

(define-key company-active-map (kbd "<backspace>") 'company-backspace)
(define-key company-active-map (kbd "C-h") nil)

(evil-define-key 'insert company-mode-map (kbd "C-n")
  'company-select-next-or-complete-selection)
(evil-define-key 'insert company-mode-map (kbd "C-p")
  'company-select-previous-or-complete-selection)
(evil-define-key 'insert company-active-map (kbd "C-n")
  'company-select-next)
(evil-define-key 'insert company-active-map (kbd "C-p")
  'company-select-previous)
(evil-define-key 'insert company-active-map (kbd "ESC")
  (lambda () (interactive) (company-abort)(evil-normal-state)))
;; Company end

; (evil-define-key 'normal org-mode-map (kbd "H") 'org-shiftleft)
; (evil-define-key 'normal org-mode-map (kbd "L") 'org-shiftright)
; (evil-define-key 'insert org-mode-map (kbd "RET") (lambda () (interactive) (org-return nil)))

;; (evil-define-key 'normal pdf-view-mode-map (kbd "J") 'pdf-view-next-page)
;; (evil-define-key 'normal pdf-view-mode-map (kbd "K") 'pdf-view-previous-page)

;; (evil-define-key 'normal 'global "G"
;;   (lambda () (interactive) (evil-goto-line) (forward-line -1)))

;; (define-key global-map (kbd "C-SPC") nil)
;; (define-key global-map (kbd "C-S-SPC") nil)

(evil-global-set-key 'normal (kbd "C-_") nil)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)
(global-set-key (kbd "C-)") (lambda () (interactive) (text-scale-set 0)))
