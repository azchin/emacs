;; (evil-set-leader '(normal visual emacs) (kbd "SPC")) ;; or use '(normal visual)
;; (evil-define-key '(normal visual emacs) 'global (kbd "SPC") 'evil-send-leader) ;; or use '(normal visual)
;; (evil-global-set-key 'normal (kbd "<leader>ol") 'org-store-link)

(defvar leader-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(defvar leader-states '(normal visual emacs)
  "Evil states for the leader keybinding")

(define-minor-mode leader-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(leader-intercept-mode)

(dolist (state leader-states)
  (evil-make-intercept-map
   ;; NOTE: This requires an evil version from 2018-03-20 or later
   (evil-get-auxiliary-keymap leader-intercept-mode-map state t t)
   state))
(evil-define-key '(normal insert) 'global (kbd "C-S-v") 'evil-paste-before)
(evil-define-key 'visual 'global (kbd "C-S-c") 'evil-yank)
(evil-define-key 'insert 'global (kbd "C-S-c") 'copy-region-as-kill)
(evil-define-key leader-states leader-intercept-mode-map (kbd "SPC") 'evil-send-leader)
(evil-define-key leader-states 'global (kbd "[ d") 'hl-todo-previous)
(evil-define-key leader-states 'global (kbd "] d") 'hl-todo-next)
(evil-define-key leader-states 'global (kbd "[ c") 'flycheck-previous-error)
(evil-define-key leader-states 'global (kbd "] c") 'flycheck-next-error)
(evil-define-key leader-states 'global (kbd "g t") (lambda (x) (interactive "P") (if x (tab-bar-select-tab x) (tab-bar-switch-to-next-tab))))
(evil-define-key leader-states 'global (kbd "<leader> c t") 'toggle-tabs)
(evil-define-key leader-states 'global (kbd "<leader> c y") 'conditional-tabify)
(evil-define-key leader-states 'global (kbd "<leader> c i") 'indent-whole-buffer)
(evil-define-key leader-states 'global (kbd "<leader> c m") (lambda () (interactive) (menu-bar-mode 'toggle)))
;; "c t u" (lambda () (interactive) (untabify (window-start) (window-end)))
(evil-define-key leader-states 'global (kbd "<leader> c e") (lambda () (interactive)
                                                              (unless visual-line-mode (evil-visual-line))
                                                              (call-interactively 'eval-last-sexp)
                                                              (evil-exit-visual-state)))
(evil-define-key leader-states 'global (kbd "<leader> c r") 'rename-uniquely)
;; (evil-define-key leader-states 'global (kbd "<leader> c f") 'fill-whole-buffer)
(evil-define-key leader-states 'global (kbd "<leader> c f") 'fill-region)
(evil-define-key leader-states 'global (kbd "<leader> c w") 'count-words-region)
(evil-define-key leader-states 'global (kbd "<leader> e e") (lambda () (interactive) (if desktop-save-mode
                                                                                    (progn (desktop-save (emacsd "cache/default-desktop"))
                                                                                           (desktop-save-mode 0))
                                                                                  (progn (desktop-save-mode 1)
                                                                                         (desktop-read (emacsd "cache/default-desktop"))))))
(evil-define-key leader-states 'global (kbd "<leader> e s") (lambda () (interactive) (desktop-save (emacsd "cache/default-desktop")))) ;; TODO prompt user for new dir
(evil-define-key leader-states 'global (kbd "<leader> e r") (lambda () (interactive) (desktop-read (emacsd "cache/default-desktop"))))
;; (evil-define-key leader-states 'global (kbd "<leader> o l") 'org-store-link)
;; "o a" 'org-agent
(evil-define-key leader-states 'global (kbd "<leader> o c") 'org-capture)
(evil-define-key leader-states 'global (kbd "<leader> o a") (lambda () (interactive) (create-new-frame-command 'org-agenda-list) (delete-other-windows)))
(evil-define-key leader-states 'global (kbd "<leader> m i") 'magit-init)
(evil-define-key leader-states 'global (kbd "<leader> m c") 'magit-clone)
(evil-define-key leader-states 'global (kbd "<leader> m m") 'magit-status)
(evil-define-key leader-states 'global (kbd "<leader> m b") 'magit-blame)
(evil-define-key leader-states 'global (kbd "<leader> m <") 'smerge-keep-upper)
(evil-define-key leader-states 'global (kbd "<leader> m >") 'smerge-keep-lower)
(evil-define-key leader-states 'global (kbd "<leader> m |") 'smerge-keep-base)
(evil-define-key leader-states 'global (kbd "<leader> f f") 'find-file)
(evil-define-key leader-states 'global (kbd "<leader> f h") (lambda () (interactive) (find-file (read-file-name "Find file: " home-dir))))
(evil-define-key leader-states 'global (kbd "<leader> f c") (lambda () (interactive) (find-file (read-file-name "Find file: " "~/.config"))))
(evil-define-key leader-states 'global (kbd "<leader> f p") (lambda () (interactive) (find-file (read-file-name "Find file: " "~/projects"))))
(evil-define-key leader-states 'global (kbd "<leader> f r") (lambda () (interactive) (find-file (read-file-name "Find file: " "~/drive"))))
(evil-define-key leader-states 'global (kbd "<leader> f l") (lambda () (interactive) (find-file (read-file-name "Find file: " "~/clone"))))
(evil-define-key leader-states 'global (kbd "<leader> f o") (lambda () (interactive) (find-file (read-file-name "Find file: " "~/org"))))
(evil-define-key leader-states 'global (kbd "<leader> f d") (lambda () (interactive) (find-file (read-file-name "Find file: " "~/drive"))))
(evil-define-key leader-states 'global (kbd "<leader> f e e") (lambda () (interactive) (find-file (read-file-name "Find file: " (emacsd "")))))
;; (evil-define-key leader-states 'global (kbd "<leader> f h") (lambda () (interactive) (if (equal major-mode 'dired-mode)
;;                                                                                     (counsel-find-file)
;;                                                                                   (counsel-find-file (concat home-dir "^")))))
(evil-define-key leader-states 'global (kbd "<leader> f e a") (lambda () (interactive) (find-file (emacsd "core/appearance.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e b") (lambda () (interactive) (find-file (emacsd "core/buffer.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e p") (lambda () (interactive) (find-file (emacsd "packages.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e i") (lambda () (interactive) (find-file (emacsd "init.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e k") (lambda () (interactive) (find-file (emacsd "leader.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e l") (lambda () (interactive) (find-file (emacsd "core/late.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e m") (lambda () (interactive) (find-file (emacsd "core/modeline.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e t") (lambda () (interactive) (find-file (emacsd "core/tabs.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e o") (lambda () (interactive) (find-file (emacsd "core/org.el"))))
;; (evil-define-key leader-states 'global (kbd "<leader> f o a") (lambda () (interactive) (find-file (concat org-directory "agenda.org"))))
;; (evil-define-key leader-states 'global (kbd "<leader> f o i") (lambda () (interactive) (find-file (concat org-directory "index.org"))))
(evil-define-key leader-states 'global (kbd "<leader> f s o") (lambda () (interactive) (create-new-buffer "*org-scratch*" 'org-mode)))
(evil-define-key leader-states 'global (kbd "<leader> f s c") (lambda () (interactive) (create-new-buffer "*c-scratch*" 'c-mode)))
(evil-define-key leader-states 'global (kbd "<leader> f s r") (lambda () (interactive) (create-new-buffer "*rust-scratch*" 'rust-mode)))
(evil-define-key leader-states 'global (kbd "<leader> f s p") (lambda () (interactive) (create-new-buffer "*python-scratch*" 'python-mode)))
(evil-define-key leader-states 'global (kbd "<leader> f s t") (lambda () (interactive) (create-new-buffer "*text-scratch*" 'text-mode)))
(evil-define-key leader-states 'global (kbd "<leader> f s h") (lambda () (interactive) (create-new-buffer "*script-scratch*" 'shell-script-mode)))
(evil-define-key leader-states 'global (kbd "<leader> f s l") (lambda () (interactive) (create-new-buffer "*lisp-scratch*" 'lisp-interaction-mode)))
(evil-define-key leader-states 'global (kbd "<leader> f s s") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(evil-define-key leader-states 'global (kbd "<leader> f u") (lambda () (interactive) (find-file (concat "/sudo::" (read-file-name "File (sudo): " "/")))))
(evil-define-key leader-states 'global (kbd "<leader> t t") 'tab-new)
(evil-define-key leader-states 'global (kbd "<leader> t b") 'switch-to-buffer-other-tab)
(evil-define-key leader-states 'global (kbd "<leader> t f") 'find-file-other-tab)
(evil-define-key leader-states 'global (kbd "<leader> t d") (lambda () (interactive) (dired-other-tab default-directory)))
(evil-define-key leader-states 'global (kbd "<leader> t h") (lambda () (interactive) (let ((new-file (read-file-name "Choose file: " home-dir))) (tab-new) (find-file new-file))))
(evil-define-key leader-states 'global (kbd "<leader> t z") (lambda () (interactive) (switch-to-buffer-other-tab (current-buffer))))
(evil-define-key leader-states 'global (kbd "<leader> t q") 'tab-close)
(evil-define-key leader-states 'global (kbd "<leader> t o") 'tab-close-other)
;; (evil-define-key leader-states 'global (kbd "<leader> t m") 'tab-move)
;; (evil-define-key leader-states 'global (kbd "<leader> t M") (lambda () (interactive) (tab-move -1)))
(evil-define-key leader-states 'global (kbd "<leader> t m") (lambda () (interactive) (tab-bar-move-tab-to (read-number "Tab index: "))))
(evil-define-key leader-states 'global (kbd "<leader> t s") (lambda () (interactive) (tab-new) (switch-to-buffer "*scratch*")))
(evil-define-key leader-states 'global (kbd "<leader> d d") 'dired-jump)
;; dired-jump opens new window, dired uses current window
(evil-define-key leader-states 'global (kbd "<leader> d h") (lambda () (interactive) (dired home-dir)))
(evil-define-key leader-states 'global (kbd "<leader> d c") (lambda () (interactive) (dired (concat home-dir ".config"))))
(evil-define-key leader-states 'global (kbd "<leader> d e") (lambda () (interactive) (dired (emacsd "core"))))
(evil-define-key leader-states 'global (kbd "<leader> d r") (lambda () (interactive) (dired (concat home-dir "drive"))))
(evil-define-key leader-states 'global (kbd "<leader> d p") (lambda () (interactive) (dired (concat home-dir "projects"))))
(evil-define-key leader-states 'global (kbd "<leader> d l") (lambda () (interactive) (dired (concat home-dir "clone"))))
(evil-define-key leader-states 'global (kbd "<leader> d o") (lambda () (interactive) (dired (concat home-dir "org"))))
(evil-define-key leader-states 'global (kbd "<leader> d s") (lambda () (interactive) (dired (concat "/ssh:" (read-string "SSH: ") ":"))))
(evil-define-key leader-states 'global (kbd "<leader> d u") (lambda () (interactive) (dired "/sudo::/")))
(evil-define-key leader-states 'global (kbd "<leader> d t") 'treemacs)
;; (evil-define-key leader-states 'global (kbd "<leader> d g") 'dired)
;; "d z h" (lambda () (interactive)
;;          (progn (setq dired-listing-switches
;;                  "-hlo --group-directories-first --time-style=iso")
;;                 (revert-buffer)))
;; https://stackoverflow.com/questions/22971299/conditionally-set-dired-listing-switches-locally-but-nil-remotely
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Summary.html#Help-Summary
(evil-define-key leader-states 'global (kbd "<leader> h k") 'describe-key)
(evil-define-key leader-states 'global (kbd "<leader> h v") 'describe-variable)
(evil-define-key leader-states 'global (kbd "<leader> h f") 'describe-function)
(evil-define-key leader-states 'global (kbd "<leader> h m") 'describe-mode)
(evil-define-key leader-states 'global (kbd "<leader> h b") 'describe-bindings)
(evil-define-key leader-states 'global (kbd "<leader> h a") 'apropos-command)
(evil-define-key leader-states 'global (kbd "<leader> h o") 'describe-symbol)
(evil-define-key leader-states 'global (kbd "<leader> b b") 'switch-to-buffer)
(evil-define-key leader-states 'global (kbd "<leader> b c") (lambda () (interactive) (let ((clean-buffer-list-kill-never-regexps
                                                                                       (remove "^[A-Za-z].*[A-Za-z]$" clean-buffer-list-kill-never-regexps)))
                                                                                  (clean-buffer-list))))
(evil-define-key leader-states 'global (kbd "<leader> b n") 'next-buffer)
(evil-define-key leader-states 'global (kbd "<leader> b p") 'previous-buffer)
(evil-define-key leader-states 'global (kbd "<leader> b s") 'write-file)
(evil-define-key leader-states 'global (kbd "<leader> b r") 'revert-buffer-quick)
(evil-define-key leader-states 'global (kbd "<leader> n n") 'make-frame-command)
(evil-define-key leader-states 'global (kbd "<leader> n b") 'create-new-frame-buffer)
(evil-define-key leader-states 'global (kbd "<leader> n f") 'create-new-frame-file)
(evil-define-key leader-states 'global (kbd "<leader> n d") 'create-dired-frame)
(evil-define-key leader-states 'global (kbd "<leader> n s") 'create-new-frame-scratch)
(evil-define-key leader-states 'global (kbd "C-S-n") 'create-new-frame-scratch)
;; (evil-define-key leader-states 'global (kbd "<leader> n h") (lambda () (interactive) (create-dired-frame home-dir)))
;; "n e" (lambda () (interactive) (let ((default-directory home-dir)) (eshell)))
(evil-define-key leader-states 'global (kbd "<leader> n e") 'create-eshell-window)
;; (evil-define-key leader-states 'global (kbd "<leader> n s") (lambda () (interactive) (create-dired-frame (concat "/ssh:" (read-string "SSH: ") ":"))))
;; (evil-define-key leader-states 'global (kbd "<leader> n u") (lambda () (interactive) (create-dired-frame (concat "/sudo::" (read-directory-name "Dir (sudo): " "/")))))
;; "s" (lambda () (interactive) (indent-whole-buffer) (save-buffer))
(evil-define-key leader-states 'global (kbd "<leader> x d") 'open-in-external-explorer)
(evil-define-key leader-states 'global (kbd "<leader> s") 'save-buffer)
(evil-define-key leader-states 'global (kbd "<leader> ;") 'eval-expression)
(evil-define-key leader-states 'global (kbd "<leader> /") (lambda () (interactive) (evil-ex-nohighlight) (lazy-highlight-cleanup t)))
;; (evil-define-key leader-states 'global (kbd "<leader> /") 'lazy-highlight-cleanup)
(evil-define-key leader-states 'global (kbd "<leader> 1") 'shell-command)
(evil-define-key leader-states 'global (kbd "<leader> a") 'modus-themes-toggle)
(evil-define-key leader-states 'global (kbd "<leader> w q") 'delete-window)
(evil-define-key leader-states 'global (kbd "<leader> w s") 'evil-window-split)
(evil-define-key leader-states 'global (kbd "<leader> w v") 'evil-window-vsplit)
(evil-define-key leader-states 'global (kbd "<leader> w f s") (lambda () (interactive) (let ((new-file (read-file-name "Choose file: ")))
                                                                                    (evil-window-split) (find-file new-file))))
(evil-define-key leader-states 'global (kbd "<leader> w f v") (lambda () (interactive) (let ((new-file (read-file-name "Choose file: ")))
                                                                                    (evil-window-vsplit) (find-file new-file))))
(evil-define-key leader-states 'global (kbd "<leader> w b s") (lambda () (interactive) (let ((new-buffer (read-buffer "Choose buffer: ")))
                                                                                    (evil-window-split) (switch-to-buffer new-buffer))))
(evil-define-key leader-states 'global (kbd "<leader> w b v") (lambda () (interactive) (let ((new-buffer (read-buffer "Choose buffer: ")))
                                                                                    (evil-window-vsplit) (switch-to-buffer new-buffer))))
(evil-define-key leader-states 'global (kbd "<leader> w d s") (lambda () (interactive) (evil-window-split) (dired default-directory)))
(evil-define-key leader-states 'global (kbd "<leader> w d v") (lambda () (interactive) (evil-window-vsplit) (dired default-directory)))
(evil-define-key leader-states 'global (kbd "<leader> w v") 'evil-window-vsplit)
(evil-define-key leader-states 'global (kbd "<leader> w h") 'evil-window-left)
(evil-define-key leader-states 'global (kbd "<leader> w j") 'evil-window-down)
(evil-define-key leader-states 'global (kbd "<leader> w k") 'evil-window-up)
(evil-define-key leader-states 'global (kbd "<leader> w l") 'evil-window-right)
(evil-define-key leader-states 'global (kbd "<leader> w w") 'evil-window-next)
(evil-define-key leader-states 'global (kbd "<leader> w o") 'delete-other-windows)
(evil-define-key leader-states 'global (kbd "<leader> q b") (lambda () (interactive) (kill-buffer-and-its-windows (current-buffer))))
;; "q k" (lambda () (interactive) (kill-buffer-mod (current-buffer)))
;; "q g" (lambda () (interactive) (kill-buffer-greedy (current-buffer)))
(evil-define-key leader-states 'global (kbd "<leader> q o") 'delete-other-frames)
(evil-define-key leader-states 'global (kbd "<leader> q f") (lambda () (interactive) (cond ((> (length (visible-frame-list)) 1) (delete-frame))
                                                                                           (t (save-buffers-kill-terminal)))))
(evil-define-key leader-states 'global (kbd "<leader> q q") (lambda () (interactive) (cond ((> (length (window-list)) 1) (delete-window))
                                                                                      ((> (length (tab-bar-tabs)) 1) (tab-close))
                                                                                      ((> (length (visible-frame-list)) 1) (delete-frame))
                                                                                      (t (save-buffers-kill-terminal)))))
(evil-define-key leader-states 'global (kbd "<leader> q h") 'kill-regex-buffer-frame)
(evil-define-key leader-states 'global (kbd "<leader> q a") (lambda () (interactive) (if daemon-mode-snapshot (mapc 'delete-frame (frame-list))
                                                                                       (save-buffers-kill-terminal))))
(evil-define-key leader-states 'global (kbd "<leader> q e") 'server-shutdown)

;; (evil-leader/set-key-for-mode 'dired-mode
;;   "s" 'dired-jump
;;   )
;; (evil-leader/set-key-for-mode 'latex-mode
;;   "m c" (lambda () (interactive)
;;         (save-buffer) (TeX-command-run-all nil))
;;   ;; "l l" (lambda () (interactive)
;;   ;;         (progn (latex-preview-pane-mode 'toggle)
;;   ;;                (latex-preview-pane-update)))
;;   )
(evil-define-key leader-states 'org-mode-map (kbd "<leader> o i") 'org-insert-structure-template)
(evil-define-key leader-states 'org-mode-map (kbd "<leader> o s") 'org-schedule)
(evil-define-key leader-states 'org-mode-map (kbd "<leader> o d") 'org-deadline)
(evil-define-key leader-states 'org-mode-map (kbd "<leader> o l") 'org-insert-link)
(evil-define-key leader-states 'org-mode-map (kbd "<leader> o T") 'org-time-stamp)
(evil-define-key leader-states 'org-mode-map (kbd "<leader> o t") (lambda () (interactive) (insert " ") (org-insert-time-stamp (current-time) nil t) (delete-char 1)))
(evil-define-key leader-states 'org-mode-map (kbd "<leader> o e") 'org-export-dispatch)
(evil-define-key leader-states 'org-mode-map (kbd "<leader> o p") 'org-latex-export-to-pdf)
;; (evil-define-key leader-states 'org-mode-map (kbd "<leader> o e l") 'org-latex-export-to-latex)
;; (evil-define-key leader-states 'org-mode-map (kbd "<leader> o e p") 'org-latex-export-to-pdf)

;; (evil-set-leader '(normal visual replace emacs motion operator) (kbd "<SPC>"))
;; (evil-define-key 'normal 'global (kbd "<leader> o l") 'org-store-link)

(evil-define-key 'normal dired-mode-map "f" 'find-file)
(evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" 'dired-find-file)
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)
(setq mouse-wheel-progressive-speed nil)

(evil-define-key 'normal org-mode-map (kbd "H") 'org-shiftleft)
(evil-define-key 'normal org-mode-map (kbd "L") 'org-shiftright)
(evil-define-key 'insert org-mode-map (kbd "RET") (lambda () (interactive) (org-return nil)))

(evil-global-set-key 'insert (kbd "C-d") 'delete-char)
(evil-global-set-key 'insert (kbd "C-a") 'beginning-of-line)
(evil-global-set-key 'insert (kbd "C-e") 'end-of-line)

;; (evil-define-key 'normal pdf-view-mode-map (kbd "J") 'pdf-view-next-page)
;; (evil-define-key 'normal pdf-view-mode-map (kbd "K") 'pdf-view-previous-page)

;; (evil-define-key 'normal 'global "G"
;;   (lambda () (interactive) (evil-goto-line) (forward-line -1)))

;; (define-key global-map (kbd "C-SPC") nil)
;; (define-key global-map (kbd "C-S-SPC") nil)

(evil-global-set-key 'normal (kbd "C-_") nil)
(keymap-global-set "C-+" 'text-scale-increase)
(keymap-global-set "C-_" 'text-scale-decrease)
(keymap-global-set "C-)" (lambda () (interactive) (text-scale-set 0)))
