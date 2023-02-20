(provide 'my-leader)

;; (evil-set-leader '(normal visual emacs) (kbd "SPC")) ;; or use '(normal visual)
(evil-set-leader '(normal visual) (kbd "SPC")) ;; or use '(normal visual)
;; (evil-define-key '(normal visual emacs) 'global (kbd "SPC") 'evil-send-leader) ;; or use '(normal visual)
;; (evil-global-set-key 'normal (kbd "<leader>ol") 'org-store-link)

(defvar leader-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(defvar leader-rw-states '(normal visual))

(defvar leader-states (append leader-rw-states '(motion))
  "Evil states for the leader keybinding")

(define-minor-mode leader-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(leader-intercept-mode)

(defvar leader-key-string "SPC")

(dolist (state leader-states)
  (evil-make-intercept-map
   ;; NOTE: This requires an evil version from 2018-03-20 or later
   (evil-get-auxiliary-keymap leader-intercept-mode-map state t t)
   state))
;; (evil-define-key leader-states leader-intercept-mode-map (kbd leader-key-string) 'evil-send-leader)
(evil-define-key leader-states 'global (kbd (concat "<leader> " leader-key-string)) 'keyboard-quit)
(evil-define-key leader-states 'global (kbd "<leader> a") 'execute-extended-command)
(evil-define-key leader-states 'global (kbd "g t") (lambda (x) (interactive "P") (if x (tab-bar-select-tab x) (tab-bar-switch-to-next-tab))))
(evil-define-key leader-states 'global (kbd "<leader> c a") 'modus-themes-toggle)
(evil-define-key 'normal 'global (kbd "<leader> c t") 'toggle-tabs)
(evil-define-key 'normal 'global (kbd "<leader> c y") 'conditional-tabify)
(evil-define-key 'normal 'global (kbd "<leader> c i") 'indent-whole-buffer)
(evil-define-key 'normal 'global (kbd "<leader> c m") (lambda () (interactive) (menu-bar-mode 'toggle)))
(evil-define-key 'visual 'global (kbd "<leader> c e") (lambda () (interactive) (call-interactively 'eval-last-sexp) (evil-exit-visual-state)))
(evil-define-key leader-rw-states 'global (kbd "<leader> c r") 'rename-uniquely)
;; (evil-define-key leader-rw-states 'global (kbd "<leader> c f") 'fill-whole-buffer)
(evil-define-key leader-rw-states 'global (kbd "<leader> c f") 'fill-region)
(evil-define-key 'visual 'global (kbd "<leader> c w") 'count-words-region)
(evil-define-key 'normal 'global (kbd "<leader> c z") 'insert-zero-width-char)
(evil-define-key leader-states 'global (kbd "<leader> e e") (lambda () (interactive) (if desktop-save-mode
                                                                                         (progn (desktop-save (emacsd "cache/default-desktop"))
                                                                                                (desktop-save-mode 0))
                                                                                       (desktop-save-mode 1)
                                                                                       (desktop-read (emacsd "cache/default-desktop")))))
(evil-define-key leader-states 'global (kbd "<leader> e s") 'save-project-desktop)
(evil-define-key leader-states 'global (kbd "<leader> e r") (lambda () (interactive) (desktop-read (emacsd "cache/default-desktop") (desktop-save-mode 1))))
(evil-define-key leader-states 'global (kbd "<leader> e p") 'read-project-desktop)
(evil-define-key leader-states 'global (kbd "<leader> o l") 'org-store-link)
;; "o a" 'org-agent
(evil-define-key leader-rw-states 'global (kbd "<leader> o c") 'org-capture)
(evil-define-key leader-rw-states 'global (kbd "<leader> o a") (lambda () (interactive) (create-new-frame-command 'org-agenda-list) (delete-other-windows)))
;; (evil-define-key leader-states 'global (kbd "<leader> m i") 'magit-init)
;; (evil-define-key leader-states 'global (kbd "<leader> m c") 'magit-clone)
;; (evil-define-key leader-states 'global (kbd "<leader> m m") 'magit-status)
;; (evil-define-key leader-states 'global (kbd "<leader> m b") 'magit-blame)
;; TODO vc-git-create-repo, make function for cloning and setting/changing url
(evil-define-key 'normal 'global (kbd "<leader> v v") 'vc-next-action)
(evil-define-key 'normal 'global (kbd "<leader> v i") 'vc-register)
(evil-define-key 'normal 'global (kbd "<leader> v d") 'vc-dir)
(evil-define-key 'normal 'global (kbd "<leader> m <") 'smerge-keep-upper)
(evil-define-key 'normal 'global (kbd "<leader> m >") 'smerge-keep-lower)
(evil-define-key 'normal 'global (kbd "<leader> m |") 'smerge-keep-base)
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
(evil-define-key leader-states 'global (kbd "<leader> f e a") (lambda () (interactive) (find-file (emacsd "core/my-appearance.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e b") (lambda () (interactive) (find-file (emacsd "core/my-buffer.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e p") (lambda () (interactive) (find-file (emacsd "core/stagetwo.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e i") (lambda () (interactive) (find-file (emacsd "init.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e k") (lambda () (interactive) (find-file (emacsd "core/my-leader.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e l") (lambda () (interactive) (find-file (emacsd "core/my-late.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e m") (lambda () (interactive) (find-file (emacsd "core/my-modeline.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e t") (lambda () (interactive) (find-file (emacsd "core/my-tabs.el"))))
(evil-define-key leader-states 'global (kbd "<leader> f e o") (lambda () (interactive) (find-file (emacsd "core/my-org.el"))))
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
(evil-define-key leader-states 'global (kbd "<leader> p f") 'project-find-file)
(evil-define-key leader-states 'global (kbd "<leader> p d") 'project-dired)
(evil-define-key leader-states 'global (kbd "<leader> p v") 'project-vc-dir)
(evil-define-key leader-states 'global (kbd "<leader> p e") 'project-eshell)
(evil-define-key leader-states 'global (kbd "<leader> p c") 'project-compile)
(evil-define-key leader-states 'global (kbd "<leader> p b") 'project-switch-to-buffer)
(evil-define-key leader-states 'global (kbd "<leader> p p") 'project-switch-project)
(evil-define-key leader-states 'global (kbd "<leader> p k") 'project-kill-buffers)
(evil-define-key leader-states 'global (kbd "<leader> p R") 'project-forget-project)
(evil-define-key leader-states 'global (kbd "<leader> p g") 'project-find-regexp)
(evil-define-key leader-states 'global (kbd "<leader> t t") 'tab-new)
(evil-define-key leader-states 'global (kbd "<leader> t b") 'switch-to-buffer-other-tab)
(evil-define-key leader-states 'global (kbd "<leader> t f") 'find-file-other-tab)
(evil-define-key leader-states 'global (kbd "<leader> t w") 'tab-window-detach)
(evil-define-key leader-states 'global (kbd "<leader> t p") (lambda () (interactive) (let ((new-file (read-file-name "Choose file: " (project-root (project-current)))))
                                                                                       (tab-new) (find-file new-file))))
(evil-define-key leader-states 'global (kbd "<leader> t d") (lambda () (interactive) (dired-other-tab default-directory)))
(evil-define-key leader-states 'global (kbd "<leader> t h") (lambda () (interactive) (let ((new-file (read-file-name "Choose file: " home-dir))) (tab-new) (find-file new-file))))
;; (evil-define-key leader-states 'global (kbd "<leader> t z") (lambda () (interactive) (switch-to-buffer-other-tab (read-buffer-to-switch "Choose buffer: "))))
;; TODO not quite correct, check for nil project
(evil-define-key leader-states 'global (kbd "<leader> t v") (lambda () (interactive) (let ((vcbuf (project-root (project-current t))))
                                                                                       (tab-new) (vc-dir vcbuf))))
(evil-define-key leader-states 'global (kbd "<leader> t q") 'tab-close-save-buffer)
(evil-define-key leader-states 'global (kbd "<leader> t o") 'tab-close-other)
;; (evil-define-key leader-states 'global (kbd "<leader> t m") 'tab-move)
;; (evil-define-key leader-states 'global (kbd "<leader> t M") (lambda () (interactive) (tab-move -1)))
(evil-define-key leader-states 'global (kbd "<leader> t m") (lambda () (interactive) (tab-bar-move-tab-to (read-number "Tab index: "))))
;; (defun tab-new-scratch ()
;;   (interactive)
;;   (tab-new) (call-interactively 'noisy-send-leader))
;; (evil-define-key leader-states 'global (kbd "<leader> t s") 'tab-new-scratch)
(keymap-global-set "C-S-t" 'tab-new)
(evil-define-key leader-states 'global (kbd "<leader> d d") 'dired-jump)
;; dired-jump opens new window, dired uses current window
(evil-define-key leader-states 'global (kbd "<leader> d h") (lambda () (interactive) (dired home-dir)))
(evil-define-key leader-states 'global (kbd "<leader> d c") (lambda () (interactive) (dired (concat home-dir ".config"))))
(evil-define-key leader-states 'global (kbd "<leader> d e") (lambda () (interactive) (dired (emacsd "core"))))
(evil-define-key leader-states 'global (kbd "<leader> d r") (lambda () (interactive) (dired (concat home-dir "drive"))))
(evil-define-key leader-states 'global (kbd "<leader> d p") (lambda () (interactive) (dired (concat home-dir "projects"))))
(evil-define-key leader-states 'global (kbd "<leader> d l") (lambda () (interactive) (dired (concat home-dir "clone"))))
(evil-define-key leader-states 'global (kbd "<leader> d o") (lambda () (interactive) (dired (concat home-dir "org"))))
(evil-define-key leader-states 'global (kbd "<leader> d s") (lambda () (interactive) (dired (concat "/-:" (read-string "TRAMP: ") ":"))))
(evil-define-key leader-states 'global (kbd "<leader> d u") (lambda () (interactive) (dired "/sudo::/")))
;; (evil-define-key leader-states 'global (kbd "<leader> d g") 'dired)
;; "d z h" (lambda () (interactive)
;;          (progn (setq dired-listing-switches
;;                  "-hlo --group-directories-first --time-style=iso")
;;                 (revert-buffer)))
;; https://stackoverflow.com/questions/22971299/conditionally-set-dired-listing-switches-locally-but-nil-remotely
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Summary.html#Help-Summary
(evil-define-key leader-states 'global (kbd "<leader> x g") 'gnus)
(evil-define-key leader-states 'global (kbd "<leader> h k") 'describe-key)
(evil-define-key leader-states 'global (kbd "<leader> h v") 'describe-variable)
(evil-define-key leader-states 'global (kbd "<leader> h f") 'describe-function)
(evil-define-key leader-states 'global (kbd "<leader> h m") 'describe-mode)
(evil-define-key leader-states 'global (kbd "<leader> h b") 'describe-bindings)
(evil-define-key leader-states 'global (kbd "<leader> h a a") 'apropos)
(evil-define-key leader-states 'global (kbd "<leader> h a c") 'apropos-command)
(evil-define-key leader-states 'global (kbd "<leader> h a f") 'apropos-function)
(evil-define-key leader-states 'global (kbd "<leader> h a v") 'apropos-variable)
(evil-define-key leader-states 'global (kbd "<leader> h o") 'describe-symbol)
(evil-define-key leader-states 'global (kbd "<leader> b b") 'switch-to-buffer)
(evil-define-key leader-states 'global (kbd "<leader> b c")
  (lambda () (interactive) (let ((clean-buffer-list-kill-never-regexps
                                  '("^\\ .*$" "\\*.*scratch\\*" "\\` \\*Minibuf-.*\\*\\'" "^\\*EMMS Playlist\\*.*$"
                                    "^\\*Article.*\\*$" "^\\*Summary.*\\*$" "^\\*eww\\*$" "^\\*Group\\*$")))
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
(evil-define-key leader-rw-states 'global (kbd "<leader> s") 'save-buffer)
(evil-define-key leader-states 'global (kbd "<leader> ;") 'eval-expression)
(evil-define-key leader-states 'global (kbd "<leader> /") (lambda () (interactive) (evil-ex-nohighlight) (lazy-highlight-cleanup t)))
;; (evil-define-key leader-states 'global (kbd "<leader> /") 'lazy-highlight-cleanup)
(evil-define-key leader-states 'global (kbd "<leader> 1") 'shell-command)
(evil-define-key leader-states 'global (kbd "<leader> w q") 'delete-window)
(evil-define-key leader-states 'global (kbd "<leader> w s") 'evil-window-split)
(evil-define-key leader-states 'global (kbd "<leader> w v") 'evil-window-vsplit)
;; (evil-define-key leader-states 'global (kbd "<leader> w s") 'split-window-with-margins)
(evil-define-key leader-states 'global (kbd "<leader> w w") 'evil-window-next)
(evil-define-key leader-states 'global (kbd "<leader> w x") 'evil-window-exchange)
(defun window-file-vsplit ()
  (interactive)
  (let ((new-file (read-file-name "Choose file: ")))
    (evil-window-vsplit) (find-file new-file)))
(evil-define-key leader-states 'global (kbd "<leader> w f s") (lambda () (interactive) (let ((new-file (read-file-name "Choose file: ")))
                                                                                         (evil-window-split) (find-file new-file))))
(evil-define-key leader-states 'global (kbd "<leader> w f v") 'window-file-vsplit)
(evil-define-key leader-states 'global (kbd "<leader> w b s") (lambda () (interactive) (let ((new-buffer (restore-tab-close-buffer)))
                                                                                         (evil-window-split) (switch-to-buffer new-buffer))))
(defun window-buffer-vsplit ()
  (interactive)
  (let ((new-buffer (restore-tab-close-buffer)))
    (evil-window-vsplit) (switch-to-buffer new-buffer)))
(evil-define-key leader-states 'global (kbd "<leader> w b v") 'window-buffer-vsplit)
(evil-define-key leader-states 'global (kbd "<leader> w d s") (lambda () (interactive) (evil-window-split) (dired default-directory)))
(defun window-dired-vsplit ()
  (interactive)
  (evil-window-vsplit) (dired default-directory))
(evil-define-key leader-states 'global (kbd "<leader> w d v") 'window-dired-vsplit)
(evil-define-key leader-states 'global (kbd "<leader> w h") 'evil-window-left)
(evil-define-key leader-states 'global (kbd "<leader> w j") 'evil-window-down)
(evil-define-key leader-states 'global (kbd "<leader> w k") 'evil-window-up)
(evil-define-key leader-states 'global (kbd "<leader> w l") 'evil-window-right)
(evil-define-key leader-states 'global (kbd "<leader> w o") 'delete-other-windows)
(evil-define-key leader-states 'global (kbd "<leader> q b") (lambda () (interactive) (kill-buffer-and-its-windows (current-buffer))))
;; "q k" (lambda () (interactive) (kill-buffer-mod (current-buffer)))
;; "q g" (lambda () (interactive) (kill-buffer-greedy (current-buffer)))
(evil-define-key leader-states 'global (kbd "<leader> q o") 'delete-other-frames)
(defun delete-frame-or-kill-terminal ()
  (interactive)
  (cond ((> (length (visible-frame-list)) 1) (delete-frame))
        (t (save-buffers-kill-terminal))))
(defun delete-granular-display ()
  (interactive)
  (cond ((> (length (window-list)) 1) (delete-window))
        ((> (length (tab-bar-tabs)) 1) (tab-close-save-buffer))
        (t (delete-frame-or-kill-terminal))))
(evil-define-key leader-states 'global (kbd "<leader> q f") 'delete-frame-or-kill-terminal)
(evil-define-key leader-states 'global (kbd "<leader> q q") 'delete-granular-display)
;; (evil-define-key leader-states 'global (kbd "<leader> q h") 'kill-regex-buffer-frame)
(evil-define-key leader-states 'global (kbd "<leader> q a") (lambda () (interactive) (if (daemon-mode-snapshot) (mapc 'delete-frame (frame-list))
                                                                                       (save-buffers-kill-terminal))))
;; (evil-define-key leader-states 'global (kbd "<leader> q e") 'server-shutdown)
(evil-define-key leader-states 'global (kbd "<leader> q e") 'save-buffers-kill-terminal)

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

(evil-define-key leader-states org-mode-map (kbd "<leader> o i") 'org-insert-structure-template)
(evil-define-key leader-states org-mode-map (kbd "<leader> o s") 'org-edit-src-code)
;; (evil-define-key leader-states org-mode-map (kbd "<leader> o s") 'org-schedule)
;; (evil-define-key leader-states org-mode-map (kbd "<leader> o d") 'org-deadline)
(evil-define-key leader-states org-mode-map (kbd "<leader> o l") 'org-insert-link)
(evil-define-key leader-states org-mode-map (kbd "<leader> o T") 'org-time-stamp)
(evil-define-key leader-states org-mode-map (kbd "<leader> o t") (lambda () (interactive) (insert " ") (org-insert-time-stamp (current-time) nil t) (delete-char 1)))
(evil-define-key leader-states org-mode-map (kbd "<leader> o e") 'org-export-dispatch)
(evil-define-key leader-states org-mode-map (kbd "<leader> o p") 'org-latex-export-to-pdf)

;; (evil-define-key leader-states org-mode-map (kbd "<leader> o e l") 'org-latex-export-to-latex)
;; (evil-define-key leader-states org-mode-map (kbd "<leader> o e p") 'org-latex-export-to-pdf)
;;NOTE Flyspell is by default bound to []s

;; (evil-set-leader '(normal visual replace emacs motion operator) (kbd "<SPC>"))
;; (evil-define-key 'normal 'global (kbd "<leader> o l") 'org-store-link)

;; (evil-define-key 'normal pdf-view-mode-map (kbd "J") 'pdf-view-next-page)
;; (evil-define-key 'normal pdf-view-mode-map (kbd "K") 'pdf-view-previous-page)

;; (define-key global-map (kbd "C-SPC") nil)
;; (define-key global-map (kbd "C-S-SPC") nil)
