(provide 'my-buffer)
(require 'my-appearance)

(defun split-window-with-margins (&optional window)
  (interactive)
  (let* ((window (or window (selected-window)))
         (margs (not (eq (window-width window)
                         (window-total-width window)))))
    (when margs (set-window-margins window 0 0))
    (let ((res (split-window-sensibly window)))
      (when margs (get-desired-display-margin)) res)))

(setq split-window-preferred-function 'split-window-with-margins)

;; (defvar pop-up-frame-regexp-list '("\\*Buffer List\\*"
;;                                    "\\*eshell\\*"
;;                                    "\\*.*-scratch\\*"
;;                                    "\\*Org Select\\*"
;;                                    "\\*Org Agenda\\*"
;;                                    "\\*Agenda Commands\\*"
;;                                    )
;;   "List of regular expressions that denote the buffers to be displayed
;; in a pop-up frame")
;; (defvar pop-up-frame-regexp-list '("\\*Flycheck errors\\*"
;;                                    )
;;   "List of regular expressions that denote the buffers to be displayed
;; in a pop-up frame")

(defun construct-regexp-from-list (list)
  "Constructs a regular expression that matches one of the elements in `list'"
  (cond ((equal list nil) "")
        ((equal (length list) 1) (car list))
        (t (concat (car list) "\\|" (construct-regexp-from-list (cdr list))))))

(defvar pop-up-frame-modes '(help-mode
                             apropos-mode)
  "List of major modes that denote the buffers to be displayed
in a pop-up frame")

(defun display-buffer-mode-query (name action)
  "Function passed to `display-buffer-alist' that checks a buffer's major mode against
`pop-up-frame-modes'"
  (member (buffer-local-value 'major-mode (get-buffer name)) pop-up-frame-modes))

(defun display-buffer-pdf-from-dired (name action)
  "Special logic to display pdf files in pop-up frames unless the current buffer
is dired"
  (and (equal (buffer-local-value 'major-mode (get-buffer name)) 'pdf-view-mode)
       (not (equal major-mode 'dired-mode))))

(defvar my-window-parameters
  '(window-parameters . ((no-delete-other-windows . t))))

(setq fit-window-to-buffer-horizontally t)

(defun dired-default-directory-on-left ()
  "Display `default-directory' in side window on left, hiding details."
  (interactive)
  (let ((buffer (dired-noselect default-directory)))
    (with-current-buffer buffer (dired-hide-details-mode t))
    (display-buffer-in-side-window
     buffer `((side . left) (slot . 0)
              (window-width . fit-window-to-buffer)
              (preserve-size . (t . nil)) ,my-window-parameters))))

;; (add-to-list 'display-buffer-alist
;;              '(display-buffer-mode-query
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . 0)))

;; (add-to-list 'display-buffer-alist
;;              `(,(construct-regexp-from-list pop-up-frame-regexp-list)
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . 0)))

;; (add-to-list 'display-buffer-alist
;;              '(display-buffer-pdf-from-dired
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . 0)))

(defvar help-modes '(help-mode apropos-mode))

(defun my-display-buffer-match-mode-window (windows mode-list buffer alist)
  (when windows
    (let ((current-window-mode (buffer-local-value 'major-mode (window-buffer (car windows)))))
      (or (when (member current-window-mode mode-list)
            (display-buffer-reuse-mode-window buffer `((mode . ,current-window-mode) . ,alist)))
          (my-display-buffer-match-mode-window (cdr windows) mode-list buffer alist)))))

(defun display-buffer-help-window (buffer alist)
  (my-display-buffer-match-mode-window (window-list) help-modes buffer alist))

(add-to-list 'display-buffer-alist
             '("\\*grep\\*"
               (display-buffer-reuse-window display-buffer-same-window)))

(add-to-list 'display-buffer-alist
             '((lambda (name action)
                 (member (buffer-local-value 'major-mode (get-buffer name))
                         help-modes))
               (display-buffer-help-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             `("\\*.*eshell\\*" display-buffer-in-side-window
               (side . bottom) (slot . 0) (window-height . 12)
               ,my-window-parameters))

(setq display-buffer-base-action
      '((display-buffer-reuse-mode-window
         display-buffer-same-window
         display-buffer-pop-up-window)
        (inhibit-switch-frame . t)))

(setq Man-notify-method 'aggressive)
;; (setq grep-command "grep --color -nH --null ")

(setq revert-without-query '(".*"))

(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window win)
                (error (condition-case nil (tab-bar-close-tab) (error nil))))))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

(defun kill-buffer-and-its-frames (buffer)
  "Kill BUFFER and delete its frames.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-frame (window-frame win))
                (error nil))))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

(defun kill-buffer-mod (buffer)
  "Kill the current buffer and all its windows - even if modified."
  (interactive)
  (let ((cur (current-buffer)))
    (switch-to-buffer buffer)
    (set-buffer-modified-p nil)
    (switch-to-buffer cur)
    (kill-buffer-and-its-windows buffer)))

(defun kill-buffer-greedy (buffer)
  "Kill the current buffer and all its frames - even if modified."
  (interactive)
  (let ((cur (current-buffer)))
    (switch-to-buffer buffer)
    (set-buffer-modified-p nil)
    (switch-to-buffer cur)
    (kill-buffer-and-its-frames buffer)))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (when desktop-save-mode
    (desktop-save-mode 0)
    (desktop-save (emacsd "cache/default-desktop")))
  (unless (daemon-mode-snapshot) (save-buffers-kill-terminal))
  (mapc 'kill-buffer-mod (buffer-list))
  ;; (mapc 'delete-frame (frame-list))
  (kill-emacs))

(defun my-clean-buffer-list ()
  "Kill old buffers that have not been displayed recently.
The relevant variables are `clean-buffer-list-delay-general',
`clean-buffer-list-delay-special', `clean-buffer-list-kill-buffer-names',
`clean-buffer-list-kill-never-buffer-names',
`clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-never-regexps'.
While processing buffers, this procedure displays messages containing
the current date/time, buffer name, how many seconds ago it was
displayed (can be nil if the buffer was never displayed) and its
lifetime, i.e., its \"age\" when it will be purged."
  (interactive)
  (let* ((tm (current-time)) bts (ts (format-time-string "%Y-%m-%d %T" tm))
        delay cbld bn)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (setq bts (with-current-buffer buf buffer-display-time)
              bn (buffer-name buf)
              delay (if bts (round (float-time (time-subtract tm bts))) 0)
              cbld (clean-buffer-list-delay bn))
        (message "[%s] `%s' [%s %d]" ts bn delay cbld)
        (unless (or (cl-find bn clean-buffer-list-kill-never-regexps
                             :test (lambda (bn re)
                                     (if (functionp re)
                                         (funcall re bn)
                                       (string-match re bn))))
                    (cl-find bn clean-buffer-list-kill-never-buffer-names
                             :test #'string-equal)
                    (get-buffer-process buf)
                    (and (buffer-file-name buf) (buffer-modified-p buf))
                    (tab-bar-get-buffer-tab buf 'visible)
                    (< delay cbld))
          (message "[%s] killing `%s'" ts bn)
          (kill-buffer buf))))))

;; (defun create-eshell-window ()
;;   "Create an eshell terminal window"
;;   (interactive)
;;   (let ((new-window (split-window (frame-root-window) -16 'below)))
;;     (select-window new-window)
;;     (eshell t)))

(defun create-eshell-window ()
  "Create an eshell terminal window"
  (interactive)
  (eshell t))

(defun create-scratch-frame (name mode)
  "Create a scratch frame"
  (interactive)
  (let ((live-scratch (buffer-live-p (get-buffer name)))
        (new-window (display-buffer (get-buffer-create name))))
    (select-window new-window)
    (funcall mode)
    (whitespace-mode)
    (unless live-scratch
      (insert (concat mode-name scratch-skeleton))
      (unless (equal comment-use-syntax 'undecided) (comment-line -5))
      (forward-line 1))))

(defun create-new-frame (name &optional mode)
  "Create a new frame with specified buffer"
  (interactive (list (read-buffer "Buffer name:" (current-buffer) nil)))
  (let ((new-frame (make-frame-command)))
    (select-frame new-frame)
    (switch-to-buffer (get-buffer-create name))
    (funcall (or mode 'text-mode))
    (whitespace-mode)
    (auto-insert)))

(defun create-new-buffer (name &optional mode)
  "Create a new buffer in the current window"
  (interactive (list (read-buffer "Buffer name:" (current-buffer) nil)))
  (switch-to-buffer (get-buffer-create name))
  (funcall (or mode 'fundamental-mode))
  (auto-insert))

(defun create-new-frame-command (command)
  "Create a new frame and evaluate command"
  (let ((new-frame (make-frame-command)))
    (select-frame new-frame)
    (switch-to-buffer (get-buffer-create "*Untitled*"))
    (funcall command)))

(defun create-new-frame-scratch ()
  "Create a new frame go to scratch"
  (interactive)
  (let ((new-frame (make-frame-command)))
    (select-frame new-frame)
    (switch-to-buffer "*scratch*")))

(defun create-new-frame-file () 
  "Create a new frame and choose file"
  (interactive)
  (let ((new-file (read-file-name "Choose file: " home-dir))
        (new-frame (make-frame-command)))
    (select-frame new-frame)
    (find-file new-file)))

(defun create-new-frame-buffer () 
  "Create a new frame and choose buffer"
  (interactive)
  (let ((new-buffer (read-buffer "Choose buffer: "))
        (new-frame (make-frame-command)))
    (select-frame new-frame)
    (switch-to-buffer new-buffer)))

(defun create-dired-frame (&optional dir)
  "Create a dired frame"
  (interactive)
  (let ((new-frame (make-frame-command)))
    (select-frame new-frame)
    (dired dir)))

(defvar buffer-regexp-name
  "\\*Help\\*\\|\\*Buffer List\\*\\|\\*Messages\\*")

(defun kill-regex-buffer-frame (&optional regexp)
  "Kill all buffers and its frames matching the pattern regexp"
  (interactive)
  (mapc 'kill-buffer-and-its-frames
        (seq-filter 
         (lambda (x) (string-match-p (or regexp buffer-regexp-name) (buffer-name x)))
         (buffer-list))))

(setq frame-auto-hide-function 'delete-frame)

(defun filter-dired-buffer-list ()
  "Bury all the dired buffers from the buffer list"
  (interactive)
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (eq 'dired-mode major-mode) (bury-buffer buff)))))

(add-hook 'after-change-major-mode-hook 'filter-dired-buffer-list)

(defvar tab-close-buffer-tmp nil)

(defun tab-close-save-buffer ()
  (interactive)
  (setq tab-close-buffer-tmp (current-buffer))
  (tab-close))

(defun restore-tab-close-buffer ()
  (interactive)
  (let ((ret (if tab-close-buffer-tmp
                 tab-close-buffer-tmp
               (read-buffer "Choose buffer: "))))
    (setq tab-close-buffer-tmp nil)
    ret))

(winner-mode)
;; C-c LEFT, C-c RIGHT
