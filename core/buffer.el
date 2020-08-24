;; (defun add-to-display-buffer-list
;;     (add-to-list 'display-buffer-alist
;;                   ))

(add-to-list 'display-buffer-alist
             '("\\*Buffer List\\*\\|\\*Help\\*\\|\\*eshell\\*\\|\\*Messages\\*\\|\\*.*-scratch\\*\\|\\*Org Select\\*"
               (display-buffer-reuse-window display-buffer-pop-up-frame)
               (reusable-frames . 0)))
               ;; . ((display-buffer-reuse-window display-buffer-pop-up-frame)
               ;;    (reusable-frames . 0))))

;; (add-to-list 'display-buffer-alist
;;              '("\\*grep\\*"
;;                display-buffer-same-window))

(setq Man-notify-method 'aggressive)
;; (setq grep-command "grep --color -nH --null ")
;; (setq grep-command "grep ")

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
              (condition-case nil (delete-window win) (error nil))))))
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
  "Kill the current buffer - even if modified."
  (interactive)
  (let ((cur (current-buffer)))
    (switch-to-buffer buffer)
    (set-buffer-modified-p nil)
    (switch-to-buffer cur)
    (kill-buffer-and-its-windows buffer)))

(defun kill-buffer-greedy (buffer)
  "Kill the current buffer - even if modified."
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
  (mapc 'kill-buffer-mod (buffer-list))
  (kill-emacs))


(defvar scratch-skeleton
  " buffer for temporary work\n\n"
  "Message for custom made scratch buffers")

(auto-insert-mode 1)
(setq auto-insert-query nil)
(define-auto-insert "\\*.*-scratch\\*" 'scratch-skeleton)

(defun create-scratch-frame (mode name)
  "Create a scratch frame"
  (interactive)
  (let ((live-scratch (buffer-live-p (get-buffer name)))
        (new-window (display-buffer (get-buffer-create name))))
    (select-window new-window)
    (funcall mode)
    (unless live-scratch
      (insert (concat mode-name scratch-skeleton))
      (comment-line -5)
      (forward-line 1))))

(defun create-new-frame (name &optional mode)
  "Create a new frame with specified buffer"
  (interactive)
  (let ((live (buffer-live-p (get-buffer name)))
        (new-window (display-buffer (get-buffer-create name))))
    (select-window new-window)
    (unless live
      (funcall (or mode text-mode)))))

(defun create-dired-frame (&optional dir)
  "Create a dired frame"
  (interactive)
  (let ((new-frame (make-frame-command)))
    (select-frame new-frame)
    (dired dir)))

(defvar buffer-regexp-name
  "\\*Help\\*\\|\\*Buffer List\\*\\|\\*Messages\\*")
  ; (regexp-opt '("*Help*" "*Buffer List*" "*Messages*")))

(defun kill-regex-buffer-frame (&optional regexp)
  "Kill all buffers and its frames matching the pattern regexp"
  (interactive)
  (mapc 'kill-buffer-and-its-frames
        (seq-filter 
         (lambda (x) (string-match-p (or regexp buffer-regexp-name) (buffer-name x)))
         (buffer-list))))
