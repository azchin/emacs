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
                             ;; messages-buffer-mode
                             magit-status-mode
                             )
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

;; (add-to-list 'display-buffer-alist
;;              '(display-buffer-mode-query
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . 0)))

;; (add-to-list 'display-buffer-alist
;;              `(,(construct-regexp-from-list pop-up-frame-regexp-list)
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . 0)))

                                        ; (add-to-list 'display-buffer-alist
                                        ;              '(display-buffer-pdf-from-dired
                                        ;                (display-buffer-reuse-window display-buffer-pop-up-frame)
                                        ;                (reusable-frames . 0)))

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
    (progn (desktop-save-mode 0)
           (desktop-save (emacsd "cache/default-desktop"))))
  (unless daemon-mode-snapshot (save-buffers-kill-terminal))
  (mapc 'kill-buffer-mod (buffer-list))
  ;; (mapc 'delete-frame (frame-list))
  (kill-emacs))

(defun create-eshell-window ()
  "Create an eshell terminal window"
  (interactive)
  (let ((new-window (split-window-below -16)))
    (select-window new-window)
    (eshell 't)))

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
;; (let ((new-f (buffer-live-p (get-buffer name)))
;;       (new-window (display-buffer (get-buffer-create name))))
;;   (select-window new-window)
;;   (unless live
;;     (funcall (or mode 'text-mode)))))

(defun create-new-frame-command (command)
  "Create a new frame and evaluate command"
  (let ((new-frame (make-frame-command)))
    (select-frame new-frame)
    (switch-to-buffer (get-buffer-create "*Untitled*"))
    (funcall command)))

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

(setq frame-auto-hide-function 'delete-frame)
