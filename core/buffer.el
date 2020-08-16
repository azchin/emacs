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

; (defun kill-buffer-and-its-windows ()
;   "Kill BUFFER and delete its windows.  Default is `current-buffer'.
; BUFFER may be either a buffer or its name (a string)."
;   (interactive)
;   (setq buffer  (current-buffer))
;   (if (buffer-live-p buffer)            ; Kill live buffer only.
;       (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
;         (when (and (buffer-modified-p buffer)
;                    (fboundp '1on1-flash-ding-minibuffer-frame))
;           (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
;         (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
;           (dolist (win  wins)           ; (User might keep buffer if modified.)
;             (when (window-live-p win)
;               ;; Ignore error, in particular,
;               ;; "Attempt to delete the sole visible or iconified frame".
;               (condition-case nil (delete-window win) (error nil))))))
;     (when (interactive-p)
;       (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

(defun kill-buffer-mod (buffer)
  "Kill the current buffer - even if modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer-and-its-windows buffer))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
	(mapc 'kill-buffer-mod (buffer-list))
  (kill-emacs))



;;;; midnight mode
;(require 'midnight)

;;;kill buffers if they were last disabled more than this seconds ago
;(setq clean-buffer-list-delay-special 900)

;; (defvar clean-buffer-list-timer nil
;;   "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

;; ;; run clean-buffer-list every 2 hours
;; (setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))

;;; kill everything, clean-buffer-list is very intelligent at not killing
;;; unsaved buffer.
;(setq clean-buffer-list-kill-regexps '("^.*$"))

;;; keep these buffer untouched
;;; prevent append multiple times
;(defvar clean-buffer-list-kill-never-buffer-names-init
;  clean-buffer-list-kill-never-buffer-names
;  "Init value for clean-buffer-list-kill-never-buffer-names")
;(setq clean-buffer-list-kill-never-buffer-names
;      (append
;       '("*Messages*" "*cmd*" "*scratch*" "*eshell*")
;       clean-buffer-list-kill-never-buffer-names-init))

;;; prevent append multiple times
;(defvar clean-buffer-list-kill-never-regexps-init
;  clean-buffer-list-kill-never-regexps
;  "Init value for clean-buffer-list-kill-never-regexps")
;;; append to *-init instead of itself
;(setq clean-buffer-list-kill-never-regexps
;      (append '("^\\*EMMS Playlist\\*.*$")
;	      clean-buffer-list-kill-never-regexps-init))
