;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup
(setq 
 backup-by-copying t
 backup-directory-alist `(("." . ,(emacsd "cache/backups")))
 auto-save-file-name-transforms `((".*" ,(emacsd "cache/saves/") t))
 auto-save-default t
 ;; auto-save-visited-mode t
 create-lockfiles nil
 version-control t
 delete-old-versions t
 kept-new-versions 2
 kept-old-versions 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Async shell commands
(defadvice erase-buffer (around erase-buffer-noop)
  "make erase-buffer do nothing")

(defadvice shell-command (around shell-command-unique-buffer activate compile)
  (if (or current-prefix-arg
          (not (string-match "[ \t]*&[ \t]*\\'" command)) ;; background
          (bufferp output-buffer)
          (stringp output-buffer))
      ad-do-it ;; no behavior change

    ;; else we need to set up buffer
    (let* ((command-buffer-name
            (format "*background: %s*"
                    (substring command 0 (match-beginning 0))))
           (command-buffer (get-buffer command-buffer-name)))

      (when command-buffer
        ;; if the buffer exists, reuse it, or rename it if it's still in use
        (cond ((get-buffer-process command-buffer)
               (set-buffer command-buffer)
               (rename-uniquely))
              ('t
               (kill-buffer command-buffer))))
      (setq output-buffer command-buffer-name)

      ;; insert command at top of buffer
      (switch-to-buffer-other-window output-buffer)
      (insert "Running command: " command
              "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")

      ;; temporarily blow away erase-buffer while doing it, to avoid
      ;; erasing the above
      (ad-activate-regexp "erase-buffer-noop")
      ad-do-it
      (ad-deactivate-regexp "erase-buffer-noop"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
(defun my-universal-command ()
  (interactive)
  (cond
   ((and buffer-file-name (string-match-p "Xresources" buffer-file-name))
    (shell-command (concat "xrdb " buffer-file-name)))
   ((and buffer-file-name (string-match-p "/nixos-config/.*\\.nix$" buffer-file-name))
    (shell-command "~/nixos-config/rebuild.sh &"))
   ((eq major-mode 'org-mode)
    (call-interactively 'org-ctrl-c-ctrl-c))
   ((eq major-mode 'sh-mode)
    (shell-command (concat "chmod +x " buffer-file-name)))))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  ;; (setq col (current-column))
  ;; (beginning-of-line) (setq start (point))
  ;; (end-of-line) (forward-char) (setq end (point))
  ;; (save-excursion (forward-line n) (setq endline (point)))
  (let* ((col (current-column))
         (start (progn (beginning-of-line) (point)))
         (finishline (save-excursion (forward-line (* (1+ (abs n)) (/ n (abs n)))) (point)))
         (end (progn (end-of-line) (forward-char) (point)))
         (line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (if (> finishline start) (indent-region start finishline)
      (indent-region finishline start))
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (- n)))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line n))

(keymap-global-set "M-<up>" 'move-line-up)
(keymap-global-set "M-<down>" 'move-line-down)

(defun tramp-change-sudoedit-doas ()
  (interactive)
  (delete '("sudoedit"
            (tramp-sudo-login
             (("sudo")
              ("-u" "%u")
              ("-S")
              ("-H")
              ("-p" "Password:")
              ("--"))))
          tramp-methods)
  (add-to-list 'tramp-methods
               '("sudoedit"
                 (tramp-sudo-login
                  (("doas")
                   ("-u" "%u")
                   ("-p" "Password:")
                   ("--"))))))
;; (add-hook 'tramp--startup-hook 'tramp-change-sudoedit-doas)

(defun my-change-number-at-point (change increment)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (forward-word)
      (search-backward (number-to-string number))
      (replace-match (number-to-string (funcall change number increment)))
      (goto-char point))))

(defun my-increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (my-change-number-at-point '+ (or increment 1)))

(defun my-decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (my-change-number-at-point '- (or increment 1)))

;; (global-set-key (kbd "C-c a") 'my-increment-number-at-point)
;; (global-set-key (kbd "C-c x") 'my-decrement-number-at-point)

(defun point-at-beginning-of-word-p ()
  "Check if point is at the beginning of a word."
  (looking-back "\\b" 1))

(defun my-replace-marked-region-with-killed-text ()
  "Replace the marked (active) region with the text from the kill ring."
  (interactive)
  (let ((killed-text (current-kill 0 t)))
    (if (use-region-p)
        (let ((region-start (region-beginning))
              (region-end (region-end)))
          (delete-region region-start region-end))
      (progn
        (unless (point-at-beginning-of-word-p)
          (backward-word))
        (kill-word nil)))
    (insert killed-text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External
;; https://oremacs.com/2015/01/04/dired-nohup/
;; https://www.emacswiki.org/emacs/OpenWith
;; https://stackoverflow.com/questions/25109968/in-emacs-how-to-open-file-in-external-program-without-errors

(defun open-in-external-explorer ()
  "Open the current directory in an external explorer (Linux only atm)"
  (interactive)
  (let ((external-explorer "xdg-open"))
    (start-process "" nil external-explorer default-directory)))

(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto insert
(auto-insert-mode 1)
(setq auto-insert-query nil)
(setq auto-insert-directory (emacsd "insert"))

(setq auto-insert-alist (delq (assoc '("\\.el\\'" . "Emacs Lisp header")
                                     auto-insert-alist) auto-insert-alist))
(define-auto-insert 'sh-mode "shell")
;; (define-auto-insert "\\.\\(cc\\|cpp\\)" "cplusplus")

;; Autocmd
;; (defun suckless-autocmd ()
;;   (when (and buffer-file-name (string-match "config.h" (buffer-name)))
;;     (shell-command "sudo make clean install")))
;; (add-hook 'after-save-hook 'suckless-autocmd)

(defun environment-autocmd ()
  (when (and (not (equal major-mode 'sh-mode)) (string-match "\\(profile\\|aliases\\)" (buffer-name)))
    (sh-mode)))
(add-hook 'buffer-list-update-hook 'environment-autocmd)

;; (defun executable-autocmd ()
;;   (when (string-match ".*\\.sh" (buffer-name))
;;     (shell-command (concat "[ $(stat -c '%a' " buffer-file-name ") = 755 ] || chmod 755 " buffer-file-name))))
;; (add-hook 'after-save-hook 'executable-autocmd)

;; Major modes
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hideshow mode
(defvar my-hs-hide-threshold 50
  "Hideshow will hide all if buffer has more lines than this value")
(defun my-hs-hide-all ()
  "Conditionally hide all, depending on `my-hs-hide-threshold'"
  (interactive)
  (when (> (line-number-at-pos (point-max) t)
           my-hs-hide-threshold)
    (hs-hide-all)))
(add-hook 'prog-mode-hook 'hs-minor-mode)
;; (add-hook 'hs-minor-mode-hook 'my-hs-hide-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
(put 'suspend-frame 'disabled t)
(setq doc-view-continuous t)
(setq use-short-answers t)
(setq-default lexical-binding t)
(setopt epa-pinentry-mode 'loopback)
(setq epg-gpg-program "gpg2")
(fset 'epg-wait-for-status 'ignore)

(provide 'my-extra)
