(provide 'my-extra)

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
;; Commands
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
(add-hook 'tramp--startup-hook 'tramp-change-sudoedit-doas)

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

(defun executable-autocmd ()
  (when (string-match ".*\\.sh" (buffer-name))
    (shell-command (concat "[ $(stat -c '%a' " buffer-file-name ") = 755 ] || chmod 755 " buffer-file-name))))
(add-hook 'after-save-hook 'executable-autocmd)

;; Major modes
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
(put 'suspend-frame 'disabled t)
(setq tramp-default-method "ssh")
(setq doc-view-continuous t)
(setq use-short-answers t)
