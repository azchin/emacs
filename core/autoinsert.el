(auto-insert-mode 1)
(setq auto-insert-query nil)
(setq auto-insert-directory (emacsd "insert"))

(define-auto-insert 'sh-mode "shell")
(define-auto-insert "\\.\\(cc\\|cpp\\)" "cplusplus")

;; Autocmd
(defun suckless-autocmd ()
  (when (and buffer-file-name (string-match "config.h" (buffer-name)))
     (shell-command "sudo make clean install")))
(add-hook 'after-save-hook 'suckless-autocmd)

(defun environment-autocmd ()
  (when (and (not (equal major-mode 'sh-mode)) (string-match "\\(profile\\|aliases\\)" (buffer-name)))
    (sh-mode)))
(add-hook 'buffer-list-update-hook 'environment-autocmd)

(defun executable-autocmd ()
  (when (string-match ".*\\.sh" (buffer-name))
    (shell-command (concat "[ $(stat -c '%a' " buffer-file-name ") = 755 ] || chmod 755 " buffer-file-name))))
(add-hook 'after-save-hook 'executable-autocmd)
