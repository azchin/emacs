(auto-insert-mode 1)
(setq auto-insert-query nil)
(setq auto-insert-directory (emacsd "insert"))

(define-auto-insert 'sh-mode "shell")
(define-auto-insert "\\.\\(cc\\|cpp\\)" "cplusplus")
