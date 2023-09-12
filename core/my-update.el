(require 'package)

(defvar my-update-file (emacsd "cache/my-update"))
(defvar my-update-days-interval 14)

(defun my-update-write-time ()
  (when (file-writable-p my-update-file)
    (with-temp-file my-update-file
      (insert (number-to-string (time-to-days (current-time)))))))

(defun my-update-read-time ()
  (if (file-exists-p my-update-file)
      (with-temp-buffer
        (insert-file-contents my-update-file)
        (string-to-number (buffer-string))) 0))

(defun can-retrieve-packages ()
  (cl-loop for url in (mapcar 'cdr package-archives)
           do (condition-case e
                  (kill-buffer (url-retrieve-synchronously url))
                (error (cl-return)))
           finally (cl-return t)))

(defun my-package-update ()
  (when (> (- (time-to-days (current-time)) (my-update-read-time))
           my-update-days-interval)
    (when (can-retrieve-packages)
      (package-upgrade-all)
      (my-update-write-time))))

(add-hook 'after-init-hook 'my-package-update)

(provide 'my-update)
