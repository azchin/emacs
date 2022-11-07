(provide 'my-update)
(require 'package)

(defvar my-update-file (emacsd ".my-update"))
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

(defun my-package-update ()
  (when (> (- (time-to-days (current-time)) (my-update-read-time))
           my-update-days-interval)
    (package-update-all)
    (my-update-write-time)))

(add-hook 'after-init-hook 'my-package-update)
