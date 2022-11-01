(defun buffer-local-eww-browser-default ()
  (make-local-variable 'browse-url-browser-function)
  (setq browse-url-browser-function 'eww-browse-url))

(defun buffer-local-generic-browser-default ()
  (make-local-variable 'browse-url-browser-function)
  (setq browse-url-browser-function 'browse-url-generic))

(add-hook 'gnus-mode-hook 'buffer-local-generic-browser-default)
(add-hook 'eww-after-render-hook 'eww-readable)
(setq browse-url-generic-program "vimb")
