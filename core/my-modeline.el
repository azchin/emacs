(setq line-number-mode t)
(setq column-number-mode t)

(defun mode-line-minor-mode-get (mode)
  (list mode (car (cdr (assoc mode minor-mode-alist)))))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))


(setq mode-line-whitespace " ")

(setq-default mode-line-position `(:eval ,(concat "(%l %C)" mode-line-whitespace "%p")))
(setq-default mode-line-buffer-identification '(:propertize "%b" face (:weight bold)))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left.
     (quote (""
             (:propertize evil-mode-line-tag face (:weight bold))
             mode-line-whitespace
             mode-line-client
             mode-line-remote
             mode-line-whitespace
             mode-line-modified
             mode-line-whitespace
             mode-line-buffer-identification
             mode-line-whitespace
             "["
             mode-name
             "]"
             mode-line-whitespace
             (:eval (when buffer-file-name
                      (let* ((s buffer-file-name)
                             (len (length s))
                             (max 20)
                             (ellipses "... "))
                        (format "[%s]"
                                (if (> len max)
                                    (concat ellipses (substring s (- len max (length ellipses))))
                                  s)))))
             ))
     ;; Right.
     (quote ((vc-mode vc-mode)
             (:eval (mode-line-minor-mode-get 'flymake-mode))
             (:eval (mode-line-minor-mode-get 'flyspell-mode))
             (display-battery-mode battery-mode-line-string)
             mode-line-whitespace
             mode-line-position
             mode-line-whitespace
             ))))))

(provide 'my-modeline)
