
(setq line-number-mode t)
(setq column-number-mode t)

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))


(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left.
     (quote (" "
	           mode-line-modified ; "%*"
             " "
             "%b" ; mode-line-buffer-identification
	           " "
	           "[%m]"))
     ;; Right.
     (quote ("%e (%l %C) %p "))))))

