
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


(setq mode-line-whitespace "  ")
;; (setq mode-line-whitespace '(:eval whitespace))

(setq-default mode-line-position `(:eval ,(concat "(%l %C)" mode-line-whitespace "%p")))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; Left.
     (quote (""
             evil-mode-line-tag
             ;; mode-line-modified ; "%*"
             mode-line-whitespace
             mode-line-client
             mode-line-remote
             mode-line-whitespace
             "%*%+"
             mode-line-whitespace
             "%b" ; mode-line-buffer-identification
             mode-line-whitespace
             "[%m]"
             ))
     ;; Right.
     (quote ("%e"
             mode-line-whitespace
             mode-line-position
             mode-line-whitespace
             ))))))

;; (setq-default mode-line-format
;;               (list
;;                evil-mode-line-tag
;;                mode-line-whitespace
;;                mode-line-client
;;                mode-line-remote
;;                mode-line-whitespace
;;                "%*%+"
;;                mode-line-whitespace
;;                "%b" ; mode-line-buffer-identification
;;                mode-line-whitespace
;;                "[%m]"
;;                mode-line-whitespace
;;                mode-line-position
;;                mode-line-whitespace
;;                "%e"
;;                )))))
