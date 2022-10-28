
(setq line-number-mode t)
(setq column-number-mode t)

;; (defun mode-line-minor-mode-get (mode)
;;   (list mode `(:eval ,(car (cdr (assoc mode minor-mode-alist))))))

(defun mode-line-minor-mode-get (mode)
  (list mode (car (cdr (assoc mode minor-mode-alist)))))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (+ (window-width) (if (car (window-margins)) (+ (car (window-margins))
                                                            (cdr (window-margins))) 0))
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))


(setq mode-line-whitespace " ")
;; (setq mode-line-whitespace '(:eval whitespace))

(setq-default mode-line-position `(:eval ,(concat "(%l %C)" mode-line-whitespace "%p")))

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
             (:propertize "%b" face (:weight bold))
             mode-line-whitespace
             mode-name
             ))
     ;; Right.
     (quote ((vc-mode vc-mode)
             (:eval (mode-line-minor-mode-get 'flymake-mode))
             (:eval (mode-line-minor-mode-get 'flyspell-mode))
             mode-line-whitespace
             mode-line-position
             mode-line-whitespace
             ))))))

;; (setq-default mode-line-format
;;               (list
;   ;             (:propertize evil-mode-line-tag face (:weight bold))
;;                mode-line-client
;;                mode-line-remote
;;                " %*%+"
;;                '(:propertize " %b" face (:weight bold))
;;                " [%m]"
;;                '(vc-mode vc-mode)
;;                (mode-line-minor-mode-get 'flymake-mode)
;;                (mode-line-minor-mode-get 'flyspell-mode)
;;                " (%l %C) %p"
;;                ))
