
(setq line-number-mode t)
(setq column-number-mode t)

; (defun line-percentage ()
;   (interactive)
;   ; (pcase-let ((`(,total ,before ,after) (page--count-lines-page)))
; 	(let ((linecount (page--count-lines-page)))
;     (format "%d%%" (/ (* (nth 1 linecount) 100) (car linecount)))))

; (setcar mode-line-position
;   '(:eval (line-percentage)))

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

;; old custom
; (setq-default mode-line-format
;  (list
;   " "
; 	; mode-line-mule-info
; 	mode-line-modified ; "%*"
; 	; mode-line-frame-identification
;   " "
; 	mode-line-buffer-identification ; "%b"
; 	" "
; 	"%m"
;   " "
; 	"(%l %C) "
; 	'(-3 "%p")
;   " "
; 	; (vc-mode vc-mode)
; 	"%e"
; 	))

; (setq-default mode-line-format 
;  (list "%e" 
; 	mode-line-client 
; 	mode-line-modified 
; 	mode-line-remote 
; 	mode-line-frame-identification 
; 	mode-line-buffer-identification 
; 	"  " 
; 	mode-line-position 
; 	evil-mode-line-tag
;   ; (vc-mode vc-mode)
;   "  " 
;   "%m" ))

; (setq-default mode-line-format
;   (list "-"
;    'mode-line-mule-info
;    'mode-line-modified
;    'mode-line-frame-identification
;    "%b--"
;    ;; Note that this is evaluated while making the list.
;    ;; It makes a mode line construct which is just a string.
;    (getenv "HOST")
;    ":"
;    'default-directory
;    "   "
;    'global-mode-string
;    "   %[("
;    '(:eval (mode-line-mode-name))
;    'mode-line-process
;    'minor-mode-alist
;    "%n"
;    ")%]--"
;    '(which-func-mode ("" which-func-format "--"))
;    '(line-number-mode "L%l--")
;    '(column-number-mode "C%c--")
;    '(-3 "%p")))

; ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
;  (vc-mode vc-mode)
;  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
