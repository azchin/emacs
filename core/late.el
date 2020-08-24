(global-hl-line-mode 1)
; (set-face-attribute hl-line-face nil 
;  :foreground nil
;  :background nil
;  :bold t)

(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    :foreground "#ff5c57"
                    :background (face-attribute 'default :background))

(set-face-italic 'font-lock-comment-face nil)

;; (custom-set-faces
;;  '(whitespace-tab ((t (:foreground "#636363")))))
;; (set-face-attribute 'whitespace-tab nil
;;                     :background nil
;;                     :foreground "#636363")
(set-face-background 'whitespace-tab nil)

;; TODO when new frames are made, highlight gets overridden
(defun change-highlight-face (&optional frame)
  (set-face-attribute 'highlight frame
                      :weight 'bold
                      :foreground nil ;; "#ffd866" ;; "#9aedfe" 
                      :background nil))

(add-to-list 'after-make-frame-functions 'change-highlight-face)
(change-highlight-face)
