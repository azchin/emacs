(global-hl-line-mode 1)
;; (set-face-attribute hl-line-face nil 
;;                     :foreground nil
;;                     :background (face-attribute 'default :background)
;;                     :bold t)

(show-paren-mode 1)
;; (set-face-attribute 'show-paren-match nil
;;                     :weight 'bold
;;                     :foreground "#ff5c57"
;;                     :background (face-attribute 'default :background))

(set-face-italic 'font-lock-comment-face nil)

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

;; (defun my-begin-backend (backends)
;;  (lambda ()
;;   (company-begin-backend backends)))

;; (defvar shell-mode-backend
;;   '(company-shell company-shell-env company-files))
;; (defvar c-mode-backend
;;   '(company-c-headers company-semantic company-cmake company-capf company-files))
;; (add-to-list 'company-backends shell-mode-backend)
;; (add-to-list 'company-backends c-mode-backend)
;; (add-hook 'shell-script-mode-hook (my-begin-backend shell-mode-backend))
;; (add-hook 'c-mode-hook (my-begin-backend c-mode-backend))
;; (add-hook 'c++-mode-hook (my-begin-backend c-mode-backend))
;; (add-hook 'c++-mode-hook 
;;   (lambda () (company-begin-backend 'company-c-headers)))

