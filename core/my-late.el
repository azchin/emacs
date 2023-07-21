(provide 'my-late)

(require 'org)
(global-hl-line-mode 1)
;; (set-face-attribute hl-line-face nil 
;;                     :foreground nil
;;                     :background (face-attribute 'default :background)
;;                     :bold t)

(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    ;; :foreground "#ff5c57"
                    ;; :background (face-attribute 'hl-line :background)
                    )

(set-face-italic 'font-lock-comment-face nil)

;; (set-face-attribute 'whitespace-tab nil
;;                     :background nil
;;                     :foreground "#636363")
;; (set-face-background 'whitespace-tab nil)

(set-face-attribute 'line-number nil
                    :family default-font-family
                    :inherit 'default)
(set-face-attribute 'line-number-current-line nil
                    :inherit '(bold line-number))

(set-face-attribute 'org-document-title nil :weight 'bold :height 1.5)
;; (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
;;                             org-level-6 org-level-7 org-level-8))
;;   (set-face-attribute face nil :weight 'bold))

(set-face-attribute 'org-document-title nil :family markup-font-family :weight 'bold :height 1.5)
(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.2)
                (org-level-3 . 1.1)
                (org-level-4 . 1.1)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :family markup-font-family :weight 'medium :height (cdr face)))
(set-face-attribute 'org-table nil :family default-font-family :height default-font-height)
(set-face-attribute 'org-checkbox nil :family default-font-family :height default-font-height)

(set-face-attribute 'default nil :family default-font-family :height default-font-height)
(set-face-attribute 'variable-pitch nil :family markup-font-family :height markup-font-height)
(set-face-attribute 'fixed-pitch nil :family default-font-family :height default-font-height)
;; (set-face-attribute 'org-block nil :family default-font-family :inherit 'default)
;; (set-face-attribute 'org-block-begin-line nil :inherit 'default)
;; (set-face-attribute 'org-block-end-line nil :inherit 'org-block-begin-line)
;; (set-face-attribute 'org-code nil :inherit 'default)
;; (set-face-attribute 'org-date nil :inherit '(modus-themes-link-symlink))

;; TODO when new frames are made, highlight gets overridden
(defun change-highlight-face (&optional frame)
  (set-face-attribute 'highlight frame
                      :weight 'bold
                      :foreground 'unspecified ;; "#ffd866" ;; "#9aedfe" 
                      :background 'unspecified))

(add-to-list 'after-make-frame-functions 'change-highlight-face)
(change-highlight-face)
