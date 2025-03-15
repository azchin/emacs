(require 'evil)

(define-minor-mode colemak-dh-mode
  "A minor mode for Colemak DH key bindings."
  :lighter " Colemak-DH"
  :keymap (make-sparse-keymap))
  
(define-globalized-minor-mode global-colemak-dh-mode
  colemak-dh-mode
  (lambda () (colemak-dh-mode 1)))

;; (global-colemak-dh-mode 1)

;; TODO gj gk
(evil-define-key '(normal visual motion) 'colemak-dh-mode
  (kbd "n") 'evil-backward-char
  (kbd "e") 'evil-next-line
  (kbd "i") 'evil-previous-line
  (kbd "o") 'evil-forward-char

  (kbd "N") 'evil-window-top
  (kbd "E") 'evil-join
  (kbd "I") 'evil-lookup
  (kbd "O") 'evil-window-bottom

  (kbd "j") 'evil-yank
  (kbd "J") 'evil-yank-line

  (kbd "h") 'evil-forward-word-end
  (kbd "H") 'evil-forward-WORD-end
  )

(evil-define-key 'normal 'colemak-dh-mode
  (kbd "l") 'evil-undo
  (kbd "L") nil
  (kbd "u") 'evil-insert
  (kbd "U") 'evil-insert-line
  (kbd "y") 'evil-open-below
  (kbd "Y") 'evil-open-above
  (kbd "k") 'evil-ex-search-next
  (kbd "K") 'evil-ex-search-previous
  )

;; (evil-define-key 'normal colemak-dh-mode-map )
;; (evil-define-key 'normal colemak-dh-mode-map (kbd "u") 'evil-insert)
;; (evil-define-key 'normal colemak-dh-mode-map (kbd "y") 'evil-open-below)
;; (evil-define-key 'normal colemak-dh-mode-map (kbd "k") 'evil-ex-search-next)
;; (evil-define-key 'normal colemak-dh-mode-map (kbd "K") 'evil-ex-search-previous)

(evil-define-key 'operator 'colemak-dh-mode
  (kbd "i") 'evil-previous-line
  (kbd "u") evil-inner-text-objects-map
  )
(evil-define-key 'visual 'colemak-dh-mode
  (kbd "i") 'evil-previous-line
  (kbd "u") evil-inner-text-objects-map
  )
;; (evil-define-key 'visual 'colemak-dh-mode (kbd "i") nil)
  
;; (define-key evil-operator-state-map "a" evil-outer-text-objects-map)
;; (define-key evil-operator-state-map "i" 'evil-previous-line)

(evil-define-key leader-states 'colemak-dh-mode
  (kbd "<leader> w n") 'evil-window-left
  (kbd "<leader> w e") 'evil-window-down
  (kbd "<leader> w i") 'evil-window-up
  (kbd "<leader> w o") 'evil-window-right
  (kbd "<leader> w y") 'delete-other-windows
  )

(provide 'my-colemak-dh)
