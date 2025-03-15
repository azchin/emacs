(define-minor-mode colemak-dh-mode
  "A minor mode for Colemak DH key bindings."
  :lighter " Colemak-DH"
  :keymap (make-sparse-keymap))
  
(define-globalized-minor-mode global-colemak-dh-mode
  colemak-dh-mode
  (lambda () (colemak-dh-mode 1)))

; (global-colemak-dh-mode 1)

(evil-define-key '(normal visual motion) colemak-dh-mode-map
  (kbd "n") 'evil-backward-char
  (kbd "e") 'evil-next-line
  (kbd "i") 'evil-previous-line
  (kbd "o") 'evil-forward-char
  (kbd "j") 'evil-yank
  (kbd "h") 'evil-forward-word-end
  (kbd "H") 'evil-forward-WORD-end
  )
(evil-define-key 'normal colemak-dh-mode-map (kbd "l") 'evil-undo)
(evil-define-key 'normal colemak-dh-mode-map (kbd "u") 'evil-insert)
(evil-define-key 'normal colemak-dh-mode-map (kbd "y") 'evil-open-below)
(evil-define-key 'normal colemak-dh-mode-map (kbd "k") 'evil-ex-search-next)
(evil-define-key 'normal colemak-dh-mode-map (kbd "K") 'evil-ex-search-previous)
(evil-define-key 'operator colemak-dh-mode-map (kbd "i") nil)
  
;; (define-key evil-operator-state-map "a" evil-outer-text-objects-map)
;; (define-key evil-operator-state-map "i" evil-inner-text-objects-map)

(evil-define-key 'operator colemak-dh-mode-map (kbd "y") evil-inner-text-objects-map)

(evil-define-key leader-states colemak-dh-mode-map (kbd "<leader> w n") 'evil-window-left)
(evil-define-key leader-states colemak-dh-mode-map (kbd "<leader> w e") 'evil-window-down)
(evil-define-key leader-states colemak-dh-mode-map (kbd "<leader> w i") 'evil-window-up)
(evil-define-key leader-states colemak-dh-mode-map (kbd "<leader> w o") 'evil-window-right)
(evil-define-key leader-states colemak-dh-mode-map (kbd "<leader> w y") 'delete-other-windows)

(provide 'my-colemak-dh)
