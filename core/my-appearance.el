;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
(menu-bar-mode 0)
(tool-bar-mode 0)
(when window-system
  (scroll-bar-mode 0))
(setq scroll-bar-adjust-thumb-portion nil)
(blink-cursor-mode 0)
(unless window-system
  (xterm-mouse-mode))
(setopt frame-resize-pixelwise t)
(setopt frame-inhibit-implied-resize t)
(setq scroll-conservatively 128)
;; (setq mouse-wheel-progressive-speed t)
;; (setq auto-window-vscroll nil)
;; (setq mouse-wheel-scroll-amount
;;       '(0.02 ((shift) . hscroll) ((meta))
;;           ((control meta) . global-text-scale)
;;           ((control) . text-scale)))

(pixel-scroll-precision-mode)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq pixel-scroll-precision-large-scroll-height 64.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq lazy-highlight-buffer-max-at-a-time nil)
(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-interval 0)
(setq lazy-highlight-cleanup nil)
(setq isearch-lazy-count t)

(setq visible-cursor nil)
(setq ring-bell-function 'ignore)
(setq text-scale-mode-step 1.1)

(keymap-global-set "C-+" 'text-scale-increase)
(keymap-global-set "C-_" 'text-scale-decrease)
(keymap-global-set "C-)" (lambda () (interactive) (text-scale-set 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab bar
(setq tab-bar-show 1)
(setq tab-bar-close-button-show nil)
(setq tab-bar-close-last-tab-choice 'delete-frame)
(setq tab-bar-new-button-show nil)
(setq tab-bar-new-tab-to 'right)
(setq tab-bar-tab-hints t)
(setq-default tab-bar-tab-name-function (lambda () (concat 
                                                    (when (buffer-modified-p) "+")
                                                    (tab-bar-tab-name-current))))
(setq tab-bar-button-margin '(4 . 8))
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-auto-width-min '(80 8))
(setq tab-bar-auto-width-max '(220 20))
(add-hook 'prog-mode-hook 'tab-bar-history-mode)
;; (setq tab-bar-button-relief 32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window dimensions
;; (let ((win '(100 40 256 192)))
;;     (add-to-list 'initial-frame-alist `(width . ,(nth 0 win)))
;;     (add-to-list 'initial-frame-alist `(height . ,(nth 1 win)))
;;     ;; (add-to-list 'initial-frame-alist `(left . ,(nth 2 win)))
;;     ;; (add-to-list 'initial-frame-alist `(top . ,(nth 3 win)))
;;     (add-to-list 'default-frame-alist `(width . ,(nth 0 win)))
;;     (add-to-list 'default-frame-alist `(height . ,(nth 1 win)))
;;     ;; (add-to-list 'default-frame-alist `(left . ,(nth 2 win)))
;;     ;; (add-to-list 'default-frame-alist `(top . ,(nth 3 win)))
;;     )
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(cond
 ((string-equal (getenv "XDG_CURRENT_DESKTOP") "openbox") t)
 ((string-equal system-type "gnu/linux")
  (add-to-list 'default-frame-alist '(undecorated . t))))

;; (setq my-display-offset 50)
;; (setq my-display-gap 10)
;; (setq my-cursed-height-magic 100)
;; (setq my-width (- (/ (display-pixel-width) 2)
;;                   (* my-display-offset 2)
;;                   my-display-gap))
;; (setq my-height (- (display-pixel-height)
;;                    (* my-display-offset 2)
;;                    my-cursed-height-magic))

;; (add-to-list 'initial-frame-alist `(width text-pixels . ,my-width))
;; (add-to-list 'initial-frame-alist `(height text-pixels . ,my-height))
;; (add-to-list 'initial-frame-alist `(left . ,my-display-offset))
;; (add-to-list 'initial-frame-alist `(top . ,my-display-offset))
;; (setq my-frame-alist `((width text-pixels . ,my-width)
;;                        (height text-pixels . ,my-height)
;;                        (left . ,(- my-display-offset))
;;                        (top . ,my-display-offset)))
;; (make-frame my-frame-alist)

;; (setq my-frame-alist '((width . 0.45)
;;                        (height . 0.9)
;;                        (left . 0.95)
;;                        (top . 0.05)
;;                        (reuse . t)))
;; (make-frame my-frame-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
(defvar default-font-family "DejaVu Sans Mono"
  "Default face font family")
(defvar markup-font-family "DejaVu Sans"
  "Markup font family")
;; b4 font bug, we had 113 and 120, after 72, 80
(cond
 ((eq system-type 'darwin)
  (defvar default-font-height 140
    "Default face font height") ;; 10: 98, 11: 113, 12: 120
  (defvar markup-font-height 160
    "Markup font height")
  )
 (t
  (defvar default-font-height 113
    "Default face font height") ;; 10: 98, 11: 113, 12: 120
  (defvar markup-font-height 120
    "Markup font height")
  )
 )
(add-to-list 'default-frame-alist `(font . ,(concat default-font-family "-" (number-to-string (round default-font-height 10)))))
;; (add-hook 'org-mode-hook (lambda () (buffer-face-set :family markup-font-family :height markup-font-height)))
(add-hook 'org-mode-hook (lambda () (setq-local face-remapping-alist '((default variable-pitch)))))
;; (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-10"))

;; http://xahlee.info/emacs/emacs/emacs_set_font_zh.html
(defun my-configure-font ()
  (set-fontset-font t 'han (cond ((member "Noto Sans CJK SC" (font-family-list)) "Noto Sans CJK SC")))
  (set-fontset-font t 'kana (cond ((member "Noto Sans CJK JP" (font-family-list)) "Noto Sans CJK JP")))
  (set-fontset-font t 'hangul (cond ((member "Noto Sans CJK KR" (font-family-list)) "Noto Sans CJK KR"))))
(defun my-configure-font-server ()
  (my-configure-font)
  (remove-hook 'server-after-make-frame-hook 'my-configure-font-server))
(when window-system
  (add-hook 'server-after-make-frame-hook 'my-configure-font-server)
  (my-configure-font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-frame-parameter (selected-frame) 'alpha '(95 . 86))
(defun make-frame-transparent() (interactive) (set-frame-parameter (selected-frame) 'alpha 80))
(defun make-frame-opaque() (interactive) (set-frame-parameter (selected-frame) 'alpha 100))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 86))) ;; first(?) frame flickers
;; (add-to-list 'default-frame-alist '(alpha . 95))
;; (setq frame-title-format '("Emacs - %b [%m] %f"))
;; (setq frame-title-format `(,(user-login-name) "@" ,(system-name) "     " global-mode-string "     %f" ))
;; (setq frame-title-format `(,(system-name) " - %b [%m] %f"  ))
(setq frame-title-format '("%b :: %f"))

(setopt initial-buffer-choice t)
(setopt initial-major-mode 'org-mode)
(setopt initial-scratch-message
        (format "Welcome %s, to the AIxCC デスマーチ\n"
                (capitalize (user-login-name))))
(setopt inhibit-startup-screen t)
;; (fringe-mode 0)
(when window-system
  (fringe-mode '(0 . 1)))
;; (fringe-mode '(1 . 1))
(setq-default fringes-outside-margins t)
(setq-default truncate-partial-width-windows 100)
(setq-default truncate-lines nil)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(setq echo-keystrokes 0.2)

;; Column indicator
(setq-default fill-column 80)
(add-hook 'c-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'c++-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'rust-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'latex-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'markdown-mode-hook 'display-fill-column-indicator-mode)

;; (global-prettify-symbols-mode 1)
(setq font-latex-fontify-script nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line numbers
(global-display-line-numbers-mode)
;; (global-visual-line-mode)
(setq display-line-numbers-width-start 3)
(defun relative-lines() (interactive) (setq-local display-line-numbers 'relative))
(defun visual-lines() (interactive) (setq-local display-line-numbers 'visual))
(defun disable-lines() (interactive) (setq-local display-line-numbers nil))
(defun absolute-lines() (interactive) (setq-local display-line-numbers t))
(defun relative-lines-type() (setq display-line-numbers-type 'relative))
(defun visual-lines-type() (setq display-line-numbers-type 'visual))
(defun disable-lines-type() (setq display-line-numbers-type nil))
(defun absolute-lines-type() (setq display-line-numbers-type t))

(add-hook 'prog-mode-hook 'relative-lines-type)
(add-hook 'text-mode-hook 'relative-lines-type)
(add-hook 'org-mode-hook 'visual-lines-type)
(add-hook 'conf-mode-hook 'relative-lines-type)
(add-hook 'special-mode-hook 'relative-lines-type)
(add-hook 'dired-mode-hook 'relative-lines-type)
(add-hook 'pdf-view-mode-hook 'disable-lines-type)
(add-hook 'doc-view-mode-hook 'disable-lines-type)
(add-hook 'image-mode-hook 'disable-lines-type)
(add-hook 'osm-mode-hook 'disable-lines-type)
;; (add-hook 'org-mode-hook 'absolute-lines)
;; (add-hook 'minibuffer-inactive-mode-hook 'disable-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Margins
;; TODO make minor mode
(setq desired-display-width 100)

;; (defvar display-margin-mode-list '(org-mode))
(defvar display-margin-mode-list nil)

(defun calculate-display-margin (width)
  (let ((selected-width (window-total-width)))
    (if (> selected-width width)
        (round (/ (- selected-width width) 2)) 0)))

(defun get-desired-display-margin ()
  (let ((margin (calculate-display-margin desired-display-width)))
    (set-window-margins (selected-window) margin margin)))

(defun setup-display-margin ()
  (when (member major-mode display-margin-mode-list)
    (get-desired-display-margin)
    (add-hook 'window-configuration-change-hook 'get-desired-display-margin nil t)))

(defun reset-display-margin ()
  (when (member major-mode display-margin-mode-list)
    (remove-hook 'window-configuration-change-hook 'get-desired-display-margin t)
    (set-window-margins (selected-window) 0 0)))

(defun my-zero-display-margin ()
  (interactive)
  (set-window-margins (selected-window) 0 0))

(defvar margin-window-vsplit-commands
  '(evil-window-vsplit
    evil-window-vsplit-leader
    split-window-right
    split-window-with-margins
    evil-file-vsplit
    evil-buffer-vsplit))

(defun pre-split-margins ()
  (when (member this-command margin-window-vsplit-commands)
    (reset-display-margin)))

(defun post-split-margins ()
  (when (member this-command margin-window-vsplit-commands)
    (setup-display-margin)))

(dolist (minor-mode display-margin-mode-list)
  (add-hook (intern (concat (symbol-name minor-mode) "-hook"))
            'setup-display-margin 90))
(add-hook 'pre-command-hook 'pre-split-margins)
(add-hook 'post-command-hook 'post-split-margins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc 'disable-theme custom-enabled-themes))

(defadvice load-theme (after theme-dont-propagate activate)
  (load "my-late" nil t))

(defun set-faces-to-default-font-family (faces)
  (dolist (face faces)
    (set-face-attribute face nil
                        :family default-font-family)))

(setq modus-themes-intense-mouseovers t
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-deuteranopia t
      modus-themes-tabs-accented nil
      modus-themes-mixed-fonts t)

;; (load-theme 'modus-operandi) 
;; (load-theme 'modus-vivendi) 

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

(provide 'my-appearance)
