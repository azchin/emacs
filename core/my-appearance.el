(provide 'my-appearance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq scroll-bar-adjust-thumb-portion nil)
(blink-cursor-mode 0)
(setq frame-resize-pixelwise t)
(setq scroll-conservatively 128)
(setq mouse-wheel-progressive-speed nil)
;; (setq auto-window-vscroll nil)
;; (setq mouse-wheel-scroll-amount
;;       '(0.02 ((shift) . hscroll) ((meta))
;;           ((control meta) . global-text-scale)
;;           ((control) . text-scale)))
;; (pixel-scroll-precision-mode)
;; (setq pixel-scroll-precision-large-scroll-height 64.0)

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
(tab-bar-mode)
(setq tab-bar-show t)
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
(add-hook 'prog-mode-hook 'tab-bar-history-mode)
;; (setq tab-bar-button-relief 32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window dimensions
(let ((win '(100 40 256 192)))
    (add-to-list 'initial-frame-alist `(width . ,(nth 0 win)))
    (add-to-list 'initial-frame-alist `(height . ,(nth 1 win)))
    ;; (add-to-list 'initial-frame-alist `(left . ,(nth 2 win)))
    ;; (add-to-list 'initial-frame-alist `(top . ,(nth 3 win)))
    (add-to-list 'default-frame-alist `(width . ,(nth 0 win)))
    (add-to-list 'default-frame-alist `(height . ,(nth 1 win)))
    ;; (add-to-list 'default-frame-alist `(left . ,(nth 2 win)))
    ;; (add-to-list 'default-frame-alist `(top . ,(nth 3 win)))
    )
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
(defvar default-font-family "DejaVu Sans Mono"
  "Default face font family")
;; b4 font bug, we had 113 and 120, after 72, 80
(defvar default-font-height 113
  "Default face font height") ;; 10: 98, 11: 113, 12: 120
(defvar markup-font-family "DejaVu Sans"
  "Markup font family")
(defvar markup-font-height 120
  "Markup font height")
(add-to-list 'default-frame-alist `(font . ,(concat default-font-family "-" (number-to-string (round default-font-height 10)))))
(add-hook 'org-mode-hook (lambda () (buffer-face-set :family markup-font-family :height markup-font-height)))
;; (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-10"))

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

(setq initial-buffer-choice t)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
      "# This buffer is for text that is not saved.
# To create a file, visit it with \\[find-file] and enter text in its buffer.\n\n")
;; (fringe-mode 0)
;; (fringe-mode '(0 . 1))
(fringe-mode 0)
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

;; (global-prettify-symbols-mode 1)
(setq font-latex-fontify-script nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-width-start 3)
(defun relative-lines() (setq display-line-numbers-type 'visual))
(defun disable-lines() (setq display-line-numbers-type nil))
(defun absolute-lines() (setq display-line-numbers-type t))

(add-hook 'prog-mode-hook 'relative-lines)
(add-hook 'text-mode-hook 'relative-lines)
(add-hook 'conf-mode-hook 'relative-lines)
(add-hook 'special-mode-hook 'relative-lines)
(add-hook 'dired-mode-hook 'relative-lines)
(add-hook 'pdf-view-mode-hook 'disable-lines)
(add-hook 'doc-view-mode-hook 'disable-lines)
(add-hook 'image-mode-hook 'disable-lines)
;; (add-hook 'org-mode-hook 'absolute-lines)
;; (add-hook 'minibuffer-inactive-mode-hook 'disable-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Margins
;; TODO make minor mode
(setq desired-display-width 100)

(defvar display-margin-mode-list '(org-mode))

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
            'setup-display-margin))
(add-hook 'pre-command-hook 'pre-split-margins)
(add-hook 'post-command-hook 'post-split-margins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc 'disable-theme custom-enabled-themes))

(defadvice load-theme (after theme-dont-propagate activate)
  (load "my-late"))

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
(load-theme 'modus-operandi) 
;; (use-package dracula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'dracula t))
