(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

(setq lazy-highlight-buffer-max-at-a-time nil)
(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-interval 0)
(setq lazy-highlight-cleanup nil)
(setq isearch-lazy-count t)

(setq visible-cursor nil)
(setq ring-bell-function 'ignore)

(setq tab-bar-show '1)
(setq tab-bar-close-button-show nil)
(setq tab-bar-close-last-tab-choice 'delete-frame)
(setq tab-bar-new-button-show nil)
(setq tab-bar-new-tab-to 'right)
(setq-default tab-bar-tab-name-function (lambda () (concat 
                                               " "
                                               (when (buffer-modified-p) "+ ")
                                               (tab-bar-tab-name-current)
                                               " ")))
(setq tab-bar-button-margin '(4 . 8))
;; (setq tab-bar-button-relief 32)

;; (let ((win '(128 40 256 192)))
;;     (add-to-list 'initial-frame-alist `(width . ,(nth 0 win)))
;;     (add-to-list 'initial-frame-alist `(height . ,(nth 1 win)))
;;     (add-to-list 'initial-frame-alist `(left . ,(nth 2 win)))
;;     (add-to-list 'initial-frame-alist `(top . ,(nth 3 win)))
;;     (add-to-list 'default-frame-alist `(width . ,(nth 0 win)))
;;     (add-to-list 'default-frame-alist `(height . ,(nth 1 win)))
;;     (add-to-list 'default-frame-alist `(left . ,(nth 2 win)))
;;     (add-to-list 'default-frame-alist `(top . ,(nth 3 win))))

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-10"))
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 86))
(defun make-frame-transparent() (interactive) (set-frame-parameter (selected-frame) 'alpha 80))
(defun make-frame-opaque() (interactive) (set-frame-parameter (selected-frame) 'alpha 100))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 86))) ;; first(?) frame flickers
;; (add-to-list 'default-frame-alist '(alpha . 95))
(setq inhibit-startup-screen t)
;; (setq frame-title-format '("Emacs - %b [%m] %f"))
;; (setq frame-title-format `(,(user-login-name) "@" ,(system-name) "     " global-mode-string "     %f" ))
;; (setq frame-title-format `(,(system-name) " - %b [%m] %f"  ))
(setq frame-title-format '("%b [%m]"))

(setq scroll-conservatively 128)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
"# This buffer is for text that is not saved.
# To create a file, visit it with \\[find-file] and enter text in its buffer.\n\n")
;; (fringe-mode 0)
(fringe-mode '(0 . 1))
(setq-default truncate-partial-width-windows nil)
(setq-default truncate-lines nil)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(setq echo-keystrokes 0.2)

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-width-start 3)
(defun enable-lines() (setq display-line-numbers-type 'relative))
(defun disable-lines() (setq display-line-numbers-type nil))
(defun absolute-lines() (setq display-line-numbers-type t))

(add-hook 'prog-mode-hook 'enable-lines)
(add-hook 'text-mode-hook 'enable-lines)
(add-hook 'special-mode-hook 'enable-lines)
(add-hook 'dired-mode-hook 'enable-lines)
(add-hook 'pdf-view-mode-hook 'disable-lines)
(add-hook 'image-mode-hook 'disable-lines)
;; (add-hook 'org-mode-hook 'absolute-lines)
;; (add-hook 'minibuffer-inactive-mode-hook 'disable-lines)

;; (setq dired-listing-switches "-Ahlo --group-directories-first --time-style='+%b %d %R'")
(setq dired-listing-switches "-Ahlo --group-directories-first --time-style=iso")
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-hide-details-hide-information-lines t)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(global-prettify-symbols-mode 1)
(setq font-latex-fontify-script nil)
