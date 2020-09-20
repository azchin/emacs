(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9"))
;; (add-to-list 'default-frame-alist '(font . "FuraCode Nerd Font-9"))
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 86))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 86))) ;; first(?) frame flickers
;; (add-to-list 'default-frame-alist '(alpha . 95))
(setq inhibit-startup-screen t)
(setq-default frame-title-format '("Emacs - %b [%m] %f"))

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
