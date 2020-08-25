(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9"))
(setq inhibit-startup-screen t)
(setq-default frame-title-format '("emacs %b"))
(setq scroll-conservatively 128)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
"# This buffer is for text that is not saved.
# To create a file, visit it with \\[find-file] and enter text in its buffer.\n\n")
;; (fringe-mode 0)
(fringe-mode '(0 . 1))
(setq echo-keystrokes 0.2)

;; Line numbers
(global-display-line-numbers-mode)
(defun enable-lines() (setq display-line-numbers-type 'relative))
(defun disable-lines() (setq display-line-numbers-type nil))

(add-hook 'prog-mode-hook 'enable-lines)
(add-hook 'text-mode-hook 'enable-lines)
(add-hook 'special-mode-hook 'enable-lines)
(add-hook 'pdf-view-mode-hook 'disable-lines)
(add-hook 'image-mode-hook 'disable-lines)
;; (add-hook 'minibuffer-inactive-mode-hook 'disable-lines)

(setq dired-listing-switches
      "-Ahlo --group-directories-first --time-style=iso")
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-hide-details-hide-information-lines t)

(global-prettify-symbols-mode 1)
(setq font-latex-fontify-script nil)
