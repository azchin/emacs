(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq visible-cursor nil)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9"))
(setq inhibit-startup-screen t)
(setq-default frame-title-format '("emacs %b"))
;; (fringe-mode 0)
(fringe-mode '(0 . 1))

;; Line numbers
(global-display-line-numbers-mode)
(defun enable-lines() (setq display-line-numbers-type 'relative))
(defun disable-lines() (setq display-line-numbers-type nil))

(add-hook 'prog-mode-hook 'enable-lines)
(add-hook 'text-mode-hook 'enable-lines)
(add-hook 'pdf-view-mode-hook 'disable-lines)
(add-hook 'image-mode-hook 'disable-lines)

(setq dired-listing-switches
      "-Ahlo --group-directories-first --time-style=iso")
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-hide-details-hide-information-lines t)
