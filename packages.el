(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(eval-when-compile
 (require 'use-package))
(setq use-package-always-ensure t)

;; MELPA use-package
(use-package auto-package-update
 :custom
 (auto-package-update-delete-old-versions t)
 (auto-package-update-hide-results t)
 :config
 (add-hook 'auto-package-update-before-hook
           (lambda () (message "Updating packages...")))
 (auto-package-update-maybe))

(use-package evil
 :custom
 (evil-shift-width custom-tab-width)
 (evil-want-C-u-scroll t)
 (evil-want-C-u-delete t)
 (evil-want-C-w-scroll t)
 (evil-want-C-w-delete t)
 (evil-want-C-i-jump t)
 (evil-want-Y-yank-to-eol t)
 (evil-want-integration t)
 (evil-want-keybinding nil)
 :config
 (define-key evil-insert-state-map (kbd "<backspace>") 
             'backspace-whitespace-to-tab-stop)
 (evil-mode 1))

(use-package evil-surround
 :requires evil
 :config
 (global-evil-surround-mode 1))

(use-package evil-commentary
 :requires evil
 :config
 (evil-commentary-mode))

(use-package evil-snipe
 :requires evil
 :custom
 (evil-snipe-scope 'buffer)
 (evil-snipe-use-vim-sneak-bindings t)
 :config
 (evil-snipe-mode 1)
 ; (evil-snipe-override-mode 1) ; this overrides f F t T
 (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-quickscope
 :requires evil
 :config
 (global-evil-quickscope-mode 1)
 (set-face-foreground 'evil-quickscope-first-face "#FBFF00")
 (set-face-foreground 'evil-quickscope-second-face "#AE57FF"))

(use-package evil-collection
  :requires evil
  :config
  (evil-collection-init))

(use-package smartparens
 :config
 (require 'smartparens-config)
 (add-hook 'prog-mode-hook #'smartparens-mode)
 (add-hook 'text-mode-hook #'smartparens-mode))

; (use-package doom-themes
;  :config
;  ; (load-theme 'doom-monokai-spectrum t))
;  ; (load-theme 'doom-monokai-pro t))
;  (load-theme 'doom-one t))

(use-package monokai-pro-theme
 :config
 (load-theme 'monokai-pro t))

;; (use-package gruvbox-theme
;;  :config
;;  (load-theme 'gruvbox t))

(use-package haskell-mode)

(use-package minimap
 :custom
 (minimap-window-location 'right)
 (minimap-update-delay 0)
 (minimap-width-fraction 0.08)
 (minimap-minimum-width 15))

(use-package pdf-tools
 :config 
 (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
 (pdf-tools-install))

; (use-package latex-preview-pane)
 ; :config
 ; (latex-preview-pane-enable))

; https://www.reddit.com/r/emacs/comments/cd6fe2/how_to_make_emacs_a_latex_ide/
(use-package tex
 :ensure auctex
 :custom
 (TeX-auto-save t)
 (TeX-parse-self t)
 :config
 ; (add-hook 'TeX-after-compilation-finished-functions 
 ;           #'TeX-revert-document-buffer)
 ; (add-hook 'LaTeX-mode-hook
 ;  				 (lambda () (reftex-mode t) (flyspell-mode t)))
)

(use-package midnight
 :custom
 (clean-buffer-list-delay-special 600)
 (clean-buffer-list-timer (run-at-time t 3600 'clean-buffer-list))
 (clean-buffer-list-kill-regexps '("^.*$"))
 (clean-buffer-list-kill-never-buffer-names 
	'("*scratch*" "*Messages*" "*cmd*" "*eshell*"))
 (clean-buffer-list-kill-never-regexps 
	'("\\` \\*Minibuf-.*\\*\\'" "^\\*EMMS Playlist\\*.*$")))

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(load "~/.emacs.d/evil/leader.el")


;; Manually cloned
;; TODO periodically run "git pull" using midnight (or a cron)
(load "~/.emacs.d/clone/evil-unimpaired/evil-unimpaired.el")
(require 'evil-unimpaired)
(evil-unimpaired-mode)
