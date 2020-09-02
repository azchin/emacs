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
  (add-hook 'auto-package-update-before-hook (lambda () (message "Updating packages...")))
  (auto-package-update-maybe))

(use-package undo-tree
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `(("." . ,(emacsd "cache/undotree"))))
  :config
  (global-undo-tree-mode 1))

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-C-w-scroll t)
  (evil-want-C-w-delete t)
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :config
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
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (evil-snipe-mode 1))
  ; (evil-snipe-override-mode 1) ; this overrides f F t T

(use-package evil-quickscope
  :requires evil
  :config
  ;; (set-face-foreground 'evil-quickscope-first-face "#FBFF00")
  ;; (set-face-foreground 'evil-quickscope-second-face "#AE57FF")
  (global-evil-quickscope-mode 1))

(use-package evil-collection
  :requires evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after (org evil) 
  :config
  (require 'org-tempo)
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook
            (lambda ()
              (evil-org-mode)
              ))
  (add-hook 'evil-org-mode (lambda () (evil-org-set-key-theme)))
  (evil-org-agenda-set-keys))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'special-mode-hook #'smartparens-mode)
  (add-hook 'text-mode-hook #'smartparens-mode)
  (smartparens-strict-mode))

(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

;; (use-package monokai-pro-theme
;;   :config
;;   (load-theme 'monokai-pro t))

(use-package gruvbox-theme
 :config
 (load-theme 'gruvbox-dark-hard t)
 ;; (load-theme 'gruvbox-dark-medium t)
 ;; (load-theme 'gruvbox-light-soft t)
 )

;; (use-package modus-operandi-theme
;;   :config
;;   (load-theme 'modus-operandi t))
;; (use-package modus-vivendi-theme
;;   :config
;;   (load-theme 'modus-vivendi t))

(use-package haskell-mode)
(use-package markdown-mode)

(use-package minimap
  :custom
  (minimap-window-location 'right)
  (minimap-update-delay 0)
  (minimap-width-fraction 0.08)
  (minimap-minimum-width 15))

(use-package pdf-tools
  :config 
  ;; (add-hook 'pdf-view-mode 'auto-revert-mode)
  (add-hook 'pdf-view-mode 'pdf-view-midnight-minor-mode)
  (pdf-tools-install))

; https://www.reddit.com/r/emacs/comments/cd6fe2/how_to_make_emacs_a_latex_ide/
(use-package tex
  :ensure auctex
  :after (evil pdf-tools)
  :custom
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  :config
  (add-hook 'LaTeX-mode-hook
   (lambda () (set-face-foreground 'font-latex-script-char-face "#9aedfe")))

  (add-hook 'TeX-after-compilation-finished-functions 
            #'TeX-revert-document-buffer)
  ; (add-hook 'LaTeX-mode-hook
  ;           (lambda () (reftex-mode t) (flyspell-mode t)))
  )


(use-package midnight
  :custom
  (clean-buffer-list-delay-special 600)
  (clean-buffer-list-timer (run-at-time t 3600 'clean-buffer-list))
  (clean-buffer-list-kill-regexps '("^.*$"))
  (clean-buffer-list-kill-never-buffer-names
   '("*scratch*" "*Messages*" "*cmd*" "*eshell*"))
  (clean-buffer-list-kill-never-regexps
   '("\\*.*scratch\\*" "\\` \\*Minibuf-.*\\*\\'" "^\\*EMMS Playlist\\*.*$")))

(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package dired-x)

;; (use-package fcitx
;;   :custom
;;   (fcitx-use-dbus t)
;;   :config
;;   (fcitx-default-setup)
;;   (fcitx-prefix-keys-add "C-x" "C-c" "C-h" "M-s" "M-o")
;;   (fcitx-prefix-keys-turn-on))


(use-package ivy
  :custom
  (ivy-count-format "")
  (ivy-height 16)
  (ivy-re-builders-alist
   '((counsel-describe-variable . ivy--regex-ignore-order)
     (counsel-describe-function . ivy--regex-ignore-order)
     (counsel-describe-symbol . ivy--regex-ignore-order)
     (counsel-describe-face . ivy--regex-ignore-order)
     (counsel-descbinds . ivy--regex-ignore-order)
     (counsel-M-x . ivy--regex-ignore-order)
     (counsel-find-file . ivy--regex-plus)
     (counsel-dired . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  ;; (ivy-initial-inputs-alist nil)
  :config
  ;; (add-to-list 'ivy-initial-inputs-alist '(counsel-find-file . "^"))
  (add-to-list 'ivy-initial-inputs-alist '(counsel-minor . ""))
  (ivy-mode 1))

(use-package magit)
(use-package evil-magit
  :after (magit evil)
  :custom
  (evil-magit-use-y-for-yank t)
  :config
  (add-hook 'magit-mode-hook (lambda () (evil-snipe-local-mode 0))))
(use-package ivy-hydra)
(use-package swiper)
(use-package counsel
  :config
  (counsel-mode 1))

(use-package company
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-selection-wrap-around t)
  (company-backends
   '((company-semantic company-capf company-files company-etags company-keywords
                      company-dabbrev company-dabbrev-code company-cmake)))
  :config
  (defun add-to-company-backends (list)
    (setq company-backends `(,(append list (car company-backends)))))
  (global-company-mode))
(use-package company-c-headers
  :after company
  :config
  (add-to-company-backends '(company-c-headers)))
(use-package company-shell
  :after company
  :custom
  (add-to-company-backends '(company-shell company-shell-env)))
(use-package company-auctex
  :after company
  :config
  (add-to-company-backends '(company-auctex))
  (company-auctex-init))

;; (use-package lsp-mode
;;   :hook
;;   ((prog-mode-hook . lsp)
;;    (text-mode-hook . lsp)
;;    (special-mode-hook . lsp))
;;   :commands lsp)
;; (use-package lsp-ivy)
;; (use-package ccls)
;; (use-package lsp-latex)
;; (use-package lsp-haskell)
;; (use-package lsp-python-ms
;;   :custom
;;   (lsp-python-ms-auto-install-server t))

(eload "leader.el")

;; Manually cloned
;; TODO periodically run "git pull" using midnight (or a cron)
(eload "clone/evil-unimpaired/evil-unimpaired.el")
(require 'evil-unimpaired)
(evil-unimpaired-mode)
