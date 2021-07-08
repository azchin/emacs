(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
                                        ; (defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
                                        ; (defvar org-elpa '("org" . "http://orgmode.org/elpa/"))
(setq package-archives nil)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
                                        ; (add-to-list 'package-archives melpa-stable t)
                                        ; (add-to-list 'package-archives org-elpa t)

(when (< emacs-major-version 27)
  (package-initialize))
(unless (or package-archive-contents (not daemon-mode-snapshot))
  (package-refresh-contents))
(unless (require 'use-package nil 'noerror)
  (package-install 'use-package))
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

(use-package magit)

(use-package org
 :config
 (eload "core/org.el"))

;; (use-package undo-tree
;;   :custom
;;   ; (undo-tree-visualizer-diff t)
;;   ; (undo-tree-visualizer-timestamps t)
;;   ; (undo-tree-visualizer-relative-timestamps t)
;;   (undo-tree-auto-save-history t)
;;   (undo-tree-history-directory-alist `(("." . ,(emacsd "cache/undotree"))))
;;   :config
;;   (global-undo-tree-mode 1))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "●" "○" "●" "○" "●" "○")))

(use-package undo-fu
  :custom
  (undo-fu-ignore-keyboard-quit t))

(use-package undo-fu-session
  :after (undo-fu)
  :custom
  (undo-fu-session-directory (emacsd "cache/backups"))
  :config
  (global-undo-fu-session-mode))

(use-package evil
  :after (undo-fu)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete nil)
  (evil-want-C-d-scroll t)
  (evil-want-C-w-delete t)
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-move-beyond-eol nil)
  (evil-respect-visual-line-mode nil)
  ;; (evil-undo-system 'undo-tree)
  (evil-undo-system 'undo-fu)
  (evil-want-change-word-to-end nil)
  ;; (evil-search-module 'isearch)
  (evil-search-module 'evil-search)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  ;; :config
  ;; (global-undo-tree-mode)
  :config
  (eload "leader.el")
  (evil-mode 1)) ;; enable evil-mode in evil-leader

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; (use-package evil-snipe
;;   :after evil
;;   :custom
;;   (evil-snipe-scope 'buffer)
;;   (evil-snipe-use-vim-sneak-bindings t)
;;   :config
;;   (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
;;   (evil-snipe-mode 1))
;;   ; (evil-snipe-override-mode 1) ; this overrides f F t T

(use-package evil-quickscope
  :after evil
  :config
  ;; (set-face-foreground 'evil-quickscope-first-face "#FBFF00")
  ;; (set-face-foreground 'evil-quickscope-second-face "#AE57FF")
  (global-evil-quickscope-mode 1))

(use-package evil-collection
  :after (magit evil)
  :custom
  (evil-magit-use-y-for-yank t)
  :config
  (add-hook 'magit-mode-hook (lambda () (evil-snipe-local-mode 0)))
  (evil-collection-init))

(use-package evil-org
  :after (org evil) 
  :custom
  (evil-org-special-o/O '(table-row item))
  (org-edit-src-content-indentation 2)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-todo-keywords '((sequence "TODO" "PROG" "|" "DONE" "AXED")))
  :config
  (require 'evil-org-agenda)
  (require 'org-tempo)
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
  (sp-pair "\\\\(" nil :actions :rem)
  (sp-pair "\\{" nil :actions :rem)
  (sp-pair "\\(" nil :actions :rem)
  (sp-pair "\\\"" nil :actions :rem)
  (sp-pair "[" nil :actions :rem)
  ;; (sp-pair "(" nil :actions :rem)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  ;; (sp-pair "\"" nil :actions :rem)
  ;; (sp-local-pair 'emacs-lisp-mode "(" ")")
  (smartparens-strict-mode))

;; (use-package anzu
;;   :config
;;   (global-anzu-mode +1))

;; (use-package evil-smartparens
;;   :config
;;   (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

;; (use-package monokai-pro-theme
;;   :config
;;   (load-theme 'monokai-pro t))

(use-package gruvbox-theme
  :config
  ;; (load-theme 'gruvbox-dark-hard t)
  ;; (load-theme 'gruvbox-dark-medium t)
  ;; (load-theme 'gruvbox-light-hard t)
  )

(use-package modus-themes
  :config
  (load-theme 'modus-operandi t)
  )

;; (use-package haskell-mode)
(use-package markdown-mode)

;; (use-package pandoc-mode)

(use-package flycheck
  :config
  (add-hook 'org-mode-hook 'flyspell-mode))

(use-package rust-mode
  :after smartparens
  :config
  (sp-local-pair 'rust-mode "'" nil :actions :rem)
  (sp-local-pair 'rust-mode "<" nil :actions :rem))
;; (use-package rustic
;;   :after flycheck smartparens
;;   :config
;;   (sp-local-pair 'rustic-mode "'" nil :actions :rem)
;;   (sp-local-pair 'rustic-mode "<" nil :actions :rem))
(use-package js2-mode)
(use-package json-mode)

(use-package pdf-tools
  :config 
  ;; (add-hook 'pdf-view-mode 'auto-revert-mode)
  (add-hook 'pdf-view-mode 'pdf-view-midnight-minor-mode)
  (pdf-tools-install))

;; ; https://www.reddit.com/r/emacs/comments/cd6fe2/how_to_make_emacs_a_latex_ide/
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
  ;; (TeX-engine 'xetex)
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda () (set-face-foreground 'font-latex-script-char-face "#9aedfe")))

  (add-hook 'TeX-after-compilation-finished-functions 
            #'TeX-revert-document-buffer)
                                        ; (add-hook 'LaTeX-mode-hook
                                        ;           (lambda () (reftex-mode t) (flyspell-mode t)))
  )

;; TODO evil-tex

(use-package midnight
  :custom
  (clean-buffer-list-delay-special 0)
  (clean-buffer-list-delay-general 1)
  ;; (clean-buffer-list-timer (run-at-time t 3600 'clean-buffer-list))
  (clean-buffer-list-kill-regexps '("^.*$"))
  (clean-buffer-list-kill-never-buffer-names
   '("*scratch*" "*Messages*" "*cmd*"))
  (clean-buffer-list-kill-never-regexps
   '("^\\ .*$" "\\*.*scratch\\*" "\\` \\*Minibuf-.*\\*\\'" "^\\*EMMS Playlist\\*.*$" "^[A-Za-z].*[A-Za-z]$"))
  :config
  (run-at-time t 1800 'clean-buffer-list))

(require 'dired-x)

(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package yasnippet)

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
  (ivy-use-selectable-prompt t)
  ;; (ivy-initial-inputs-alist nil)
  :config
  ;; (add-to-list 'ivy-initial-inputs-alist '(counsel-find-file . "^"))
  (add-to-list 'ivy-initial-inputs-alist '(counsel-minor . ""))
  ;; (add-hook 'ebuild-mode-hook
  ;;           (lambda () (setq-local completing-read-function #'completing-read-default)))
  ;; (add-to-list 'ivy-completing-read-handlers-alist
  ;;              '(ebuild-mode-insert-skeleton . completing-read-default))
  (ivy-mode 1))

(use-package ivy-hydra)
(use-package swiper
  :after (evil)
  :config
  (evil-global-set-key 'normal (kbd "s") 'swiper)
  (evil-global-set-key 'normal (kbd "S") 'swiper-backward)
  (global-set-key (kbd "C-s") 'swiper))
(use-package counsel
  :config
  (counsel-mode 1))

(use-package company
  :after (evil)
  :custom
  (company-idle-delay 0.0)
  ;; (company-idle-delay nil)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-selection-wrap-around t)
  (company-backends
   '((company-semantic company-capf company-files company-etags company-keywords
                       company-dabbrev company-dabbrev-code company-cmake)))
  :config
  ;; Company start
  (defun company-backspace ()
    (interactive)
    (if (equal company-selection-changed nil)
        (if tab-control-auto (backward-delete-char-untabify 1)
          (backspace-whitespace-to-tab-stop))
      (company-abort)))

  (defun company-select-next-or-complete-selection (&optional arg)
    "Insert selection if appropriate, or select the next candidate."
    (interactive)
    (if (not (company-tooltip-visible-p)) (company-manual-begin))
    (cond ((> company-candidates-length 1) (company-select-next arg))
          ((equal company-candidates-length 1) (company-finish (car company-candidates)))))

  (defun company-select-previous-or-complete-selection ()
    "Insert selection if appropriate, or select the previous candidate."
    (interactive)
    (company-select-next-or-complete-selection -1))

  (define-key company-active-map (kbd "<backspace>") 'company-backspace)
  (define-key company-active-map (kbd "C-h") nil)

  (evil-define-key 'insert company-mode-map (kbd "C-n")
    'company-select-next-or-complete-selection)
  (evil-define-key 'insert company-mode-map (kbd "C-p")
    'company-select-previous-or-complete-selection)
  (evil-define-key 'insert company-active-map (kbd "C-n")
    'company-select-next)
  (evil-define-key 'insert company-active-map (kbd "C-p")
    'company-select-previous)
  (evil-define-key 'insert company-active-map (kbd "ESC")
    (lambda () (interactive) (company-abort)(evil-normal-state)))
  ;; Company end

  (defun add-to-company-backends (list)
    (setq company-backends `(,(append list (car company-backends)))))
  ;; (global-company-mode)
  (add-hook 'sh-mode-hook 'company-mode)
  (add-hook 'conf-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rustic-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'company-mode))

;; (use-package company-c-headers
;;   :after company
;;   :config
;;   (add-to-company-backends '(company-c-headers)))

;; (use-package company-shell
;;   :after company
;;   :custom
;;   (add-to-company-backends '(company-shell company-shell-env)))

;; (use-package company-auctex
;;   :after company
;;   :config
;;   (add-to-company-backends '(company-auctex))
;;   (company-auctex-init))

(use-package spinner
  :pin gnu)

(use-package lsp-mode
  :after (spinner)
  :custom
  (lsp-restart 'ignore)
  (lsp-completion-show-detail nil)
  :config
  (add-hook 'rust-mode-hook 'lsp))

;; (use-package lsp-ivy)
;; (use-package ccls)
;; (use-package lsp-latex)
;; (use-package lsp-haskell)
;; (use-package lsp-python-ms
;;   :custom
;;   (lsp-python-ms-auto-install-server t))

(use-package which-key)

(use-package all-the-icons
  :config
  (unless (member "all-the-icons" (font-family-list))))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :after (all-the-icons)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-modal-icon nil)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-irc nil)
  (doom-modeline-height 22)
  (doom-modeline-buffer-encoding nil))

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-set-bar 'over)
;;   :custom
;;   (centaur-tabs-change-fonts "Fira Code" 100)
;;   (define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
;;   (define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

;; Manually cloned
;; TODO periodically run "git pull" using midnight (or a cron)
(eload "clone/evil-unimpaired/evil-unimpaired.el")
(require 'evil-unimpaired)
(evil-unimpaired-mode)
