(require 'package)

(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(setq package-archives nil)
(add-to-list 'package-archives melpa t)
;; (defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives gnu t)

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
  :hook (auto-package-update-before . (lambda () (message "Updating packages...")))
  :config
  (auto-package-update-maybe))

;; (use-package magit)

(use-package org
  :ensure nil
  :config
  (eload "core/org.el"))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "●" "○" "●" "○" "●" "○")))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package hl-todo
  :custom
  (hl-todo-wrap-movement t)
  :hook
  (prog-mode . hl-todo-mode))

(use-package undo-fu
  :custom
  (undo-fu-ignore-keyboard-quit t))

(use-package undo-fu-session
  :after undo-fu
  :custom
  (undo-fu-session-directory (emacsd "cache/backups"))
  :config
  (global-undo-fu-session-mode))

(use-package evil
  :after (undo-fu)
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-u-delete nil
        evil-want-C-d-scroll t
        evil-want-C-w-delete nil
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-move-beyond-eol nil
        evil-respect-visual-line-mode nil
        evil-undo-system 'undo-fu
        evil-want-change-word-to-end nil
        evil-search-module 'evil-search
        evil-split-window-below nil
        evil-vsplit-window-right nil)
  :config
  (eload "leader.el")
  (evil-mode 1)
  )

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-quickscope
  :after evil
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map [mouse-2] 'dired-mouse-find-file))

(use-package evil-org
  :after (org evil) 
  :custom
  (evil-org-special-o/O '(table-row item))
  (evil-org-use-additional-insert nil)
  (org-edit-src-content-indentation 2)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-todo-keywords '((sequence "TODO" "PROG" "|" "DONE" "AXED")))
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . evil-org-set-key-theme)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; TODO replace with electric-pair? this does provide latex \begin{} \end{} pairs tho
;;      maybe snippets for latex instead
(use-package smartparens
  :after (org)
  :hook (prog-mode special-mode text-mode conf-mode)
  :config
  (require 'smartparens-config)
  ;; (sp-pair "\\\\(" nil :actions :rem)
  ;; (sp-pair "\\{" nil :actions :rem)
  ;; (sp-pair "\\(" nil :actions :rem)
  ;; (sp-pair "\\\"" nil :actions :rem)
  ;; (sp-pair "[" nil :actions :rem)
  ;; (sp-pair "(" nil :actions :rem)
  ;; (sp-pair "'" nil :actions :rem)
  ;; (sp-pair "`" nil :actions :rem)
  ;; (sp-pair "\"" nil :actions :rem)
  ;; (sp-local-pair 'emacs-lisp-mode "(" ")")
  (sp-local-pair 'org-mode "/" nil :actions :rem)
  (sp-local-pair 'org-mode "~" nil :actions :rem)
  (sp-local-pair 'org-mode "=" nil :actions :rem)
  (sp-local-pair 'org-mode "*" nil :actions :rem)
  (sp-local-pair 'org-mode "_" nil :actions :rem)
  (sp-local-pair 'org-mode "`" nil :actions :rem)
  (smartparens-strict-mode))

;; (use-package evil-smartparens
;;   :config
;;   (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

(use-package gruvbox-theme
  ;; :config
  ;; (load-theme 'gruvbox-dark-hard t)
  )

(use-package modus-themes
  :ensure nil
  :after (evil)
  :init
  (setq modus-themes-intense-mouseovers t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-deuteranopia t
        modus-themes-mixed-fonts t)
  ;; (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  )

(use-package openwith
  :custom
  (openwith-associations '(("\\.pdf\\'" "zathura" (file))))
  :config
  (openwith-mode t))

(use-package markdown-mode)

(use-package cmake-mode)

;; (use-package flycheck
;;   :config
;;   ;; (add-hook 'org-mode-hook 'flyspell-mode)
;;   (add-hook 'rust-mode 'flycheck-mode))

(use-package rust-mode
  :after smartparens
  :config
  (sp-local-pair 'rust-mode "'" nil :actions :rem))
;; (use-package rustic
;;   :after flycheck smartparens
;;   :custom
;; ;; (rustic-lsp-server 'rust-analyzer)
;;   :config
;;   (sp-local-pair 'rustic-mode "'" nil :actions :rem)
;;   (sp-local-pair 'rustic-mode "<" nil :actions :rem))

;; (use-package js2-mode
;;   :config
;;   (setq js-indent-level 2))
;; (use-package typescript-mode
;;   :custom
;;   (typescript-indent-level 2))
;; (use-package json-mode)

;; (use-package pdf-tools
;;   :config 
;;   ;; (add-hook 'pdf-view-mode 'auto-revert-mode)
;;   (add-hook 'pdf-view-mode 'pdf-view-midnight-minor-mode)
;;   (pdf-tools-install))

;; ; https://www.reddit.com/r/emacs/comments/cd6fe2/how_to_make_emacs_a_latex_ide/
;; (use-package tex
;;   :ensure auctex
;;   :after (evil pdf-tools)
;;   :init
;;   (setq TeX-source-correlate-mode t)
;;   (setq TeX-source-correlate-start-server t)
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
;;   (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
;;   ;; (setq TeX-engine 'xetex)
;;   :config
;;   (add-hook 'LaTeX-mode-hook
;;             (lambda () (set-face-foreground 'font-latex-script-char-face "#9aedfe")))

;;   (add-hook 'TeX-after-compilation-finished-functions 
;;             'TeX-revert-document-buffer)
;;   ;; (add-hook 'LaTeX-mode-hook
;;   ;;           (lambda () (reftex-mode t) (flyspell-mode t)))
;;   )

;; TODO evil-tex

(use-package midnight
  :ensure nil
  :custom
  (clean-buffer-list-delay-special 0)
  (clean-buffer-list-delay-general 1)
  ;; (clean-buffer-list-timer (run-at-time t 3600 'clean-buffer-list))
  (clean-buffer-list-kill-regexps '("^.*$"))
  (clean-buffer-list-kill-never-buffer-names
   '("*scratch*" "*Messages*" "*cmd*"))
  (clean-buffer-list-kill-never-regexps
   '("^\\ .*$" "\\*.*scratch\\*" "\\` \\*Minibuf-.*\\*\\'" "^\\*EMMS Playlist\\*.*$" "^[A-Za-z].*[A-Za-z]$" "^[A-Za-z].*[A-Za-z]<*[A-Za-z]>$"))
  :config
  (run-at-time t 1800 'clean-buffer-list))

(use-package dired-x
  :ensure nil)

(use-package dired-subtree
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package yasnippet)

(use-package ivy
  :init
  (setq ivy-re-builders-alist
   '((counsel-describe-variable . ivy--regex-ignore-order)
     (counsel-describe-function . ivy--regex-ignore-order)
     (counsel-describe-symbol . ivy--regex-ignore-order)
     (counsel-describe-face . ivy--regex-ignore-order)
     (counsel-descbinds . ivy--regex-ignore-order)
     (counsel-M-x . ivy--regex-ignore-order)
     (counsel-find-file . ivy--regex-fuzzy)
     (counsel-dired . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  :custom
  (ivy-count-format "")
  (ivy-height 16)
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
  (defvaralias 'swiper-history 'regexp-search-ring)
  (evil-global-set-key 'normal (kbd "s") 'swiper)
  ;; (evil-global-set-key 'normal (kbd "S") 'swiper-backward)
  ;; (evil-global-set-key 'normal (kbd "C-n") 'isearch-repeat-forward)
  ;; (evil-global-set-key 'normal (kbd "C-p") 'isearch-repeat-backward)
  (evil-global-set-key 'normal (kbd "C-s") 'swiper-isearch))
(use-package counsel
  :config
  (counsel-mode 1))

;; TODO fix backends
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
  :hook
  ((sh-mode conf-mode c-mode c++-mode rust-mode emacs-lisp-mode LaTeX-mode python-mode))
  (company-mode . company-tng-mode)
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

  (keymap-set company-active-map "<backspace>" 'company-backspace)
  (keymap-set company-active-map "C-h" nil)

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
  )

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

;; (use-package web-mode
;;   :custom
;;   (web-mode-code-indent-offset 2)
;;   (web-mode-markup-indent-offset 2)
;;   (web-mode-css-indent-offset 2)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.jsx")))
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.tsx"))))

(use-package eglot
  :ensure nil
  :hook
  (rust-mode . eglot-ensure)
  (eglot-managed-mode . company-mode)
  :custom
  (gc-cons-threshold 1600000)
  (read-process-output-max (* 1024 32))
  :config
  (add-to-list 'eglot-server-programs
               '(c-mode . ("ccls")))
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rustup" "run" "stable" "rust-analyzer"))))

;; (use-package lsp-mode
;;   :after (spinner)
;;   :init
;;   (setq lsp-restart 'ignore
;;         lsp-completion-show-detail nil
;;         lsp-completion-enable-additional-text-edit nil
;;         lsp-completion-show-label-description nil
;;         lsp-headerline-breadcrumb-enable nil
;;         lsp-lens-enable nil
;;         ;; lsp-enable-symbol-highlighting nil
;;         ;; lsp-eldoc-enable-hover nil
;;         ;; lsp-enable-xref nil
;;         ;; lsp-modeline-code-diagnostics-enable nil
;;         ;; lsp-enable-file-watchers nil
;;         lsp-modeline-code-actions-enable nil
;;         lsp-modeline-code-workspace-status-enable nil
;;         lsp-enable-snippet nil
;;         lsp-enable-indentation nil
;;         lsp-enable-on-type-formatting nil
;;         lsp-enable-text-document-color nil
;;         lsp-rust-server 'rust-analyzer ;; rust-analyzer vs rls
;;         lsp-rust-analyzer-completion-auto-self-enable nil
;;         lsp-rust-analyzer-completion-add-call-argument-snippets nil
;;         lsp-rust-analyzer-completion-add-call-parenthesis nil
;;         lsp-use-plists t
;;         gc-cons-threshold 3200000
;;         read-process-output-max (* 1024 1024)
;;         lsp-log-io nil)
;;   :config
;;   ;; (lsp-rust-analyzer-completion-auto-import-enable nil)
;;   ;; (lsp-rust-analyzer-completion-postfix-enable nil)
;;   ;; (delete '(".*\\.js$" . "javascript") lsp-language-id-configuration)
;;   ;; (delete '(".*\\.ts$" . "typescript") lsp-language-id-configuration)
;;   ;; (delete '(js-mode . "javascript") lsp-language-id-configuration)
;;   ;; (add-to-list 'lsp-language-id-configuration '(js-mode . "deno"))
;;   ;; (add-to-list 'lsp-language-id-configuration '(typescript-mode . "deno"))
;;   ;; (lsp-register-client
;;   ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("deno" "lsp"))
;;   ;;                   :activation-fn (lsp-activate-on "deno")
;;   ;;                   :server-id 'deno))
;;   ;; (add-hook 'js-mode-hook 'lsp)
;;   ;; (add-hook 'typescript-mode-hook 'lsp)
;;   (add-hook 'c-mode-hook 'lsp)
;;   (add-hook 'c++-mode-hook 'lsp)
;;   (add-hook 'rust-mode-hook 'lsp))

;; (use-package lsp-ivy)
;; (use-package lsp-treemacs
;;   :config
;;   (lsp-treemacs-sync-mode 1))
;; (use-package ccls)
;; (use-package lsp-latex)
;; (use-package lsp-haskell)
;; (use-package lsp-python-ms
;;   :custom
;;   (lsp-python-ms-auto-install-server t))

(use-package which-key)

;; Manually cloned
;; TODO periodically run "git pull" using midnight (or a cron)
(use-package evil-unimpaired
  :ensure nil
  :load-path "clone/evil-unimpaired/"
  :after evil
  :config (evil-unimpaired-mode))
