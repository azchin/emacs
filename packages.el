(require 'package)

(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(setq package-archives nil)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

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
  (evil-set-initial-state 'eshell-mode 'insert)
  (add-hook 'gnus-mode-hook 'turn-off-evil-mode)
  (add-hook 'newsticker-treeview-mode-hook 'turn-off-evil-mode)
  (evil-mode 1))

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
  (delete 'gnus evil-collection-mode-list)
  (delete 'newsticker evil-collection-mode-list)
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map [mouse-2] 'dired-mouse-find-file))

(use-package evil-org
  :after (org evil) 
  :custom
  (evil-org-special-o/O '(table-row item))
  (evil-org-use-additional-insert nil)
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . evil-org-set-key-theme)
  :config
  ;; (evil-define-minor-mode-key '(normal visual) 'evil-org-mode (kbd "H") 'org-shiftleft)
  ;; (evil-define-minor-mode-key '(normal visual) 'evil-org-mode (kbd "L") 'org-shiftright)
  (evil-define-key '(normal visual) org-mode-map (kbd "H") 'org-shiftleft)
  (evil-define-key '(normal visual) org-mode-map (kbd "L") 'org-shiftright)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package gruvbox-theme
  ;; :config
  ;; (load-theme 'gruvbox-dark-hard t)
  )

(setq modus-themes-intense-mouseovers t
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-deuteranopia t
      modus-themes-tabs-accented nil
      modus-themes-mixed-fonts t)
(load-theme 'modus-operandi) 

(use-package openwith
  :custom
  (openwith-associations '(("\\.pdf\\'" "zathura" (file))))
  :config
  (openwith-mode t))

(use-package markdown-mode)

(use-package cmake-mode)

(use-package rust-mode)

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
   '("^\\ .*$" "\\*.*scratch\\*" "\\` \\*Minibuf-.*\\*\\'"
     "^\\*EMMS Playlist\\*.*$" "^[A-Za-z].*[A-Za-z]$" "^\\*.*eshell\\*"
     "^[A-Za-z].*[A-Za-z]<*[A-Za-z]>$"))
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
  (add-to-list 'ivy-initial-inputs-alist '(counsel-minor . ""))
  (ivy-mode 1))

(use-package ivy-hydra)
(use-package swiper
  :after (evil)
  :config
  (defvaralias 'swiper-history 'regexp-search-ring)
  (evil-global-set-key 'normal (kbd "s") 'swiper)
  (evil-global-set-key 'normal (kbd "C-s") 'swiper-isearch))
(use-package counsel
  :config
  (counsel-mode 1))

;; TODO fix backends
(use-package company
  :after (evil)
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 2)
  (company-show-quick-access t)
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
        (backspace-whitespace-to-tab-stop)
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
    (lambda () (interactive) (company-abort) (evil-normal-state)))

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

(use-package which-key)

;; Manually cloned
;; TODO periodically run "git pull" using midnight (or a cron)
;; (use-package evil-unimpaired
;;   :ensure nil
;;   :load-path "clone/evil-unimpaired/"
;;   :after evil
;;   :config (evil-unimpaired-mode))
