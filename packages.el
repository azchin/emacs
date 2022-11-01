(require 'package)

(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(setq package-archives nil)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(when (< emacs-major-version 27)
  (package-initialize))
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
  :hook
  (auto-package-update-before . (lambda () (message "Updating packages...")))
  (auto-package-update-after . (lambda () (message "Packages updated...")))
  :config
  (auto-package-update-maybe))

;; (use-package magit)

(use-package dired
  :ensure nil)

(use-package org
  :ensure nil
  :config
  (eload "core/org.el"))

(use-package org-tempo
  :ensure nil
  :requires org)

(use-package hl-todo
  :custom
  (hl-todo-wrap-movement t)
  :hook
  (prog-mode . hl-todo-mode))

(use-package undo-fu
  :custom
  (undo-fu-ignore-keyboard-quit t))

(use-package undo-fu-session
  :requires undo-fu
  :custom
  (undo-fu-session-directory (emacsd "cache/backups"))
  :config
  (global-undo-fu-session-mode))

(use-package evil
  :requires (undo-fu dired org)
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-u-delete nil
        evil-want-C-d-scroll t
        evil-want-C-w-delete nil
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-kbd-macro-suppress-motion-error 'replay
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
  (evil-set-initial-state 'vc-annotate-mode 'insert)
  (evil-set-initial-state 'gnus-mode 'emacs)
  (evil-mode 1))

(use-package evil-surround
  :requires evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :requires evil
  :config
  (evil-commentary-mode))

(use-package evil-quickscope
  :requires evil
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-collection
  :requires evil
  :config
  ;; (delete 'gnus evil-collection-mode-list)
  ;; (delete 'newsticker evil-collection-mode-list)
  (evil-collection-init
   '(apropos auto-package-update bookmark calc calendar cmake-mode compile debug
             dictionary diff-mode dired dired-sidebar doc-view ediff eglot
             elisp-mode elisp-refs eshell eww go-mode grep help image
             image-dired imenu info ivy js2-mode log-edit log-view kotlin-mode
             org org-present org-roam outline-mode (pdf pdf-view) popup python
             replace simple typescript-mode vc-annotate vc-dir vc-git vdiff view
             which-key xref yaml-mode))
  (evil-collection-define-key 'normal 'dired-mode-map [mouse-2] 'dired-mouse-find-file))

(use-package evil-org
  :requires (org evil) 
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

;; (use-package openwith
;;   :custom
;;   (openwith-associations '(("\\.pdf\\'" "zathura" (file))))
;;   :config
;;   (openwith-mode t))

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
;;   :requires (evil pdf-tools)
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
     "^\\*Article.*\\*$" "^\\*Summary.*\\*$" "^\\*eww\\*$" "^\\*Group\\*$"
     "^[A-Za-z].*[A-Za-z]<*[A-Za-z]>$"))
  :config
  (run-at-time t 1800 'clean-buffer-list))

(use-package dired-x
  :ensure nil)

;; (use-package dired-subtree
;;   :requires dired
;;   :config
;;   (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
;;   (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

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
  :config
  (defvaralias 'swiper-history 'regexp-search-ring)
  (keymap-global-set "C-s" 'swiper-isearch))
(use-package counsel
  :config
  (counsel-mode 1))

;; TODO fix backends
(use-package company
  :requires (evil)
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 2)
  (company-show-quick-access t)
  (company-selection-wrap-around t)
  (company-tooltip-maximum-width 60)
  (company-backends
   '((company-capf company-clang company-cmake company-keywords :with company-dabbrev-code :separate)))
  (company-global-modes
   '(sh-mode conf-mode c-mode c++-mode rust-mode emacs-lisp-mode latex-mode
             python-mode org-mode eglot-managed-mode))
  :config
  ;; TODO :with yasnippet, abbrev
  (defun company-shell-mode-configure ()
    (setq-local company-backends
                '((company-capf company-keywords company-files :with company-dabbrev-code :separate))))
  (defun company-org-mode-configure ()
    (setq-local company-backends
                '((company-capf company-ispell :with company-dabbrev :separate))))
  (defun company-cmake-mode-configure ()
    (setq-local company-backends
                '((company-capf company-keywords company-cmake :with company-dabbrev-code :separate))))
  (add-hook 'sh-mode-hook 'company-shell-mode-configure)
  (add-hook 'conf-mode-hook 'company-shell-mode-configure)
  (add-hook 'org-mode-hook 'company-org-mode-configure)
  (add-hook 'cmake-mode-hook 'company-cmake-mode-configure)

  (defun set-company-faces-to-default-font-family ()
    (dolist (face '(company-tooltip company-tooltip-common
                                    company-tooltip-search
                                    company-tooltip-search-selection))
      (set-face-attribute face nil
                          :family default-font-family)))
  (add-hook 'company-mode-hook 'set-company-faces-to-default-font-family)
  
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
  (global-company-mode)
  (company-tng-mode))

;; (use-package company-c-headers
;;   :requires company
;;   :config
;;   (add-to-company-backends '(company-c-headers)))

;; (use-package company-shell
;;   :requires company
;;   :custom
;;   (add-to-company-backends '(company-shell company-shell-env)))

;; (use-package company-auctex
;;   :requires company
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
;;   :requires evil
;;   :config (evil-unimpaired-mode))
