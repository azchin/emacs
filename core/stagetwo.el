(provide 'stagetwo)

;; Supress warnings
(when (string-match "29\\.0\\.50" (version))
  (setq warning-minimum-level :error))

(require 'server)
(defvar daemon-mode-snapshot (and (boundp 'server-process)
                                  (processp server-process)
                                  (server-running-p)))
(cd "~")
(defvar home-dir default-directory)

(require 'package)
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(setq package-archives (list melpa gnu))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My package agnostic customizations
(require 'my-modeline)

(require 'my-tabs)

(require 'my-buffer)

(require 'my-desktop)

(require 'my-extra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in packages
(require 'dired)
(require 'dired-x)
(require 'dired-aux)
(setq dired-free-space nil)

(require 'org)
(require 'org-tempo)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-decorations
      '("\n " "" "\n " "" "[" "]" " [No match]" " [Matched]" " [Not readable]"
        " [Too big]" " [Confirm]" "\n " " >>"))
(setq ido-max-window-height 0.5)
(setq ido-max-prospects 25)
(setq ido-enable-last-directory-history nil)
(setq ido-enter-matching-directory 'first)

(keymap-unset ido-common-completion-map "C-s" t)
(keymap-unset ido-common-completion-map "C-r" t)
(keymap-unset ido-common-completion-map "M-n" t)
(keymap-set ido-common-completion-map "C-n" 'ido-next-match)
(keymap-set ido-common-completion-map "C-p" 'ido-prev-match)

(defun ido-complete-or-match (matchf)
  (let ((ido-cannot-complete-command matchf))
    (call-interactively 'ido-complete)))
(defun ido-complete-or-next ()
  (interactive)
  (ido-complete-or-match 'ido-next-match))
(defun ido-complete-or-prev ()
  (interactive)
  (ido-complete-or-match 'ido-prev-match))
(defun ido-select-text-or-complete ()
  (interactive)
  (if (and ido-current-directory (equal "" ido-text))
      (call-interactively 'ido-select-text)
    (call-interactively 'ido-exit-minibuffer)))

(keymap-set ido-common-completion-map "TAB" 'ido-complete-or-next)
(keymap-set ido-common-completion-map "<tab>" 'ido-complete-or-next)
(keymap-set ido-common-completion-map "<shift-tab>" 'ido-complete-or-prev)
(keymap-set ido-common-completion-map "<backtab>" 'ido-complete-or-prev)
(keymap-set ido-common-completion-map "SPC" 'ido-exit-minibuffer)
(keymap-set ido-common-completion-map "RET" 'ido-select-text-or-complete)

;; https://stackoverflow.com/questions/905338/can-i-use-ido-completing-read-instead-of-completing-read-everywhere
(defvar ido-enable-replace-completing-read nil
  "If t, use ido-completing-read instead of completing-read if possible.

Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:

(defadvice foo (around original-completing-read-only activate)
  (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
    (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

(defmacro ido-replace-completing-read-gen (fname)
  `(defun ,(intern (concat "ido-" (symbol-name fname))) ()
     (interactive)
     (let ((ido-enable-replace-completing-read t))
       (call-interactively (quote ,fname)))))

(ido-everywhere 1)
(ido-mode 1)

(require 'midnight)
(setq clean-buffer-list-delay-special 0)
(setq clean-buffer-list-delay-general 1)
;; (setq clean-buffer-list-timer (run-at-time t 3600 'clean-buffer-list))
(setq clean-buffer-list-kill-regexps '("^.*$"))
(setq clean-buffer-list-kill-never-buffer-names
      '("*scratch*" "*Messages*" "*cmd*"))
(setq clean-buffer-list-kill-never-regexps
      '("^\\ .*$" "\\*.*scratch\\*" "\\` \\*Minibuf-.*\\*\\'"
        "^\\*EMMS Playlist\\*.*$" "^[A-Za-z].*[A-Za-z]$" "^\\*.*eshell\\*"
        "^\\*Article.*\\*$" "^\\*Summary.*\\*$" "^\\*eww\\*$" "^\\*Group\\*$"
        "^[A-Za-z].*[A-Za-z]<*[A-Za-z]>$"))
(run-at-time t 1800 'clean-buffer-list)

;; Soft dependency on yasnippets and company
(require 'eglot)
(add-hook 'rust-mode-hook 'eglot-ensure)
(setq gc-cons-threshold 1600000)
(setq read-process-output-max (* 1024 32))
(add-to-list 'eglot-server-programs
             '(c-mode . ("ccls")))
(add-to-list 'eglot-server-programs
             '(rust-mode . ("rust-analyzer")))

(require 'gnus)
(setq gnus-select-method '(nnnil))
(add-hook 'kill-emacs-query-functions
          (lambda () (when (gnus-alive-p) (gnus-group-exit)) t))

(require 'eww)
(setq browse-url-generic-program "vimb")
(defun buffer-local-eww-browser-default ()
  (make-local-variable 'browse-url-browser-function)
  (setq browse-url-browser-function 'eww-browse-url))

(defun buffer-local-generic-browser-default ()
  (make-local-variable 'browse-url-browser-function)
  (setq browse-url-browser-function 'browse-url-generic))

(add-hook 'gnus-mode-hook 'buffer-local-generic-browser-default)
(add-hook 'eww-after-render-hook 'eww-readable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA packages
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :hook
  (auto-package-update-before . (lambda () (message "Updating packages...")))
  (auto-package-update-after . (lambda () (message "Packages updated...")))
  :config
  (auto-package-update-maybe))

(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-wrap-movement t)
  :hook
  (prog-mode . hl-todo-mode))

(use-package markdown-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package undo-fu
  :ensure t
  :custom
  (undo-fu-ignore-keyboard-quit t))

(use-package undo-fu-session
  :ensure t
  :requires undo-fu
  :custom
  (undo-fu-session-directory (emacsd "cache/backups"))
  :config
  (global-undo-fu-session-mode))

(use-package evil
  :ensure t
  :requires undo-fu
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
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'vc-annotate-mode 'insert)
  (evil-set-initial-state 'gnus-mode 'emacs)
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :requires evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :requires evil
  :config
  (evil-commentary-mode))

(use-package evil-quickscope
  :ensure t
  :requires evil
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-collection
  :ensure t
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

  (evil-define-key 'normal dired-mode-map "f" 'find-file)
  (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-file)
  (keymap-set dired-mode-map "<mouse-2>" 'dired-mouse-find-file)
  (defun dired-goto-subdir-and-focus ()
    (interactive)
    (call-interactively 'dired-maybe-insert-subdir)
    (recenter 1))
  (defun dired-kill-subdir-and-pop ()
    (interactive)
    (dired-kill-subdir)
    (set-mark-command 1)
    (recenter))
  (defun dired-kill-subdir-and-up ()
    (interactive)
    (set-mark-command nil)
    (condition-case nil
        (progn (dired-tree-up 1)
               (exchange-point-and-mark)
               (dired-kill-subdir)
               (set-mark-command 1)
               (recenter 0))
      (error (deactivate-mark))))
  (defun dired-kill-subdir-recurse (level)
    (interactive)
    (condition-case nil
        (progn (dired-tree-down) (dired-kill-subdir-recurse (+ level 1)))
      (error (condition-case nil
                 (unless (eq level 0)
                   (progn (dired-kill-subdir-and-up)
                          (dired-kill-subdir-recurse (- level 1))))
               (error nil)))))
  (defun dired-kill-children-subdir ()
    (interactive)
    (dired-kill-subdir-recurse 0)
    (dired-kill-subdir-and-up))
  (evil-define-key 'normal dired-mode-map (kbd "TAB") 'dired-goto-subdir-and-focus)
  (evil-define-key 'normal dired-mode-map (kbd "<backtab>") 'dired-kill-children-subdir)
  (evil-define-key 'normal dired-mode-map (kbd "<shift-tab>") 'dired-kill-children-subdir)
  (evil-define-key 'normal dired-mode-map (kbd "gu") 'dired-kill-children-subdir)
  (evil-define-key 'normal dired-mode-map (kbd "gh") 'dired-tree-up)
  (evil-define-key 'normal dired-mode-map (kbd "gj") 'dired-next-subdir)
  (evil-define-key 'normal dired-mode-map (kbd "gk") 'dired-prev-subdir)
  (evil-define-key 'normal dired-mode-map (kbd "gl") 'dired-goto-subdir-and-focus)
  (evil-define-key 'normal dired-mode-map [mouse-2] 'dired-mouse-find-file))

(use-package evil-org
  :ensure t
  :requires (org evil) 
  :custom
  (evil-org-special-o/O '(table-row item))
  (evil-org-use-additional-insert nil)
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . evil-org-set-key-theme)
  :config
  (evil-define-key '(normal visual) org-mode-map (kbd "H") 'org-shiftleft)
  (evil-define-key '(normal visual) org-mode-map (kbd "L") 'org-shiftright)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package yasnippet
  :ensure t
  :config
  ;; (keymap-unset yas-minor-mode-map "<tab>" t)
  ;; (keymap-unset yas-minor-mode-map "TAB" t)
  ;; (keymap-unset yas-minor-mode-map "<shift-tab>" t)
  ;; (keymap-unset yas-minor-mode-map "<backtab>" t)
  ;; (evil-define-minor-mode-key 'insert 'yas-minor-mode (kbd "SPC") yas-maybe-expand)
  (yas-global-mode 1))

;; (use-package yasnippet-snippets
;;   :requires yasnippet)

(use-package company
  :ensure t
  :requires evil
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 2)
  (company-show-quick-access t)
  (company-selection-wrap-around t)
  (company-tooltip-maximum-width 60)
  (company-selection-default nil)
  (company-backends
   '((company-capf company-clang company-cmake company-keywords :with company-dabbrev-code :separate)))
  (company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend company-preview-frontend company-echo-metadata-frontend))
  (company-global-modes
   '(sh-mode conf-mode c-mode c++-mode rust-mode latex-mode python-mode
             eglot-managed-mode))
  :config
  (defun company-shell-mode-configure ()
    (setq-local company-backends
                '((company-capf company-keywords company-files :with company-dabbrev-code :separate))))
  (defun company-org-mode-configure ()
    (setq-local company-backends
                '((company-capf company-ispell :with company-dabbrev :separate))))
  (add-hook 'sh-mode-hook 'company-shell-mode-configure)
  (add-hook 'conf-mode-hook 'company-shell-mode-configure)
  (add-hook 'org-mode-hook 'company-org-mode-configure)

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

  (defun company-complete-or-self-insert ()
    "Complete selection if selected, otherwise self insert"
    (interactive)
    (if (equal company-selection-changed nil)
        (call-interactively 'self-insert-command)
      (company-complete-selection)))
  
  (keymap-set company-active-map "<backspace>" 'company-backspace)
  (keymap-set company-active-map "C-h" nil)

  (evil-define-key 'insert company-mode-map (kbd "C-n")
    'company-select-next-or-complete-selection)
  (evil-define-key 'insert company-mode-map (kbd "C-p")
    'company-select-previous-or-complete-selection)
  (evil-define-key 'insert company-active-map (kbd "ESC")
    (lambda () (interactive) (company-abort) (evil-normal-state)))

  (keymap-set company-active-map "TAB" 'company-select-next)
  (keymap-set company-active-map "<tab>" 'company-select-next)
  (keymap-set company-active-map "<shift-tab>" 'company-select-previous)
  (keymap-set company-active-map "<backtab>" 'company-select-previous)
  (keymap-set company-active-map "SPC" 'company-complete-or-self-insert)

  (defun add-to-company-backends (list)
    (setq company-backends `(,(append list (car company-backends)))))
  (global-company-mode))

(use-package which-key
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My package-dependent customizations
(use-package my-appearance
  :requires org)

(use-package my-org
  :requires org)

(use-package my-parens
  :requires evil)

(use-package my-leader
  :requires (evil dired org ido my-tabs my-desktop my-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code graveyard

;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-hard t))

;; (use-package openwith
;;   :custom
;;   (openwith-associations '(("\\.pdf\\'" "zathura" (file))))
;;   :config
;;   (openwith-mode t))

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


;; (require 'my-appearance) ;; load (setq, defun, add-hook, defadvice)
;; (require 'my-modeline) ;; flymake, vc, flyspell
;; (require 'my-backup) ;; load (setq)
;; (require 'my-buffer) ;; (setq alist, defun interactive)
;; (require 'my-desktop) ;; load before leader.el, need symbols
;; leader needs evil, buffer.el, dired, desktop, gnus (command), external.el,
;;      org, flymake, hl-todo (can remap to hl-todo's use-package)

;; (require 'my-packages)

;; (require 'my-tabs) ;; has some mode-hooks, could load early and set in pkg. May need evil
;; (require 'my-parens) ;; currently needs evil, rewrite to use tabs.el' helper f for global overwrite
;; (require 'my-commands) ;; TODO inc/dec number
;; (require 'my-external) ;; literally just xdg-open wrapper
;; (require 'my-autoinsert) ;; has js2-mode hook, move out
;; (require 'my-gnus) ;; some variables
;; (require 'my-eww) ;; some variables
