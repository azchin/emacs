(provide 'my-packages)

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000)))

(require 'server)
(defun daemon-mode-snapshot ()
  (and (boundp 'server-process)
       (processp server-process)
       (server-running-p)))
(unless (server-running-p)
  (server-start))

(cd "~")
(defvar home-dir default-directory)

(require 'package)
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar nongnu '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(setq package-archives (list melpa gnu nongnu))

(setq use-package-compute-statistics t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments on package dependency structure:
;; - Assume the use of evil - key bindings should be set in the
;;   appropariate package expression rather than aggregated in evil
;; - Use hooks and commands wherever possible
;; - :config + setq vs. :custom - setq can be more easily evaluated
;; - :after vs. :requires - :after allows for sorting by source
;; - my-leader can be aggregate since it doesn't override bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My customizations
(use-package my-appearance)
(use-package my-modeline)
(use-package my-buffer)
(use-package my-desktop)
(use-package my-extra)
(use-package my-skeleton)
(use-package my-update)
(use-package my-org
  :after org)
(use-package my-parens ;; evil-define-minor-mode-key
  :after evil)
(use-package my-tabs ;; evil-shift-width and evil-define-key
  :after evil)
(use-package my-abbrev)
(use-package my-leader
  :after (evil evil-collection dired my-tabs my-desktop my-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in packages
(use-package org
  :after my-leader
  :config
  (evil-define-key 'normal 'global (kbd "<leader> o a") (lambda () (interactive) (create-new-frame-command 'org-agenda-list) (delete-other-windows)))
  (evil-define-key 'normal 'global (kbd "<leader> o l") 'org-store-link)
  ;; "o a" 'org-agent
  (evil-define-key 'normal 'global (kbd "<leader> o c") 'org-capture)
  (evil-define-key 'normal 'global (kbd "<leader> a o") 'my-org-toggle-appearance)
  (evil-define-key 'normal org-mode-map (kbd "<leader> o i") 'org-insert-structure-template)
  (defun my-org-src ()
    "Insert new source block or edit current block"
    (interactive)
    (condition-case nil
        (org-edit-src-code)
      (user-error (let ((mode (read-string "Major mode: ")))
                    (org-insert-structure-template "src")
                    (insert mode)
                    (org-edit-src-code)))))
  (evil-define-key 'normal org-mode-map (kbd "<leader> o s") 'my-org-src)
  ;; (evil-define-key leader-states org-mode-map (kbd "<leader> o s") 'org-schedule)
  ;; (evil-define-key leader-states org-mode-map (kbd "<leader> o d") 'org-deadline)
  (evil-define-key 'normal org-mode-map (kbd "<leader> o l") 'org-insert-link)
  (evil-define-key 'normal org-mode-map (kbd "<leader> o T") (lambda () (interactive) (insert " ") (call-interactively 'org-time-stamp) (delete-char 1)))
  (evil-define-key 'normal org-mode-map (kbd "<leader> o t") (lambda () (interactive) (insert " ") (org-insert-time-stamp (current-time) nil t) (delete-char 1)))
  (evil-define-key 'normal org-mode-map (kbd "<leader> o e") 'org-export-dispatch)
  (evil-define-key 'normal org-mode-map (kbd "<leader> o p") 'org-latex-export-to-pdf)
  (evil-define-key 'normal org-mode-map (kbd "<leader> o /") 'org-sparse-tree)
  (evil-define-key 'normal org-mode-map (kbd "<leader> o g g") 'org-occur)
  (evil-define-key 'normal org-mode-map (kbd "<leader> o g l") (lambda () (interactive) (org-occur "\\[\\[.*\\]\\[.*\\]\\]")))
  (evil-define-key 'normal org-mode-map (kbd "<leader> o b") 'org-bibtex-yank)
  (evil-define-key 'normal org-mode-map (kbd "<leader> o 5") 'org-present)
  (evil-define-key 'normal org-mode-map (kbd "g x") 'org-open-at-point)
  ;; (evil-define-key leader-states org-mode-map (kbd "<leader> o e l") 'org-latex-export-to-latex)
  ;; (evil-define-key leader-states org-mode-map (kbd "<leader> o e p") 'org-latex-export-to-pdf)
  ;; (evil-define-key 'normal 'global (kbd "<leader> o l") 'org-store-link)
  )

(use-package dired
  :config
  (require 'dired-x)
  (require 'dired-aux)
  (require 'ls-lisp)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-Ahl")
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-hide-details-hide-information-lines t)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-free-space nil)
  (setopt dired-mouse-drag-files t)
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-use-string-collate nil)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  ;; (defun dired-goto-subdir-and-focus ()
  ;;   (interactive)
  ;;   (call-interactively 'dired-maybe-insert-subdir)
  ;;   (recenter 1))

  ;; (defun dired-kill-subdir-and-pop ()
  ;;   (interactive)
  ;;   (dired-kill-subdir)
  ;;   (set-mark-command 1)
  ;;   (recenter))

  ;; (defun dired-kill-subdir-and-up ()
  ;;   (interactive)
  ;;   (set-mark-command nil)
  ;;   (condition-case nil
  ;;       (progn (dired-tree-up 1)
  ;;              (exchange-point-and-mark)
  ;;              (dired-kill-subdir)
  ;;              (set-mark-command 1)
  ;;              (recenter 0))
  ;;     (error (deactivate-mark))))

  ;; (defun dired-kill-subdir-recurse (level)
  ;;   (interactive)
  ;;   (condition-case nil
  ;;       (progn (dired-tree-down) (dired-kill-subdir-recurse (+ level 1)))
  ;;     (error (condition-case nil
  ;;                (unless (eq level 0)
  ;;                  (progn (dired-kill-subdir-and-up)
  ;;                         (dired-kill-subdir-recurse (- level 1))))
  ;;              (error nil)))))

  ;; (defun dired-kill-children-subdir ()
  ;;   (interactive)
  ;;   (dired-kill-subdir-recurse 0)
  ;;   (dired-kill-subdir-and-up))
  )
(use-package dired
  :after evil
  :config
  (evil-define-key 'normal dired-mode-map "f" 'find-file)
  (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-file)
  (evil-define-key 'normal dired-mode-map (kbd "<left>") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "<right>") 'dired-find-file)
  ;; (evil-define-key 'normal dired-mode-map (kbd "TAB") 'dired-goto-subdir-and-focus)
  ;; (evil-define-key 'normal dired-mode-map (kbd "<backtab>") 'dired-kill-children-subdir)
  ;; (evil-define-key 'normal dired-mode-map (kbd "<shift-tab>") 'dired-kill-children-subdir)
  ;; (evil-define-key 'normal dired-mode-map (kbd "gu") 'dired-kill-children-subdir)
  ;; (evil-define-key 'normal dired-mode-map (kbd "gh") 'dired-tree-up)
  ;; (evil-define-key 'normal dired-mode-map (kbd "gj") 'dired-next-subdir)
  ;; (evil-define-key 'normal dired-mode-map (kbd "gk") 'dired-prev-subdir)
  ;; (evil-define-key 'normal dired-mode-map (kbd "gl") 'dired-goto-subdir-and-focus)
  (evil-define-key 'normal dired-mode-map [mouse-2] 'dired-mouse-find-file))

(use-package midnight
  :config
  (setq clean-buffer-list-delay-special 0)
  (setq clean-buffer-list-delay-general 1)
  ;; (setq clean-buffer-list-timer (run-at-time t 3600 'clean-buffer-list))
  (setq clean-buffer-list-kill-regexps '("^.*$"))
  (setq clean-buffer-list-kill-never-buffer-names
        '("*scratch*" "*Messages*" "*cmd*"))
  (setq clean-buffer-list-kill-never-regexps
        '("^\\ .*$" "\\*.*scratch\\*" "\\` \\*Minibuf-.*\\*\\'"
          "^\\*EMMS Playlist\\*.*$" "^\\*.*eshell\\*" "^\\*Help.*\\*$"
          "^\\*Article.*\\*$" "^\\*Summary.*\\*$" "^\\*eww\\*$" "^\\*Group\\*$"
          "^\\([A-Za-z0-9_-]*\\.\\)+[A-Za-z0-9]*\\(<[A-Za-z0-9_-]*>\\)?$" "^Makefile.*$"))
  (run-at-time t 1800 'clean-buffer-list))

;; Soft dependency on yasnippets and company
(use-package eglot
  :hook
  (rust-mode . eglot-ensure)
  :config
  (setq read-process-output-max (* 1024 32))
  (add-to-list 'eglot-server-programs
               '(c-mode . ("ccls")))
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer"))))

(use-package flymake
  :after evil
  :commands (flymake-mode flymake-start)
  :config
  (evil-define-minor-mode-key 'normal 'flymake-mode (kbd "[ c") 'flymake-goto-prev-error)
  (evil-define-minor-mode-key 'normal 'flymake-mode (kbd "] c") 'flymake-goto-next-error))

;; treesit-install-language-grammar
(use-package treesit
  :hook
  (rust-mode . rust-ts-mode)
  (c-mode . c-ts-mode)
  (c++-mode . c++-ts-mode)
  (python-mode . python-ts-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA packages
(use-package magit
  :ensure t
  :commands (magit-init magit-clone magit-status magit-blame)
  :config
  (keymap-unset magit-status-mode-map "SPC"))

(use-package org-roam
  :ensure t
  :after org
  :commands (org-roam-node-insert org-roam-capture org-roam-node-find
                                  org-roam-buffer-toggle org-roam-buffer-display-dedicated
                                  org-roam-graph)
  :config
  (require 'org-roam-protocol)
  ;; xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
  (defvar my-org-roam-bibliography "~/org-roam/references.bib" "Big bibliography file")
  (defun my-org-cite-yank-or-insert-key ()
    (interactive)
    (condition-case nil (my-bibtex-yank my-org-roam-bibliography)
      (user-error (with-temp-buffer
                    (org-mode)
                    (insert (format "#+bibliography: %s\n" my-org-roam-bibliography))
                    (org-cite-insert nil)
                    (search-backward "@")
                    (forward-char)
                    (push-mark)
                    (search-forward "]")
                    (backward-char)
                    (buffer-substring-no-properties (mark) (point))))))
  (add-to-list 'org-roam-capture-templates
               '("t" "todo" plain "%?" :target
                 (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}
#+filetags: :todo:
")))
  (add-to-list 'org-roam-capture-templates
               `("p" "paper" plain "%?" :target
                 (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            ,(concat "#+title: ${title}
#+bibliography: " my-org-roam-bibliography "
"))))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :commands org-roam-ui-mode
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package citar-org-roam
  :ensure t
  :after (org-roam citar)
  :config
  (defun my-org-citar-open-note ()
    (interactive)
    (let* ((citar-bibliography (list my-org-roam-bibliography))
           (citekey (my-org-cite-yank-or-insert-key)))
      (citar-open-notes (list citekey))
      (org-roam-ref-remove (concat "@" citekey))
      (org-roam-ref-add (concat "[cite:@" citekey "]"))
      (call-interactively 'org-roam-tag-add)))
  (setq citar-org-roam-capture-template-key "p")
  (citar-org-roam-mode))

(use-package org-present
  :ensure t
  :after (org evil)
  :commands org-present
  :init
  ;; System Crafters' org-present config
  (defun dw/org-present-prepare-slide ()
    (org-overview)
    (org-show-entry)
    (org-show-children))

  (defun dw/org-present-hook ()
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.5) variable-pitch)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)
                                       ))
    (setq header-line-format " ")
    (org-display-inline-images)
    ;; (dw/org-present-prepare-slide)
    (disable-lines)
    (org-show-all))

  (defun dw/org-present-quit-hook ()
    (setq-local face-remapping-alist '((default variable-pitch)))
    (setq header-line-format nil)
    (run-hooks 'text-mode-hook)
    (org-present-small)
    (org-remove-inline-images))

  (defun dw/org-present-prev ()
    (interactive)
    (org-present-prev)
    (dw/org-present-prepare-slide))

  (defun dw/org-present-next ()
    (interactive)
    (org-present-next)
    (dw/org-present-prepare-slide))

  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode . evil-force-normal-state)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(use-package citar
  :ensure t
  :custom
  ;; (org-cite-global-bibliography '("~/bib/references.bib"))
  ;; (citar-bibliography org-cite-global-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

(use-package hl-todo
  :ensure t
  :after evil
  :hook
  (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-wrap-movement t)
  (evil-define-minor-mode-key 'normal 'hl-todo-mode (kbd "[ d") 'hl-todo-previous)
  (evil-define-minor-mode-key 'normal 'hl-todo-mode (kbd "] d") 'hl-todo-next))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :hook
  (markdown-mode . auto-fill-mode))

(use-package ox-gfm
  :after org
  :ensure t)

(use-package osm
  :ensure t
  ;; :bind ("C-c m" . osm-prefix-map) ;; Alternative: `osm-home'
  :commands (osm-home osm-search osm-goto osm-bookmark-jump osm-gpx-show)
  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information

  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

(use-package cmake-mode
  :ensure t
  :mode
  "\\.cmake\\'"
  "CMakeLists\\.txt\\'")

(use-package rust-mode
  :ensure t
  :mode
  "\\.rs\\'")

(use-package undo-fu
  :ensure t
  :config
  (setq undo-fu-ignore-keyboard-quit t))

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (setq undo-fu-session-directory (emacsd "cache/backups"))
  (global-undo-fu-session-mode))

(use-package evil
  :ensure t
  :after undo-fu
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
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-fu
        evil-want-change-word-to-end nil
        evil-search-module 'evil-search
        evil-split-window-below nil
        evil-vsplit-window-right nil)
  :config
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'vc-annotate-mode 'insert)
  (evil-set-initial-state 'gnus-mode 'emacs)
  (evil-set-initial-state 'osm-mode 'emacs)
  (defun simulate-key-presses (key-string)
    (setq unread-command-events (listify-key-sequence (kbd key-string))))
  (evil-define-key 'visual 'global (kbd "M-<down>")
    (lambda (count) (interactive "p")
      (simulate-key-presses (format ":m '>+%d <return> gv= gv" count))))
  (evil-define-key 'visual 'global (kbd "M-<up>")
    (lambda (count) (interactive "p")
      (simulate-key-presses (format ":m '<-%d <return> gv= gv" (1+ count)))))
  (evil-define-key 'insert 'global (kbd "C-d") 'delete-char)
  (evil-define-key 'insert 'global (kbd "C-a") 'beginning-of-line)
  (evil-define-key 'insert 'global (kbd "C-e") 'end-of-line)
  (evil-define-key '(normal insert) 'global (kbd "C-S-v") 'yank)
  (evil-define-key 'visual 'global (kbd "C-S-c") 'evil-yank)
  (evil-define-key 'insert 'global (kbd "C-S-c") 'copy-region-as-kill)
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-quickscope
  :ensure t
  :after evil
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  ;; (evil-collection-init
  ;;  '(apropos calc calendar cmake-mode company compile consult corfu debug
  ;;            dictionary diff-mode dired dired-sidebar doc-view edebug ediff eglot
  ;;            elisp-mode elisp-refs eshell eww flycheck flymake grep help ibuffer image
  ;;            image-dired imenu imenu-list (indent "indent") log-edit log-view man
  ;;            (magit magit-repos magit-submodule) magit-section magit-todos markdown-mode org
  ;;            org-present org-roam outline (package-menu package) (pdf pdf-view) python rg
  ;;            sh-script simple tab-bar (term term ansi-term multi-term) typescript-mode
  ;;            vertico view vterm which-key xref))
  (evil-collection-init)
  (evil-define-motion my-evil-collection-unimpaired-next-error (count)
    "Go to next error."
    :jump t
    (setq count (or count 1))
    (cond
     ((and (bound-and-true-p flycheck-mode)
           (fboundp 'flycheck-next-error))
      (flycheck-next-error count))
     ((and (bound-and-true-p flymake-mode)
           (fboundp 'flymake-goto-next-error))
      (flymake-goto-next-error count))
     (:default
      (next-error count))))

  (evil-define-motion my-evil-collection-unimpaired-previous-error (count)
    "Go to previous error."
    :jump t
    (my-evil-collection-unimpaired-next-error (- (or count 1))))

  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map (kbd "[ q") 'my-evil-collection-unimpaired-previous-error)
  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map (kbd "] q") 'my-evil-collection-unimpaired-next-error)
  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map (kbd "[ x") 'xref-go-back)
  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map (kbd "] x") 'xref-go-forward)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "SPC") nil)
  (evil-collection-define-key 'normal 'help-mode-map (kbd "SPC") nil))

(use-package evil-org
  :ensure t
  :after (org evil) 
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . evil-org-set-key-theme)
  :config
  (setq evil-org-special-o/O '(table-row item))
  (setq evil-org-use-additional-insert nil)
  (evil-define-key '(normal visual) org-mode-map (kbd "H") 'org-shiftleft)
  (evil-define-key '(normal visual) org-mode-map (kbd "L") 'org-shiftright)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package avy
  :ensure t
  :config
  ;; g x w c f d a r s t
  (setopt avy-keys '(103 120 119 99 102 100 97 114 115 116)))

(use-package yasnippet
  :ensure t
  :hook
  (eglot-managed-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package company
  :ensure t
  :after evil
  :hook
  (sh-mode conf-mode c-mode c++-mode rust-mode latex-mode python-mode
           eglot-managed-mode)
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 2)
  (setq company-show-quick-access t)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-maximum-width 60)
  (setq company-selection-default nil)
  (setq company-backends
        '((company-capf company-cmake company-keywords
                        :with company-dabbrev-code :separate)))
  (setq company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  (defun company-shell-mode-configure ()
    (setq-local company-backends
                '((company-capf company-keywords company-files
                                :with company-dabbrev-code :separate))))
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
      (set-face-attribute face nil :family default-font-family)))
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
  
  (evil-define-key 'insert company-mode-map (kbd "C-n")
    'company-select-next-or-complete-selection)
  (evil-define-key 'insert company-mode-map (kbd "C-p")
    'company-select-previous-or-complete-selection)
  (evil-define-key 'insert company-active-map (kbd "ESC")
    (lambda () (interactive) (company-abort) (evil-normal-state)))

  ;;TODO eglot overrides some bindings, look into what's needed
  (keymap-set company-active-map "<backspace>" 'company-backspace)
  (keymap-set company-active-map "C-h" nil)
  (keymap-set company-active-map "TAB" 'company-select-next)
  (keymap-set company-active-map "<tab>" 'company-select-next)
  (keymap-set company-active-map "<shift-tab>" 'company-select-previous)
  (keymap-set company-active-map "<backtab>" 'company-select-previous)
  (keymap-set company-active-map "SPC" 'company-complete-or-self-insert)

  (defun add-to-company-backends (list)
    (setq company-backends `(,(append list (car company-backends))))))

(use-package which-key
  :ensure t
  :commands which-key-mode)

(use-package consult
  :ensure t
  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  (keymap-global-set "C-s" 'consult-line)
  )

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  ;; :bind (:map minibuffer-local-map
  ;;             ("M-A" . marginalia-cycle))
  
  :config
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package rg
  :ensure t
  :commands (rg rg-dwim-current-file rg-dwim-current-dir rg-dwim-project-dir rg-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-contrib
  :ensure t
  :after org
  :pin "nongnu"
  :config
  (add-to-list 'org-modules 'ol-git-link))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code graveyard
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))

(use-package gnus
  :disabled
  :commands gnus
  :config
  (setq gnus-select-method '(nnnil))
  (add-hook 'kill-emacs-query-functions
            (lambda () (when (gnus-alive-p) (gnus-group-exit)) t)))

(use-package eww
  :disabled
  :config
  (setq browse-url-generic-program "vimb")

  (defun buffer-local-eww-browser-default ()
    (make-local-variable 'browse-url-browser-function)
    (setq browse-url-browser-function 'eww-browse-url))

  (defun buffer-local-generic-browser-default ()
    (make-local-variable 'browse-url-browser-function)
    (setq browse-url-browser-function 'browse-url-generic))

  (add-hook 'gnus-mode-hook 'buffer-local-generic-browser-default)
  (add-hook 'eww-after-render-hook 'eww-readable))

(use-package smalltalk-mode
  :disabled
  :ensure t)

(use-package tuareg
  :disabled
  :ensure t)

(use-package auto-package-update
  :disabled
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :hook
  (auto-package-update-before . (lambda () (message "Updating packages...")))
  (auto-package-update-after . (lambda () (message "Packages updated...")))
  :config
  (auto-package-update-maybe))

(use-package icomplete
  :disabled
  :config
  ;; TODO use current text for dired rename
  (fido-vertical-mode 1))

(use-package ido
  :disabled
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-decorations
        '("\n> " "" "\n  " "" "[" "]" " [No match]" " [Matched]" " [Not readable]"
          " [Too big]" " [Confirm]" "\n>[" "]"))
  (setq ido-max-window-height 0.5)
  (setq ido-max-prospects 25)
  (setq ido-enable-last-directory-history nil)
  (setq ido-enter-matching-directory 'first)
  (setq ido-create-new-buffer 'always)

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

  (ido-replace-completing-read-gen read-project-desktop)
  (ido-replace-completing-read-gen project-find-file)
  (ido-replace-completing-read-gen project-dired)
  (ido-replace-completing-read-gen project-vc-dir)
  (ido-replace-completing-read-gen project-eshell)
  (ido-replace-completing-read-gen project-compile)
  (ido-replace-completing-read-gen project-switch-to-buffer)
  (ido-replace-completing-read-gen project-switch-project)
  (ido-replace-completing-read-gen project-kill-buffers)
  (ido-replace-completing-read-gen project-forget-project)

  (ido-everywhere 1)
  (ido-mode 1))

(use-package yasnippet-snippets
  :disabled
  :after yasnippet)

(use-package gruvbox-theme
  :disabled
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package openwith
  :disabled
  :ensure t
  :custom
  (openwith-associations '(("\\.pdf\\'" "zathura" (file))))
  :config
  (openwith-mode t))

(use-package js2-mode
  :disabled
  :ensure t
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :disabled
  :ensure t
  :custom
  (typescript-indent-level 2))

(use-package json-mode
  :disabled
  :ensure t)

(use-package pdf-tools
  :disabled
  :ensure t
  :config 
  ;; (add-hook 'pdf-view-mode 'auto-revert-mode)
  (add-hook 'pdf-view-mode 'pdf-view-midnight-minor-mode)
  (pdf-tools-install))

;; https://www.reddit.com/r/emacs/comments/cd6fe2/how_to_make_emacs_a_latex_ide/
(use-package tex
  :disabled
  :ensure auctex
  :after (evil pdf-tools)
  :init
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  ;; (setq TeX-engine 'xetex)
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda () (set-face-foreground 'font-latex-script-char-face "#9aedfe")))

  (add-hook 'TeX-after-compilation-finished-functions 
            'TeX-revert-document-buffer)
  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda () (reftex-mode t) (flyspell-mode t)))
  )

(use-package company-c-headers
  :disabled
  :ensure t
  :after company
  :config
  (add-to-company-backends '(company-c-headers)))

(use-package company-shell
  :disabled
  :ensure t
  :after company
  :custom
  (add-to-company-backends '(company-shell company-shell-env)))

(use-package company-auctex
  :disabled
  :ensure t
  :after company
  :config
  (add-to-company-backends '(company-auctex))
  (company-auctex-init))

(use-package web-mode
  :disabled
  :ensure t
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.jsx")))
  (setq web-mode-content-types-alist '(("jsx" . "\\.tsx"))))

(use-package tree-sitter
  :disabled
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :disabled
  :ensure t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package ivy
  :ensure t
  :disabled
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
  :config
  (setq ivy-count-format "")
  (setq ivy-height 16)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (add-to-list 'ivy-initial-inputs-alist '(counsel-minor . ""))
  (ivy-mode 1))

(use-package ivy-hydra
  :ensure t
  :disabled)

(use-package swiper
  :ensure t
  :disabled
  :config
  (defvaralias 'swiper-history 'regexp-search-ring)
  (keymap-global-set "C-s" 'swiper-isearch))

(use-package counsel
  :ensure t
  :disabled
  :config
  (counsel-mode 1))

(use-package org-modern
  :ensure t
  :disabled
  :after org
  :hook (org-mode))
