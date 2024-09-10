(require 'org)
(require 'org-tempo)
(require 'ox-latex)

(setq org-directory "~/drive/org/")
(setq org-agenda-files `(,(concat org-directory "agenda.org")))
(setq org-capture-templates
      `(("j" "Journal Entry"
         entry (file+olp+datetree ,(concat org-directory "journal.org")))
        ("b" "Beorg notes"
         entry (file ,(concat org-directory "beorg-notes.org")))
        ("P" "Paragraph formatting"
         plain (file (lambda () (call-interactively 'find-file) (buffer-file-name)))
         "#+latex_header: \\usepackage{parskip}\n"
         :prepend t)
        ("a" "Assignment"
         plain (file (lambda () (call-interactively 'find-file) (buffer-file-name)))
         "#+title: %^{Title}\n#+options: date:nil author:nil\n"
         :prepend t)))
(setq org-agenda-span 'month)
(setq org-image-actual-width 500)
(setq org-export-with-toc nil)
;; (setq org-export-with-date nil)
;; (setq org-export-with-author nil)
(setq org-export-with-section-numbers nil)
(setq org-export-in-background t)
(setq org-export-preserve-breaks nil)
(setq org-list-allow-alphabetical t)
(setq org-startup-folded 'nofold)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars nil)
(setq org-link-descriptive t)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-indent-mode-turns-off-adapt-indentation t)
(setq org-adapt-indentation nil)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-pretty-entities t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 4)
(setq org-export-with-sub-superscripts '{})
(setq org-use-sub-superscripts '{})
(setq org-todo-keywords '((sequence "TODO" "PROG" "|" "DONE" "AXED")))
(setopt safe-local-variable-values
        '((org-emphasis-alist . nil)
          (org-hide-emphasis-markers . nil)
          (org-pretty-entities . nil)))
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
(setq org-link-frame-setup (mapcar (lambda (x) (cond ((equal (car x) 'file)
                                                      '(file . find-file))
                                                     (t x)))
                                   org-link-frame-setup))

(defun my-org-toggle-appearance ()
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (org-toggle-link-display)
  (org-toggle-pretty-entities))

;; (setq org-latex-compiler "pdflatex")
;; (setq org-latex-bib-compiler "biber")
(setq org-latex-pdf-process '("latexmk -pdf -bibtex %f"))
;; (setq org-agenda-files '(org-directory))
;; (setq org-agenda-file-regexp "agenda.org")
(setopt org-export-backends '(ascii html latex odt md))

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_CA")
(setq ispell-local-dictionary-alist
      '(("en_CA" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(with-eval-after-load 'flyspell
  (keymap-set flyspell-mouse-map "<mouse-3>" 'flyspell-correct-word)
  (keymap-unset flyspell-mouse-map "<mouse-2>"))

(defun insert-zero-width-char ()
  (interactive) (insert-char #x200b))
(keymap-set org-mode-map "C-c z" 'insert-zero-width-char)

(defun my-org-goto-top-in-buffer-setting (setting)
  "Move point to first occurrence of `setting' or return nil"
  (let ((current-line-setting-match
         (lambda () (string-match (format "^#\\+\\(%s\\|%s\\|%s\\):"
                                          setting
                                          (upcase setting)
                                          (downcase setting))
                                  (org-current-line-string)))))
    (goto-char (point-min))
    (while (and (string-match "^\\(:[[:alpha:]]+:\\|#\\+[[:alpha:]_]+:\\)"
                              (org-current-line-string))
                (not (funcall current-line-setting-match)))
      (forward-line))
    (when (funcall current-line-setting-match)
      (progn (forward-char (match-end 0)) t))))

(defun my-org-set-top-in-buffer-setting (setting value)
  "Sets org mode in-buffer settings at the top of the buffer.
Skips past any existing top-level properties and settings, and appends
a line containing the `setting' and `value'."
  (let ((current-line-setting-match
         (lambda () (string-match (format "^#\\+\\(%s\\|%s\\|%s\\):"
                                          setting
                                          (upcase setting)
                                          (downcase setting))
                                  (org-current-line-string)))))
    (save-excursion
      (goto-char (point-min))
      (while (and (string-match "^\\(:[[:alpha:]]+:\\|#\\+[[:alpha:]_]+:\\)"
                                (org-current-line-string))
                  (not (funcall current-line-setting-match)))
        (forward-line))
      (if (my-org-goto-top-in-buffer-setting setting)
          (progn (delete-region (point) (line-end-position))
                 (insert (format " %s" value)))
        (insert (format "#+%s: %s\n" setting value))))))

(defun my-org-get-in-buffer-setting (setting)
  "Get value of first occurrence of in-buffer `setting'"
  (save-excursion 
    (goto-char (point-min))
    (when (my-org-goto-top-in-buffer-setting setting)
      (buffer-substring-no-properties (point) (line-end-position)))))

;; use org-mks to create an interactive selection table
(defun my-org-set-title ()
  "Sets the title of an org buffer"
  (interactive)
  (my-org-set-top-in-buffer-setting "title"
                                    (read-string "New title: " (org-get-title))))

(defun my-org-set-author ()
  "Sets the author of an org buffer"
  (interactive)
  (my-org-set-top-in-buffer-setting "author"
                                    (read-string "New author: " (my-org-get-in-buffer-setting "author"))))

(defun my-bibtex-yank (bibliography)
  "Paste into specified bibliography, return key"
  (with-temp-file bibliography
    (bibtex-mode)
    (or (and (progn
               (insert-file-contents bibliography)
               (goto-char (point-max))
               (condition-case nil (progn (yank) t) (error nil)))
             (progn
               (exchange-point-and-mark)
               (condition-case nil (bibtex-validate) (error nil)))
             (progn
               (deactivate-mark)
               (let ((ret (cdr (assoc "=key=" (bibtex-parse-entry)))))
                 (bibtex-reformat) ;; NOTE reformat with region if buffer is slow
                 ret)))
        (progn
          (deactivate-mark)
          (user-error "Yanked text is not a valid BibTeX entry")))))

(provide 'my-org)
