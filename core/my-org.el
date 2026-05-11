(require 'org)
(require 'ox-latex)
;; (require 'org-tempo) ;; this does < s TAB

(setq org-directory "~/drive/org/")
(setq org-agenda-files `(,(concat org-directory "agenda.org")))
(setq org-capture-templates
      `(("j" "Journal Entry"
         entry (file+olp+datetree ,(concat org-directory "journal.org")))
        ("b" "Beorg notes"
         entry (file ,(concat org-directory "beorg-notes.org")))
        ("t" "Task (hour block)"
         entry (file+headline ,(concat org-directory "agenda.org") "Tasks")
         ,(concat "* %(read-string \"Title: \") "
                  "<%(format-time-string \"%Y-%m-%d\") "
                  "%^{Start|00:00|01:00|02:00|03:00|04:00|05:00|06:00|07:00|08:00|09:00|10:00|11:00|12:00|13:00|14:00|15:00|16:00|17:00|18:00|19:00|20:00|21:00|22:00|23:00}>"))
        ("p" "Planned task"
         entry (file+headline ,(concat org-directory "agenda.org") "Planned")
         "* TODO %^{Title}")
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
(setq org-hide-leading-stars nil)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-indent-mode-turns-off-adapt-indentation t)
(setq org-adapt-indentation nil)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 4)
(setq org-export-with-sub-superscripts '{})
(setq org-use-sub-superscripts '{})

(setq org-hide-emphasis-markers nil)
(setq org-link-descriptive t)
(setq org-pretty-entities t)

(add-to-list 'org-structure-template-alist '("t" . "src text"))

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
(setq org-babel-default-inline-header-args
      (cons '(:exports . "code")
            (assq-delete-all :exports org-babel-default-inline-header-args)))

(defun my-org-toggle-appearance ()
  (interactive)
  ;; (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (org-toggle-link-display)
  (org-toggle-pretty-entities))

;; (setq org-latex-compiler "pdflatex")
;; (setq org-latex-bib-compiler "biber")
(setq org-latex-pdf-process '("latexmk -pdf -bibtex %f"))
;; (setq org-agenda-files '(org-directory))
;; (setq org-agenda-file-regexp "agenda.org")
(setopt org-export-backends '(ascii html latex beamer odt md))

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_CA")
(setq ispell-local-dictionary-alist
      '(("en_CA" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
(setq text-mode-ispell-word-completion nil)
(use-package ispell
  :config
  (ispell-find-hunspell-dictionaries))

;; (add-hook 'org-mode-hook 'auto-fill-mode)
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

(defun my-org-agenda-archive-tasks ()
  "Refile children of \"Tasks\" to \"Archived\" in agenda.org.
If a region is active in agenda.org, only refile headings whose
subtree overlaps the region."
  (interactive)
  (let* ((file (expand-file-name "agenda.org" org-directory))
         (buf (or (find-buffer-visiting file)
                  (find-file-noselect file)))
         (region-active (and (use-region-p) (eq (current-buffer) buf)))
         (rbeg (and region-active (region-beginning)))
         (rend (and region-active (region-end))))
    (with-current-buffer buf
      (let ((archived-pos (org-find-exact-headline-in-buffer "Archived"))
            (tasks-pos    (org-find-exact-headline-in-buffer "Tasks")))
        (unless archived-pos (user-error "No \"Archived\" heading in %s" file))
        (unless tasks-pos    (user-error "No \"Tasks\" heading in %s" file))
        (let* ((rfloc (list "Archived" file nil archived-pos))
               (tasks-level (save-excursion
                              (goto-char tasks-pos) (org-current-level)))
               (child-level (1+ tasks-level))
               markers)
          (save-excursion
            (goto-char tasks-pos)
            (let ((end (save-excursion (org-end-of-subtree t t))))
              (while (re-search-forward org-heading-regexp end t)
                (beginning-of-line)
                (when (= (org-current-level) child-level)
                  (let ((hbeg (point))
                        (hend (save-excursion (org-end-of-subtree t t))))
                    (when (or (not region-active)
                              (and (< hbeg rend) (> hend rbeg)))
                      (push (copy-marker hbeg) markers))))
                (end-of-line))))
          (setq markers (nreverse markers))
          (dolist (m markers)
            (goto-char m)
            (org-refile nil nil rfloc)
            (set-marker m nil))
          (message "Archived %d task(s)" (length markers)))))))

(defun my-org-read-hour-block (&optional prompt)
  "Prompt for an HH:00 time of day and return <YYYY-MM-DD HH:00>."
  (let* ((hours (mapcar (lambda (h) (format "%02d:00" h)) (number-sequence 0 23)))
         (hour (completing-read (or prompt "Start: ") hours nil nil)))
    (format "<%s %s>" (format-time-string "%Y-%m-%d") hour)))

(defun my-org-agenda-schedule-planned ()
  "Schedule task(s) from \"Planned\" into \"Tasks\" in agenda.org.
If a region is active in agenda.org, process every Planned heading
whose subtree overlaps the region; otherwise prompt to pick one.
For each, prompt for an hour-block timestamp, drop the TODO state,
append the timestamp to the title, and refile under \"Tasks\"."
  (interactive)
  (let* ((file (expand-file-name "agenda.org" org-directory))
         (buf (or (find-buffer-visiting file)
                  (find-file-noselect file)))
         (region-active (and (use-region-p) (eq (current-buffer) buf)))
         (rbeg (and region-active (region-beginning)))
         (rend (and region-active (region-end))))
    (with-current-buffer buf
      (let ((planned-pos (org-find-exact-headline-in-buffer "Planned"))
            (tasks-pos   (org-find-exact-headline-in-buffer "Tasks")))
        (unless planned-pos (user-error "No \"Planned\" heading in %s" file))
        (unless tasks-pos   (user-error "No \"Tasks\" heading in %s" file))
        (let* ((rfloc (list "Tasks" file nil tasks-pos))
               (planned-level (save-excursion
                                (goto-char planned-pos) (org-current-level)))
               (child-level (1+ planned-level))
               markers)
          (save-excursion
            (goto-char planned-pos)
            (let ((end (save-excursion (org-end-of-subtree t t))))
              (while (re-search-forward org-heading-regexp end t)
                (beginning-of-line)
                (when (= (org-current-level) child-level)
                  (let ((hbeg (point))
                        (hend (save-excursion (org-end-of-subtree t t))))
                    (when (or (not region-active)
                              (and (< hbeg rend) (> hend rbeg)))
                      (push (copy-marker hbeg) markers))))
                (end-of-line))))
          (setq markers (nreverse markers))
          (when (null markers)
            (user-error "No matching tasks under \"Planned\""))
          (unless region-active
            (let* ((alist (mapcar
                           (lambda (m)
                             (cons (save-excursion
                                     (goto-char m)
                                     (org-get-heading t t t t))
                                   m))
                           markers))
                   (choice (completing-read "Planned task: " alist nil t)))
              (setq markers (list (cdr (assoc choice alist))))))
          (dolist (m markers)
            (goto-char m)
            (let* ((title (org-get-heading t t t t))
                   (ts    (my-org-read-hour-block
                           (format "Start for %s: " title))))
              (org-edit-headline (format "%s %s" title ts))
              (org-todo 'none)
              (org-refile nil nil rfloc))
            (set-marker m nil))
          (message "Scheduled %d task(s)" (length markers)))))))

(defun my-org-agenda-sort-tasks ()
  "Sort children of \"Tasks\" in agenda.org by timestamp."
  (interactive)
  (let* ((file (expand-file-name "agenda.org" org-directory))
         (buf (or (find-buffer-visiting file)
                  (find-file-noselect file))))
    (with-current-buffer buf
      (let ((tasks-pos (org-find-exact-headline-in-buffer "Tasks")))
        (unless tasks-pos (user-error "No \"Tasks\" heading in %s" file))
        (save-excursion
          (goto-char tasks-pos)
          (org-sort-entries nil ?t))))))

(require 'transient)
(transient-define-prefix my-org-agenda-dispatch ()
  "Dispatcher for agenda.org actions."
  ["Agenda"
   ("a" "Archive tasks"        my-org-agenda-archive-tasks)
   ("s" "Sort tasks by time"   my-org-agenda-sort-tasks)
   ("p" "Schedule planned"     my-org-agenda-schedule-planned)])

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
