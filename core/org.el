(setq org-directory "~/org/")
(setq org-startup-folded t)
(setq org-agenda-files `(,(concat org-directory "agenda.org")))
(setq org-agenda-span 'month)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-src-window-setup 'other-frame)
(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
(add-hook 'org-mode-hook (lambda () (org-indent-mode -1)))
;; (setq org-agenda-files '(org-directory))
;; (setq org-agenda-file-regexp "agenda.org")

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_CA")
(setq ispell-local-dictionary-alist
      '(("en_CA" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

