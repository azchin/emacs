(setq org-directory "~/org/")
(setq org-startup-folded t)
(setq org-agenda-files `(,(concat org-directory "agenda.org")))
(setq org-agenda-span 'month)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-src-window-setup 'other-frame)
;; (add-hook 'org-mode-hook 'org-indent-mode)
;; (setq org-agenda-files '(org-directory))
;; (setq org-agenda-file-regexp "agenda.org")
