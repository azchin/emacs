(setq gnus-select-method '(nnnil))
;; (setq gnus-secondary-select-methods
;;       '((nntp "news.gwene.org")))
(add-hook 'kill-emacs-query-functions
          (lambda () (when (gnus-alive-p) (gnus-group-exit)) t))

(setq doc-view-continuous t)
