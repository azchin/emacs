(provide 'my-parens)

(setq show-paren-style 'parenthesis)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(show-paren-mode 1)

(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(setq electric-pair-open-newline-between-pairs t)
(setq electric-pair-skip-whitespace 'chomp)
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; Modified from electric-pair-mode-map
(evil-define-minor-mode-key 'insert 'electric-pair-mode (kbd "<backspace>")
  `(menu-item
    "" backward-whitespace-to-tab-stop
    :filter
    ,(lambda (cmd)
       (let* ((prev (char-before))
              (next (char-after))
              (syntax-info (and prev
                                (electric-pair-syntax-info prev)))
              (syntax (car syntax-info))
              (pair (cadr syntax-info)))
         (and next pair
              (memq syntax '(?\( ?\" ?\$))
              (eq pair next)
              (if (functionp electric-pair-delete-adjacent-pairs)
                  (funcall electric-pair-delete-adjacent-pairs)
                electric-pair-delete-adjacent-pairs)
              (delete-char 1)
              cmd)))))

;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html
;; Word constituents for non-special chars "w" or symbol "_"
;; Check before modifying using (string (char-syntax ?c))
;; (modify-syntax-entry ...)
