(defun tramp-change-sudoedit-doas ()
  (interactive)
  (delete '("sudoedit"
            (tramp-sudo-login
             (("sudo")
              ("-u" "%u")
              ("-S")
              ("-H")
              ("-p" "Password:")
              ("--"))))
          tramp-methods)
  (add-to-list 'tramp-methods
               '("sudoedit"
                 (tramp-sudo-login
                  (("doas")
                   ("-u" "%u")
                   ("-p" "Password:")
                   ("--"))))))
(add-hook 'tramp--startup-hook 'tramp-change-sudoedit-doas)

(defun my-change-number-at-point (change increment)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number increment)))
        (goto-char point)))))

(defun my-increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (my-change-number-at-point '+ (or increment 1)))

(defun my-decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (my-change-number-at-point '- (or increment 1)))

;; (global-set-key (kbd "C-c a") 'my-increment-number-at-point)
;; (global-set-key (kbd "C-c x") 'my-decrement-number-at-point)
