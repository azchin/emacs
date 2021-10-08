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
