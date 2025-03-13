;; assumes .dir-locals.el is in same directory as venv
((nil . ((eval . (setq-local my-project-path
                             (tramp-file-name-localname
                              (tramp-dissect-file-name
                               (file-name-directory
                                (let ((d (dir-locals-find-file "./")))
                                  (if (stringp d) d (car d))))))))
         (eval . (message "Project directory set to `%s'." my-project-path))))
 (python-mode . ((eval . (setq-local my-pylsp
                                     (concat my-project-path "venv/bin/pylsp")))
                 (eval . (setq-local eglot-server-programs  `((python-mode ,my-pylsp)))))))

