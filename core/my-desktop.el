(setq desktop-path `(,(emacsd "cache/default-desktop/")))
(setq desktop-load-locked-desktop t)
;; (setq desktop-restore-frames nil)
;; (setq desktop-restore-reuses-frames 'keep)
;; (setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

(savehist-mode)
(setq savehist-file (emacsd "cache/history"))
(setq savehist-autosave-interval 120)
(add-to-list 'savehist-additional-variables 'file-name-history)

(defun server-frame-desktop ()
  (desktop-save-mode nil)
  (desktop-read (emacsd "cache/default-desktop"))
  (remove-hook 'server-after-make-frame-hook 'server-frame-desktop))
;; (when daemon-mode-snapshot (add-hook 'server-after-make-frame-hook 'server-frame-desktop))

;; Restore desktop on startup 
;; (if daemon-mode-snapshot
;;     (add-hook 'server-after-make-frame-hook 'server-frame-desktop)
;;   (desktop-save-mode 1))

;; (if daemon-mode-snapshot
;;     (add-hook 'server-after-make-frame-hook (lambda () (desktop-read (emacsd "cache/default-desktop"))))
;;   (desktop-read (emacsd "cache/default-desktop")))
;; (add-hook 'server-after-make-frame-hook (lambda () (desktop-read (emacsd "cache/default-desktop"))))
;; (desktop-save-mode 1)
;; (unless daemon-mode-snapshot
;;   (add-hook 'kill-emacs-hook (lambda () (desktop-save (emacsd "cache/default-desktop")))))

;; Wrapper in case encoding is changed
(defun project-identifier-encode (name)
  (base64url-encode-string name))

(defun get-current-buffer-project-name ()
  (project-root (project-current)))

(defun get-full-desktop-path (identifier)
  (emacsd (concat "cache/" identifier)))

(defun save-project-desktop ()
  (interactive)
  (condition-case nil
      (let* ((project-identifier (project-identifier-encode (get-current-buffer-project-name) ))
             (project-desktop-path (get-full-desktop-path project-identifier)))
        (unless (file-exists-p project-desktop-path) (dired-create-directory project-desktop-path))
        (desktop-save project-desktop-path))
    (error (desktop-save (emacsd "cache/default-desktop")))))

(defun get-projects-in-cache ()
  (mapcar 'car
          (seq-filter (lambda (x) (file-exists-p (get-full-desktop-path (cdr x))))
                      (mapcar (lambda (pname) (cons pname (project-identifier-encode pname)))
                              (project-known-project-roots)))))

(defun read-project-desktop ()
  (interactive)
  (desktop-read
   (get-full-desktop-path
    (project-identifier-encode
     (completing-read "Choose project: " (get-projects-in-cache) (lambda (x) t) t))))
  (desktop-save-mode t))

(provide 'my-desktop)
