(setq desktop-path `(,(emacsd "cache/default-desktop/")))
(setq desktop-load-locked-desktop t)
;; (setq desktop-restore-frames nil)
;; (setq desktop-restore-reuses-frames 'keep)
;; (setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

(defun server-frame-desktop ()
  (desktop-save-mode nil)
  (desktop-read (emacsd "cache/default-desktop"))
  (remove-hook 'server-after-make-frame-hook 'server-frame-desktop))
;; (when daemon-mode-snapshot (add-hook 'server-after-make-frame-hook 'server-frame-desktop))
(if daemon-mode-snapshot
    (add-hook 'server-after-make-frame-hook 'server-frame-desktop)
  (desktop-save-mode 1))

;; (if daemon-mode-snapshot
;;     (add-hook 'server-after-make-frame-hook (lambda () (desktop-read (emacsd "cache/default-desktop"))))
;;   (desktop-read (emacsd "cache/default-desktop")))
;; (add-hook 'server-after-make-frame-hook (lambda () (desktop-read (emacsd "cache/default-desktop"))))
;; (desktop-save-mode 1)
;; (unless daemon-mode-snapshot
;;   (add-hook 'kill-emacs-hook (lambda () (desktop-save (emacsd "cache/default-desktop")))))
