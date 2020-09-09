(setq 
  backup-by-copying t
  ; my-backup-dir (emacsd "cache/backups")
  backup-directory-alist `(("." . ,(emacsd "cache/backups")))
  auto-save-file-name-transforms `((".*" ,(emacsd "cache/saves/") t))
  auto-save-default t
  ;; auto-save-visited-mode t
  create-lockfiles nil
  version-control t
  delete-old-versions t
  kept-new-versions 2
  kept-old-versions 2)
