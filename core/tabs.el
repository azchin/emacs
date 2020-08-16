(setq custom-tab-width 2)
(defun disable-tabs () 
  (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (setq indent-tabs-mode t))
(setq backward-delete-char-untabify-method nil)

(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(setq-default indent-tabs-mode t)
(setq-default tab-width custom-tab-width)

(add-hook 'prog-mode-hook 'enable-tabs)
(add-hook 'text-mode-hook 'enable-tabs)

; Add hooks here to disable tabs as desired
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

; (global-whitespace-mode)
; (setq whitespace-style '(face tabs tab-mark trailing))
; (custom-set-faces
;  '(whitespace-tab ((t (:foreground "#636363")))))
; (setq whitespace-display-mappings
;   '((tab-mark 9 [124 9] [92 9])))

(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
      ; (call-interactively 'backward-delete-char)
    (let ((movement (% (current-column) custom-tab-width))
          (p (point)))
      (when (= movement 0) (setq movement custom-tab-width))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))
; (global-set-key (kbd "<backspace>") 'backspace-whitespace-to-tab-stop)
