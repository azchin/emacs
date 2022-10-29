(setq custom-tab-width 4)

(defun toggle-tabs () (interactive)
       (setq indent-tabs-mode (not indent-tabs-mode)))

(defun set-tab-width (width)
  (setq tab-width width)
  (setq evil-shift-width width))

(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

(setq-default backward-delete-char-untabify-method nil)
(evil-global-set-key 'insert (kbd "<backspace>") 'backspace-whitespace-to-tab-stop)

(setq-default indent-tabs-mode nil)
(setq-default tab-width custom-tab-width)
(setq-default evil-shift-width custom-tab-width)
(setq tab-always-indent nil)
(setq c-tab-always-indent 'string)
(setq python-indent-offset 4)
(setq sh-basic-offset custom-tab-width)
;; (setq c-basic-offset custom-tab-width
;;       c-default-style "linux"
;;       c-syntactic-indentation nil)
(setq c-basic-offset custom-tab-width)
(c-set-offset 'case-label 0)
(c-set-offset 'label '*)
(setq css-indent-offset 2)

(add-hook 'python-mode-hook (lambda () (set-tab-width python-indent-offset)))
(add-hook 'js2-mode-hook (lambda () (set-tab-width 2)))
(add-hook 'json-mode-hook (lambda () (set-tab-width 2)))
(add-hook 'web-mode-hook (lambda () (set-tab-width 2)))
(add-hook 'css-mode-hook (lambda () (set-tab-width css-indent-offset)))

(global-whitespace-mode)
(setq whitespace-style '(face tabs tab-mark))
(setq whitespace-display-mappings
      '((tab-mark 9 [183 9] [124 9] [92 9]))) ;; 187 183 8594 9655 8677 8614

(delete-selection-mode)

(defun conditional-tabify ()
  (interactive)
  (if indent-tabs-mode
      (tabify (window-start) (window-end))
    (untabify (window-start) (window-end))))

(defun indent-whole-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun fill-whole-buffer ()
  (interactive)
  (fill-region (point-min) (point-max)))
