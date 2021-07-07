(setq custom-tab-width 4)
(defun choose-tabs (tabs &optional arg-width)
  (setq tab-width (or arg-width custom-tab-width))
  (setq evil-shift-width tab-width)
  (setq indent-tabs-mode tabs))
(defun disable-tabs (&optional arg-width) 
  (funcall 'choose-tabs nil arg-width))
(defun enable-tabs  (&optional arg-width)
  (funcall 'choose-tabs t arg-width))

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
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

;; TODO convert manual-tabs into a minor mode
(defvar tab-control-auto nil)
(setq backward-delete-char-untabify-method nil)
(evil-define-key 'insert 'global
  (kbd "<backspace>") 'backspace-whitespace-to-tab-stop
  (kbd "TAB") 'tab-to-tab-stop)
;; (defvar tab-control-auto t)
;; (setq backward-delete-char-untabify-method 'hungry)
;; (evil-define-key 'insert 'global
;;   (kbd "<backspace>") 'backward-delete-char-untabify
;;   (kbd "TAB") 'indent-for-tab-command)

(defun auto-tabs ()
  (set (make-local-variable 'tab-control-auto) t)
  (set (make-local-variable 'backward-delete-char-untabify-method) 'hungry)
  (evil-define-key 'insert 'local
    (kbd "<backspace>") 'backward-delete-char-untabify
    (kbd "TAB") 'indent-for-tab-command))
(defun manual-tabs ()
  (set (make-local-variable 'tab-control-auto) nil)
  (set (make-local-variable 'backward-delete-char-untabify-method) nil)
  (evil-define-key 'insert 'local
    (kbd "<backspace>") 'backspace-whitespace-to-tab-stop
    (kbd "TAB") 'tab-to-tab-stop))
(defun toggle-tabs () (interactive)
  (if tab-control-auto
      (funcall 'manual-tabs)
    (funcall 'auto-tabs)))

(defun c-tabs()
  (set (make-local-variable 'backward-delete-char-untabify-method) nil)
  (evil-define-key 'insert 'local
    (kbd "<backspace>") 'backspace-whitespace-to-tab-stop
    (kbd "TAB") 'c-indent-command))

(setq-default indent-tabs-mode t)
(setq-default tab-width custom-tab-width)
(setq-default evil-shift-width custom-tab-width)
(setq python-indent-offset 4)
(setq sh-basic-offset custom-tab-width)
(setq c-basic-offset custom-tab-width
      c-default-style "linux"
      c-syntactic-indentation nil)


(add-hook 'prog-mode-hook 'disable-tabs)
(add-hook 'special-mode-hook 'enable-tabs)
(add-hook 'text-mode-hook 'disable-tabs)
;; (add-hook 'text-mode-hook 
;;  (lambda () (disable-tabs) (setq indent-line-function (quote insert-tab)))) 

;; Add hooks here to disable tabs as desired
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)
(add-hook 'python-mode-hook (lambda () (disable-tabs python-indent-offset)))
(add-hook 'org-mode-hook 'disable-tabs)
(add-hook 'js2-mode-hook (lambda () (disable-tabs 2)))
(add-hook 'json-mode-hook (lambda () (disable-tabs 2)))

;; (add-hook 'c-mode-hook 'c-tabs)
;; (add-hook 'c++-mode-hook 'c-tabs)


;; Add hooks here to set manual vs automatic tabs
(add-hook 'text-mode-hook 'manual-tabs)
(add-hook 'prog-mode-hook 'auto-tabs)
(add-hook 'special-mode-hook 'manual-tabs)

(add-hook 'lisp-mode-hook 'auto-tabs)
(add-hook 'emacs-lisp-mode-hook 'auto-tabs)
(add-hook 'org-mode-hook 'auto-tabs)

;; (setq-default electric-indent-inhibit t)

(global-whitespace-mode)
(setq whitespace-style '(face tabs tab-mark))
(setq whitespace-display-mappings
  '((tab-mark 9 [183 9] [124 9] [92 9]))) ;; 187 183 8594 9655 8677 8614

(defun conditional-tabify ()
  (interactive)
  (if indent-tabs-mode
      (tabify (window-start) (window-end))
    (untabify (window-start) (window-end))))

(defun indent-whole-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
