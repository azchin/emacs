(require 'evil)

(defvar custom-tab-width 4)

(defun toggle-tabs () (interactive)
       (setq indent-tabs-mode (not indent-tabs-mode)))

(defun set-tab-width (width)
  (setq tab-width width)
  (setq evil-shift-width width))

;; https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
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

(defvar editor-mode-maps (list prog-mode-map text-mode-map))
(defun keymap-set-editor-key (key function)
  (dolist (map-it editor-mode-maps)
    (evil-define-key 'insert map-it (kbd key) function)))

;; Overwriting global backspace causes issues when package remaps function (e.g. ivy)
(keymap-set-editor-key "<backspace>" 'backspace-whitespace-to-tab-stop)

(defun backspace-delete-word-or-whitespace ()
  "Deletes previous word, or whitespace until previous word if whitespace is long"
  (interactive)
  (if (and (not (region-active-p))
           (or (equal (char-before) ?\n)
               (and (member (char-before) '(?\s ?\t))
                    (member (char-before (- (point) 1)) '(?\s ?\t)))))
      (let ((backward-delete-char-untabify-method 'hungry))
        (call-interactively 'backward-delete-char-untabify))
    (call-interactively 'backward-kill-word)))

(keymap-set-editor-key "C-<backspace>" 'backspace-delete-word-or-whitespace)
;; Alternative is to use Vim's "db"
;; (evil-define-key 'insert 'global (kbd "C-<backspace>") 'evil-delete-backward-word)

(setq-default indent-tabs-mode nil)
(setq-default backward-delete-char-untabify-method nil)
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
(setopt c-default-style "stroustrup")
(setopt c-ts-mode-indent-offset c-basic-offset)
(setopt c-ts-mode-indent-style 'k&r)
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
      (tabify (point-min) (point-max))
    (untabify (point-min) (point-max))))

(defun indent-whole-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun fill-whole-buffer ()
  (interactive)
  (fill-region (point-min) (point-max)))

(defun fundamental-to-text-mode ()
  (when (equal major-mode 'fundamental-mode)
    (text-mode)))
(add-hook 'find-file-hook 'fundamental-to-text-mode)

(provide 'my-tabs)
