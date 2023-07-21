(provide 'my-skeleton)

;; https://www.emacswiki.org/emacs/SkeletonMode

(defvar *skeleton-markers* nil
  "Markers for locations saved in skeleton-positions")

(add-hook 'skeleton-end-hook 'skeleton-make-markers)
(add-hook 'skeleton-end-hook 'evil-insert-state)

(defun skeleton-make-markers ()
  (while *skeleton-markers*
    (set-marker (pop *skeleton-markers*) nil))
  (setq *skeleton-markers*
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defun skeleton-next-position (&optional reverse)
  "Jump to next position in skeleton.
         REVERSE - Jump to previous position in skeleton"
  (interactive "P")
  (let* ((positions (mapcar 'marker-position *skeleton-markers*))
         (positions (if reverse (reverse positions) positions))
         (comp (if reverse '> '<))
         pos)
    (when positions
      (if (catch 'break
            (while (setq pos (pop positions))
              (when (funcall comp (point) pos)
                (throw 'break t))))
          (goto-char pos)
        (goto-char (marker-position
                    (car *skeleton-markers*)))))))

(defvar my-skeleton-mode-map (make-sparse-keymap)
  "Indicates skeleton inserted keymap.")

(define-minor-mode my-skeleton-mode
  "Global minor mode for spooky skeletons.")

(keymap-global-set "C-c s" 'skeleton-next-position)
(keymap-set my-skeleton-mode-map "TAB" 'skeleton-next-position)
(keymap-set my-skeleton-mode-map "<shift-tab>" (lambda () (skeleton-next-position t)))
(keymap-set my-skeleton-mode-map "C-SPC" 'my-skeleton-mode)
;; (add-hook 'skeleton-end-hook (lambda () (my-skeleton-mode)))

(define-skeleton test-skel
  "This is just a test" "Prompt: "
  "if (" @ - ") {" \n @ _ \n "}" >
  )


