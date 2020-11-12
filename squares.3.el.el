;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; Sample board
;;
;; +---+---+---+
;; |           |
;; +           +
;; |           |
;; +           +
;; |           |
;; +---+---+---+

(setq board '(((t nil nil t) (t nil nil nil) (t t nil nil))
	      ((nil nil nil t) (nil nil nil nil) (nil t nil nil))
	      ((nil nil t t) (nil nil t nil) (nil t t nil))))

(defun squares-render-board ()
  "Render game board"

  (interactive)
  (let ((i 0))

    (while (< i (length board))
      (let ((ii 0)
	    (row (nth i board)))

	(while (<= ii (length (nth ii row)))
	  (let* ((col (nth ii row))
		 (top (nth 0 col))
		 (right (nth 1 col))
		 (bottom (nth 2 col))
		 (left (nth 3 col)))

	    (if col
		(if top
		    (if (string-equal "+" (string (preceding-char)))
			(insert "---")
		      (insert "+---+"))))

	  ;; (insert "-")
	  (setq ii (1+ ii))))
      
	;; (insert "\n")
	(setq i (1+ i))))))

























