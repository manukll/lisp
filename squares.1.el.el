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

(defun render-board ()
  "Render game board"
  
  (interactive)
  
  (let ((row 0))

    ;; for each row
    (while (< row (length board))
      
      (let ((col 0)
	    (row (nth row board)))

	;; for each column
	(while (<= col (length (nth col row)))

	  (let ((side 0)
		(col (nth col row)))

	    ;; for each side
	    (while (< side (length col))

	      (let ((side (nth side col)))
		(if side
		    (insert "-")
		  (insert "x")))
	      
	      (setq side (+ side 1))))

	  (insert "-")
	  (setq col (+ col 1))))
      
      (insert "\n")
      (setq row (+ row 1)))))





  
















































