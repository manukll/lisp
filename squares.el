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

(setq board '(((t nil nil t))))
(setq board '(((t nil nil t) (t nil nil nil))))

(defun squares--insert-top ())
(defun squares--insert-left-right ())
(defun squares--insert-bottom ())

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

	    (when col

	      ;; (if (= ii 0)
	      ;; 	  (newline)
	      ;; 	(forward-line))
	      		      
	      (if (or top
		      (and top right bottom left))
		  (if (string-equal "+" (string (preceding-char)))
		      (insert "---+")
		    (insert "+---+"))
		(if (string-equal "+" (string (preceding-char)))
		    (insert "   ")
		  (insert "     ")))

	      (if (= ii 0)
		  (newline)
		(forward-line))

	      (if (or left
		      (and top right bottom left))
		  (if (string-equal "|" (string (preceding-char)))
		      (insert "  ")
		    (insert "| ")))

	      (if (and top right bottom left)
		  (insert "x")
		(insert " "))
	     
	      (if (or right
		      (and top right bottom left))
		  (insert " |")
		(insert "  "))

	      (if (= ii 0)
		  (newline)
		(forward-line))

	      (if (or bottom
		      (and top right bottom left))
		  (if (string-equal "+" (string (preceding-char)))
		      (insert "---+")
		    (insert "+---+"))
		(if (string-equal "+" (string (preceding-char)))
		    (insert "   ")
		  (insert "     "))))

	    (forward-line -2)
	    (end-of-line)
	    (backward-char)
	    
	    ;; (unless col ...

	  (setq ii (1+ ii))))
	(setq i (1+ i))))))
     







+---+---++
   |    
                    
