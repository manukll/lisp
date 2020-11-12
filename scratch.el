;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


(require 'svg)


(defun skyjo-render-card (card &optional disabled)
  "Render card with scalable vector graphics"

  (let* ((width 66)
	 (height 99)
	 (padding 6)
	 (margin 8)
	 (border-color "gray")
	 (x (if (= (length card) 2) 20 25))
	 (y 60)
	 (svg (svg-create width height))
	 (number (if card (string-to-number card) ""))
	 (color (when card (cond ((< number 0) "slateblue")
				 ((= number 0) "deepskyblue")
				 ((< number 5) "limegreen")
				 ((< number 9) "yellow")
				 (t "red")))))

    ;; border
    (svg-rectangle svg 0 0 width height
		   :fill-color "white"
		   :stroke-color border-color)


    (svg-gradient svg "rainbow" 'radial '((0 . "red") (25 . "green") (50 . "blue") (100 . "yellow")))
    (svg-circle svg 99 50 20 :gradient "rainbow")


    (if card
	(progn
	  (when (string-empty-p card)
	    ;; (svg-gradient svg "rainbow" 'radial '((0 . red) (25 . green) (50 . blue) (100 . yellow)))
	    ;; (svg-circle svg 99 50 20 :gradient "rainbow"))
	    )

	  (unless (string-empty-p card)
	    (svg-rectangle svg padding padding
			   (- width (* 2 padding))
			   (- height (* 2 padding))
			   :fill-color color)

	    (svg-gradient svg "highlight" 'radial `((0 . white) (100 . ,color)))
	    (svg-circle svg 33 50 20 :gradient "highlight")

	    (svg-text svg card
		      :font-size "32" :font-weight "bold" :font-family "impact"
		      :fill "black" :stroke "white" :stroke-width 2
		      :x x :y y)))
      (progn
	(svg-rectangle svg padding padding
		       (- width (* 2 padding))
		       (- height (* 2 padding))
		       :stroke-color border-color
		       :fill "white")
	
	(svg-line svg padding padding
		  (- width padding)
		  (- height padding)
		  :stroke-color border-color)
	
	(svg-line svg
		  (- width padding) padding
		  padding (- height padding)
		  :stroke-color border-color)))

    (when disabled
      (svg-rectangle svg 0 0 width height
		     :fill-color "white"
		     :opacity "0.66"))
    
    (insert-image (svg-image svg :margin margin :pointer 'hand))))


(defun foo ()
  (interactive)

  (skyjo-render-card "")
  (skyjo-render-card nil))

(defun skyjo--list-cards-display ()
  (interactive)

  (let ((numbers '(-2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12)))

    (dolist (elt numbers)
      (skyjo-render-card (number-to-string elt))

      (when (or (= elt 0)
		(= elt 4)
		(= elt 8)
		(= elt 12))
	(newline)))))


    (defun bar ()
      (interactive)
      (print (org-element-parse-buffer)))

    (defun foo ()
      (interactive)
      (pp (org-element-at-point))
    
      (defun foo ()
        (interactive)
        (message (prin1-to-string (org-element-parse-buffer))))
