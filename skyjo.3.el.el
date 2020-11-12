;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


;; TODO: set read only buffer
(setq skyjo-cards '(-2 -2 -2 -2 -2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
		       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		       1 1 1 1 1 1 1 1 1 1
		       2 2 2 2 2 2 2 2 2 2
		       3 3 3 3 3 3 3 3 3 3
		       4 4 4 4 4 4 4 4 4 4
		       5 5 5 5 5 5 5 5 5 5
		       6 6 6 6 6 6 6 6 6 6
		       7 7 7 7 7 7 7 7 7 7
		       8 8 8 8 8 8 8 8 8 8
		       9 9 9 9 9 9 9 9 9 9
		       10 10 10 10 10 10 10 10 10 10
		       11 11 11 11 11 11 11 11 11 11
		       12 12 12 12 12 12 12 12 12 12))


;; org-table-get-field
;; org-table-delete-column
;; org-table-field-info

(defun skyjo--delnth (idx list)
  "Delete nth element of list"
  
  (let ((take (seq-take list idx))
	(drop (seq-drop list (+ idx 1))))
    (seq-concatenate 'list take drop)))

(defun skyjo--setnth (idx value list)
  "Set value of nth element of list"
  
  (let ((take (seq-take list idx))
	(drop (seq-drop list (+ idx 1))))
    (seq-concatenate 'list take (list value) drop)))

(defun skyjo--shuffle (list)
  "Shuffle a list"
  
  (interactive)

  (let ((list list)
	(shuffled ()))
    (dotimes (i (length list))
      (let* ((elt (seq-random-elt list))
	     (idx (seq-position list elt)))
	
	(setq list (skyjo--delnth idx list))
	(push elt shuffled)))
    shuffled))

(defun skyjo (number-of-players)
  "Start a new SKYJO game"

  (interactive "nNumber of players: ")
  (setq skyjo-draw-pile (skyjo--shuffle skyjo-cards)
	skyjo-discard-pile ()
	skyjo-players-visible-cards ()
	skyjo-players-hidden-cards ()
	skyjo-current-player 0
	skyjo-current-score ())

  (skyjo-setup-players number-of-players)
  (skyjo-add-card-to-discard-pile (skyjo-draw-card-from-draw-pile))
  (skyjo-render-game))

(defun skyjo-setup-players (number-of-players)
  "Setup players with initial set of cards"
  
  (let ((i 0))
    (while (< i number-of-players)
      (push (skyjo-receive-cards) skyjo-players-hidden-cards)
      (setq i (1+ i)))))

(defun skyjo-draw-card-from-draw-pile ()
  "Return a card from the draw pile"

  ;; use pop
  
  (let ((card (car skyjo-draw-pile)))
    (setq skyjo-draw-pile (cdr skyjo-draw-pile))
    card))

(defun skyjo-draw-card-from-discard-pile ()
  "Return a card from the discard pile"

  ;; use pop
  
  (let ((card (car skyjo-discard-pile)))
    (setq skyjo-discard-pile (cdr skyjo-discard-pile))
    card))

(defun skyjo-receive-cards ()
  "Return 12 cards from draw pile"
  
  (let ((cards (seq-take skyjo-draw-pile 12)))
    (setq skyjo-draw-pile (seq-drop skyjo-draw-pile 12))
    cards))

(defun skyjo-shuffle-cards ()
  "Shuffle deck of cards")

(defun skyjo-add-card-to-discard-pile (card)
  "Add a card to the discard pile"
  
  (push card skyjo-discard-pile))
  
(defun skyjo-vertical-row-p ())
(defun skyjo-vertical-row-merge ())

(defun skyjo-next-player ()
  "Switch current player")

(defun skyjo-reveal-card ()
  "Reveal a hidden player's card")

(defun skyjo-render-game ()
  "Render a table like game using org-table-create"

  (require 'org-table)

  (get-buffer-create "skyjo")
  (switch-to-buffer "skyjo")
  (barf-if-buffer-read-only)
  (erase-buffer)
  
  (insert (format "

      S K Y J O

   +----+   +----+
   |////|   |    |
   |\\\\\\\\|   | %02d |
   |////|   |    |
   +----+   +----+" (car skyjo-discard-pile)))
  (newline 2)

  (let ((i 0))
    (while (< i (length skyjo-players-hidden-cards))
      (insert (format "    Player %d (%d/%d)\n" (+ i 1) 0 0))
      (newline 1)

      (org-table-create "4x4")
      (kill-line 2)
      (org-table-insert-hline t)
      (org-table-next-field)
            
      (let ((cards (nth i skyjo-players-hidden-cards))
	    (ii 0))
	(while (< ii (length cards))
	  (insert (format "%d" (nth ii cards)))
	  ;; (insert "//")
	  (unless (= ii 11)
	    (org-table-next-field))
	  (when (or (= ii 0)
		    (= ii 4)
		    (= ii 8))
	    (org-table-insert-hline))
	  (setq ii (1+ ii)))

	(org-table-align)
	(end-of-buffer)
	(newline)

	(setq i (1+ i))))))
