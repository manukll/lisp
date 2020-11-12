;; skyjo.el


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

  (require 'org-table)
  (require 'seq)

  (interactive "nNumber of players: ")
  (setq skyjo-draw-pile (skyjo--shuffle skyjo-cards)
	skyjo-discard-pile ()
	skyjo-players-visible-cards ()
	skyjo-players-hidden-cards ()
	skyjo-players number-of-players
	skyjo-current-player 0
	skyjo-current-score ())

  (skyjo-setup-players)
  (skyjo-add-card-to-discard-pile (skyjo-draw-card-from-draw-pile))
  (skyjo-render-game))

(defun skyjo-new-game ())

(defun skyjo-setup-players ()
  "Setup players with initial set of cards"
  
  (let ((nils '(nil nil nil nil nil nil nil nil nil nil nil nil)))
    (dotimes (i skyjo-players)
      (push (skyjo-receive-cards) skyjo-players-hidden-cards)
      (push nils skyjo-players-visible-cards)
      (push 0 skyjo-current-score)))

(defun skyjo-draw-card-from-draw-pile ()
  "Return a card from the draw pile"

  (pop skyjo-draw-pile))

(defun skyjo-draw-card-from-discard-pile ()
  "Return a card from the discard pile"

  (pop skyjo-discard-pile))

(defun skyjo-receive-cards ()
  "Return 12 cards from draw pile"
  
  (let ((cards (seq-take skyjo-draw-pile 12)))
    (setq skyjo-draw-pile (seq-drop skyjo-draw-pile 12))
    cards))

(defun skyjo-add-card-to-discard-pile (card)
  "Add a card to the discard pile"
  
  (push card skyjo-discard-pile))
  
(defun skyjo-vertical-row-p ())
(defun skyjo-vertical-row-merge ())

(defun skyjo-next-player ()
  "Switch current player"

  (if (= (+ skyjo-current-player 1) skyjo-players)
      (setq skyjo-current-player 0)
    (setq skyjo-current-player (1+ skyjo-current-player))))

(defun skyjo-reveal-card (idx)
  "Reveal a current player's hidden card"

  (let* ((cards (nth skyjo-current-player skyjo-players-hidden-cards))
	 (card (nth idx cards))
	 (hidden-cards (skyjo--setnth idx nil cards))
	 (visible-cards (skyjo--setnth idx card (nth skyjo-current-player skyjo-players-visible-cards))))

    (when card
      (setq skyjo-players-hidden-cards (skyjo--setnth skyjo-current-player hidden-cards skyjo-players-hidden-cards))
      (setq skyjo-players-visible-cards (skyjo--setnth skyjo-current-player visible-cards skyjo-players-visible-cards)))))

(defun skyjo-visible-score (idx)
  "Return visible score of player by index"
  
  (let ((score 0)
	(cards (nth idx skyjo-players-visible-cards)))
    (dotimes (i (length cards))
      (when (nth i cards)
	(setq score (+ score (nth i cards)))))
    score))

(defun skyjo-render-game ()
  "Render a table like game using org-table-create"

  ;; todo: set buffer read-only
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

  (dotimes (i (length skyjo-players-hidden-cards))
    (insert (format "    Player %d (%d/%d)\n" (+ i 1)
		    (skyjo-visible-score i)
		    (nth i skyjo-current-score))
    (newline 1)
      
    (org-table-create "4x4")
    (kill-line 2)
    (org-table-insert-hline t)
    (org-table-next-field)

    (let ((hidden-cards (nth i skyjo-players-hidden-cards))
	  (visible-cards (nth i skyjo-players-visible-cards)))
      (dotimes (ii (length cards))
	(let ((hidden-card (nth ii hidden-cards))
	      (visible-card (nth ii visible-cards)))
	  (if visible-card
	      (insert (format "%d" visible-card))
	    (insert "//")))
	(unless (= ii 11)
	  (org-table-next-field))
	(when (or (= ii 0)
		  (= ii 4)
		  (= ii 8))
	  (org-table-insert-hline))))

    (org-table-align)
    (end-of-buffer)
    (newline)))

(skyjo-reveal-card 3)
(skyjo-render-game)

