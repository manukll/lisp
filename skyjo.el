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

(setq skyjo-header-str "

      S K Y J O

   +----+   +----+
   |////|   |    |
   |\\\\\\\\|   | %02d |
   |////|   |    |
   +----+   +----+")

(defvar skyjo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [up] 'skyjo-navigate-up)
    (define-key map [left] 'skyjo-navigate-left)
    (define-key map [right] 'skyjo-navigate-right)
    (define-key map [down] 'skyjo-navigate-down)
    ;; (define-key map "n" 'skyjo-next-player)
    (define-key map "r" 'skyjo-reveal-card)
    (define-key map "d" 'skyjo-draw-card-from-draw-pile)
    (define-key map "t" 'skyjo-draw-card-from-discard-pile)

    (define-key map [mouse-1] 'asdf)
    map))

(defun asdf (event)
  "test mouse events"
  (message event))

(define-derived-mode skyjo-mode nil "SKYJO"
  "Major mode for Skyjo game
  \\{skyjo-mode-map}"

  (setq buffer-read-only t
	show-trailing-whitespace nil))

(defun skyjo (number-of-players)
  "Play Skyjo"

  (interactive "nNumber of players: ")

  (require 'org-table)
  (require 'seq)

  (skyjo-new-game)
  (skyjo-setup-players)
  (skyjo-add-card-to-discard-pile (skyjo-draw-card-from-draw-pile))
  (switch-to-buffer (get-buffer-create "*skyjo*"))
  (skyjo-render-game)
  (skyjo-goto-player)
  (skyjo-mode))

(defun skyjo-new-game ()
  "Start a new game"

  ;; todo: new game without full reset (number of players, score, ...)

  (setq skyjo-draw-pile (skyjo--shuffle skyjo-cards)
	skyjo-discard-pile ()
	skyjo-players-visible-cards ()
	skyjo-players-hidden-cards ()
	skyjo-players number-of-players
	skyjo-players-marker '(14 24 34 44 54 64 74 84)
	skyjo-current-player 0
	skyjo-current-field 0
	skyjo-current-score ()))

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
  
  (let ((list list)
	(shuffled ()))
    (dotimes (i (length list))
      (let* ((elt (seq-random-elt list))
	     (idx (seq-position list elt)))
	
	(setq list (skyjo--delnth idx list))
	(push elt shuffled)))
    shuffled))

(defun skyjo-setup-players ()
  "Setup players with initial set of cards"
  
  (let ((nils '(nil nil nil nil nil nil nil nil nil nil nil nil)))
    (dotimes (i skyjo-players)
      (push (skyjo-receive-cards) skyjo-players-hidden-cards)
      (push nils skyjo-players-visible-cards)
      (push 0 skyjo-current-score))))

(defun skyjo-draw-card-from-draw-pile ()
  "Return a card from the draw pile"

  (interactive)

  (pop skyjo-draw-pile))

(defun skyjo-draw-card-from-discard-pile ()
  "Return a card from the discard pile"

  (interactive)

  (let* ((card (pop skyjo-discard-pile))
	 (hidden-cards (nth skyjo-current-player skyjo-players-hidden-cards))
	 (visible-cards (nth skyjo-current-player skyjo-players-visible-cards))
	 (hidden-card (nth skyjo-current-field hidden-cards))
	 (visible-card (nth skyjo-current-field visible-cards)))


    ;; something something here, look here

    
    (when card
      (when visible-card
	(setq skyjo-players-visible-cards (skyjo--setnth skyjo-current-player visible-cards skyjo-players-visible-cards)))
      (when hidden-card
	(setq skyjo-players-hidden-cards (skyjo--setnth skyjo-current-player hidden-cards skyjo-players-hidden-cards))))))

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

  (interactive)
  
  (if (= (+ skyjo-current-player 1) skyjo-players)
      (setq skyjo-current-player 0)
    (setq skyjo-current-player (1+ skyjo-current-player)))

  (skyjo-render-game)
  (skyjo-goto-player))

(defun skyjo-goto-player ()
  "Goto player's table marker"

  (setq skyjo-current-field 0)
  (goto-line (nth skyjo-current-player skyjo-players-marker))
  (org-table-next-field))

(defun skyjo-reveal-card ()
  "Reveal a player's hidden card"

  (interactive)

  (let* ((cards (nth skyjo-current-player skyjo-players-hidden-cards))
	 (card (nth skyjo-current-field cards))
	 (hidden-cards (skyjo--setnth skyjo-current-field nil cards))
	 (visible-cards (skyjo--setnth skyjo-current-field card (nth skyjo-current-player skyjo-players-visible-cards))))

    (when card
      (setq skyjo-players-hidden-cards (skyjo--setnth skyjo-current-player hidden-cards skyjo-players-hidden-cards))
      (setq skyjo-players-visible-cards (skyjo--setnth skyjo-current-player visible-cards skyjo-players-visible-cards))

      (skyjo-render-game)
      (skyjo-next-player))))

(defun skyjo-navigate-up ()
  "Navigate table field up"

  (interactive)
  
  (let* ((cards (nth skyjo-current-player skyjo-players-hidden-cards))
	 (field skyjo-current-field)
	 (prev (cond ((= (length cards) 12) 4)
		     ((= (length cards) 9) 3)
		     ((= (length cards) 6) 2)
		     ((= (length cards) 3) 1))))

    (when (>= (- field prev) 0)
      (setq skyjo-current-field (- field prev))
      (dotimes (i prev)
	(org-table-previous-field)))))
    
(defun skyjo-navigate-left ()
  "Navigate table field left"
  
  (interactive)

  (let* ((cards (nth skyjo-current-player skyjo-players-hidden-cards))
	 (field skyjo-current-field))

    (when (>= (- field 1) 0)
      (setq skyjo-current-field (- field 1))
      (org-table-previous-field))))

(defun skyjo-navigate-right ()
  "Navigate table field right"
  
  (interactive)

  (let* ((cards (nth skyjo-current-player skyjo-players-hidden-cards))
	 (field skyjo-current-field))

    (when (< (+ field 1) (length cards))
      (setq skyjo-current-field (+ field 1))
      (org-table-next-field))))

(defun skyjo-navigate-down ()
  "Navigate table field down"
  
  (interactive)

  (let* ((cards (nth skyjo-current-player skyjo-players-hidden-cards))
	 (field skyjo-current-field)
	 (next (cond ((= (length cards) 12) 4)
		     ((= (length cards) 9) 3)
		     ((= (length cards) 6) 2)
		     ((= (length cards) 3) 1))))

    (when (< (+ field next) (length cards))
      (setq skyjo-current-field (+ field next))
      (dotimes (i next)
	(org-table-next-field)))))

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

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format skyjo-header-str (car skyjo-discard-pile)))
    (newline 2)

    (dotimes (i (length skyjo-players-hidden-cards))
      (insert (format "   Player %d (%d/%d)%s\n\n" (+ i 1)
		      (skyjo-visible-score i)
		      (nth i skyjo-current-score)
		      (if (= i skyjo-current-player) "*" "")))
      
      (org-table-create "4x4")
      (kill-line 2)
      (org-table-insert-hline t)
      (org-table-next-field)

      (let ((hidden-cards (nth i skyjo-players-hidden-cards))
	    (visible-cards (nth i skyjo-players-visible-cards)))
	(dotimes (ii (length hidden-cards))
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
      (newline))))


