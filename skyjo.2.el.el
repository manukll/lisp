;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


;; TODO: shuffle list of cards
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

(setq skyjo-staple '(99))
(setq skyjo-players '())

(defun skyjo (players)
  "Start a new game"

  (interactive "nNumber of players: ")

  (barf-if-buffer-read-only)
  (get-buffer-create "skyjo")
  (switch-to-buffer "skyjo")
  (erase-buffer)

  (skyjo-setup-players players)
  ;; skyjo-add-card-to-pile
  (skyjo-render-game))

(defun skyjo-setup-players (players)
  "Setup players with initial set of cards"

  (let ((i 0))
    (while (< i players)
      (push (skyjo-give-cards) skyjo-players)
      (setq i (1+ i)))))

(defun skyjo-draw-card ()
  "Draw a card from the deck"

  (let ((card (car skyjo-cards)))
    (setq skyjo-cards (cdr skyjo-cards))
    card))

(defun skyjo-take-card ()
  "Take a card from the staple"

  (let ((card (car skyjo-staple)))
    (setq skyjo-staple (cdr skyjo-staple))
    card))

(defun skyjo-give-cards ()
  "Return 12 cards"

  (let ((cards (seq-take skyjo-cards 12)))
    (setq skyjo-cards (seq-drop skyjo-cards 12))
    cards))

(defun skyjo-next-player ()
  "End turn and switch player")

(defun skyjo-render-game ()
  "Render a table like game using org-table-create"

  ;; org-table-get-field
  ;; org-table-delete-column
  ;; org-table-field-info

  (insert (format "

      S K Y J O

   +----+  +----+
   |////|  |    |
   |\\\\\\\\|  | %02d |
   |////|  |    |
   +----+  +----+" (car skyjo-staple)))
  (newline 2)

  (let ((i 0))
    (while (< i (length skyjo-players))
      (org-table-create "4x4")
      (kill-line 2)
      (org-table-next-field)

      (let ((cards (nth i skyjo-players))
	    (ii 0))
	(while (< ii (length cards))
	  ;; (insert (format "%d" (nth ii cards)))
	  (insert "//")
	  (unless (= ii 11)
	    (org-table-next-field))
	  (setq ii (1+ ii)))

	(org-table-align)
	(end-of-buffer)
	(newline)

	(setq i (1+ i))))))
