;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


;; TODO: randomize cards and get 12 elements

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

(setq skyjo-staple '())
(setq skyjo-players '())

(defun skyjo ()
  "Start a new game"
  (interactive)

  (get-buffer-create "skyjo")
  (switch-to-buffer "skyjo")
  ;; (buffer-read-only)
  ;; (read-only-mode)
  ;; (barf-if-buffer-read-only)
  (erase-buffer)

  (skyjo-render-game))

(defun skyjo-draw-card ()
  "Draw a card from the deck"
  (seq-random-elt skyjo-cards))

(defun skyjo-take-card ()
  "Take the card from the staple"
  (car skyjo-staple))

(defun skyjo-give-cards ()
  "Return the first 12 cards at the beginning of the game"

  ;; (seq-take skyjo-cards 12)
  ;; (setq skyjo-cards (seq-drop skyjo-cards 12))

  (let ((i 0)
	(cards ()))
    (while (< i 12)
      (push (skyjo-draw-card) cards)
      (setq i (1+ i)))
    (list cards)))

(defun skyjo-render-game ()
  "Render a table like game using org-table-create"

  ;; org-table-get-field

  (org-table-create "4x4")
  (kill-line 2)
  (org-table-next-field))



