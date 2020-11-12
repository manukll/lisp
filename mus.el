;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with SPC f f and enter text in its buffer.

(require 'json)

(defun foo ()
  (interactive)

  (let* ((command "beet export -l -i id,album,artist,title,path")
	 (output (shell-command-to-string command))
	 (json-array-type 'list))
    (json-read-from-string output)))

(defun bar ()
  (interactive)

  (org-table-create "5x2")
  (org-table-next-field)

  (dolist elt (foo)
	  (org-table-next-field)))

(nth 2 (foo))

;; (defun foo ()
;;   (interactive)

;;   (let* ((command "beet export -l -i id,album,artist,title,path")
;; 	 (output (shell-command-to-string command))
;; 	 (json-array-type 'list)
;; 	 (json (json-read-from-string output)))
;;     json))

;; (aref (foo) 0)

;; (assoc 'title (aref (foo) 0))

;; (alist-get 'title (aref (foo) 0))

;; (print (foo))



;; (defun bar ()
;;   (interactive)

;;   (org-table-create "5x2")
;;   (org-table-next-field)


;;   (dolist elt (foo)
;; 	  (org-table-next-field)))
