(asdf:load-system :uiop)
(ql:quickload "fset")

(defun get-points (input-file)
  (let ((points 0))
    (with-open-file (stream input-file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
	       (split-line (uiop:split-string trimmed-line :separator ":"))
	       (card-id (parse-integer (cadr (uiop:split-string (car split-line)))))
	       (numbers (uiop:split-string (cadr split-line) :separator "|"))
	       (winning-numbers (parse-numbers (car numbers)))
	       (your-numbers (parse-numbers (cadr numbers)))
	       (matches (fset:size (fset:intersection winning-numbers your-numbers))))
	  (print winning-numbers)
	  (print your-numbers)
	  (format t "card-id: ~a winnings numbers: ~a your numbers: ~a~%" card-id winning-numbers your-numbers)
	  (format t "matches in card id ~a: ~a~%" card-id matches)
	  (when (> matches 0) (setf points (+ points (expt 2 (- matches 1))))))))
    points))

(get-points "test input.txt")
(get-points "input.txt")

(defun get-total-cards (input-file)
  (let ((total-cards 0)
	(cards-to-check '())
	(card-map (make-hash-table)))
    (with-open-file (stream input-file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
	       (split-line (uiop:split-string trimmed-line :separator ":"))
	       (card-id (parse-integer (car (last (uiop:split-string
					      (string-trim '(#\Space #\Tab #\Newline #\Return)
							   (car split-line)))))))
	       (numbers (uiop:split-string (cadr split-line) :separator "|"))
	       (winning-numbers (parse-numbers (car numbers)))
	       (your-numbers (parse-numbers (cadr numbers)))
	       (matches (fset:size (fset:intersection winning-numbers your-numbers))))
	  (setf (gethash card-id card-map) (cons winning-numbers your-numbers))
	  (setf cards-to-check (append cards-to-check (list card-id))))))
    (loop while (> (length cards-to-check) 0) do
      (print cards-to-check)
      (let* ((card-id (car cards-to-check))
	     (numbers (gethash card-id card-map))
	     (matches (fset:size (fset:intersection (car numbers) (cdr numbers)))))
	(incf total-cards)
	(when (> matches 0)
	  (loop for i from 1 to matches
		for next-card-id = (+ card-id i) do
	    (when (gethash next-card-id card-map)
	      (setf cards-to-check (insert-after
				    cards-to-check
				    (position next-card-id cards-to-check)
				    next-card-id)))))
	(setf cards-to-check (cdr cards-to-check))))
    total-cards))

(print (get-total-cards "test input.txt"))
(print (get-total-cards "input.txt"))

(nthcdr 1 '(1 2 3))
(position 7 '(52 2 2 2 9 1 2))

(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst))) 
  lst)

(defun parse-numbers (numbers)
  (to-set (map 'list
	       #'parse-integer
	       (remove-if-not
		#'(lambda (x) (parse-integer x :junk-allowed t))
		(uiop:split-string (string-trim '(#\Space #\Tab #\Newline #\Return) numbers))))))

(defun to-set (list)
  (reduce (lambda (x y) (fset:union x (fset:set y))) list :initial-value (fset:empty-set)))

