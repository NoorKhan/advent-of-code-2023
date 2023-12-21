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
	(card-map (make-hash-table))
	(card-id-count-map (make-hash-table)))
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
	       (your-numbers (parse-numbers (cadr numbers))))
	  (setf (gethash card-id card-map) (cons winning-numbers your-numbers))
	  (setf cards-to-check (append cards-to-check (list card-id))))))
    (loop for card-id in cards-to-check do
	 (let* ((numbers (gethash card-id card-map))
		(matches (fset:size (fset:intersection (car numbers) (cdr numbers)))))
	   (add-card-copy card-id card-id-count-map)
	   (let ((card-id-count (gethash card-id card-id-count-map)))
	     (loop for i from 1 to card-id-count do
		  (loop for j from 1 to matches
		     for next-card-id = (+ card-id j) do
		       (when (gethash next-card-id card-map)
			 (add-card-copy next-card-id card-id-count-map)))))))
    (loop for v being the hash-value of card-id-count-map do (setf total-cards (+ total-cards v)))
    total-cards))

(defun add-card-copy (card-id card-id-count-map)
  (if (gethash card-id card-id-count-map)
      (setf (gethash card-id card-id-count-map) (+ 1 (gethash card-id card-id-count-map)))
      (setf (gethash card-id card-id-count-map) 1)))

(get-total-cards "test input.txt")
(get-total-cards "input.txt")

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

