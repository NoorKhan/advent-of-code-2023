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
	       (card-id (cadr (uiop:split-string (car split-line))))
	       (numbers (uiop:split-string (cadr split-line) :separator "|"))
	       (winning-numbers (parse-numbers (car numbers)))
	       (your-numbers (parse-numbers (cadr numbers)))
	       (matches (fset:size (fset:intersection winning-numbers your-numbers))))
	  (print winning-numbers)
	  (print your-numbers)
	  (format t "card-id: ~a winnings numbers: ~a your numbers: ~a~%" card-id winning-numbers your-numbers)
	  (format t "matches in card id: ~a: ~a~%" card-id matches)
	  (when (> matches 0) (setf points (+ points (expt 2 (- matches 1))))))))
    points))

(get-points "test input.txt")
(get-points "input.txt")
(fset:size (fset:intersection (fset:set 1 2 3) (fset:set 1 2 4)))

(fset:contains? (fset:set 1 2 3) 7)

(fset:union (fset:set '(1 2 3) '(1 2 3) '(1 2 4)) (fset:set '(1 2 3) '(1 2 3) '(1 8 4)))
(fset:set-fset '(1 2 3))

(expt 2 3)
(to-set '(1 3 2 3))

(defun parse-numbers (numbers)
  (to-set (map 'list
	       #'parse-integer
	       (remove-if-not
		#'(lambda (x) (parse-integer x :junk-allowed t))
		(uiop:split-string (string-trim '(#\Space #\Tab #\Newline #\Return) numbers))))))

(defun to-set (list)
  (reduce (lambda (x y) (fset:union x (fset:set y))) list :initial-value (fset:empty-set)))

(search ":" "Card 1:")
(cadr (uiop:split-string "A:B" :separator ":"))
(subseq "abc" 1)

(remove-if #'(lambda (x) (parse-integer x)) '("1" "a" "2"))
