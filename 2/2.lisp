(defparameter *bag* (make-hash-table :test 'equal))
(setf (gethash "red" *bag*) 12
      (gethash "green" *bag*) 13
      (gethash "blue" *bag*) 14)

(defun get-valid-game-ids (input-file bag)
  (let ((valid-game-ids '()))
    (with-open-file (stream input-file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(let* ((colon-index (search ":" line))
	       (game-id (parse-integer (subseq line (+ 1 (search " " line)) colon-index))))
	  (when (loop with subsets = (uiop:split-string (subseq line (+ 1 colon-index)) :separator ";")
		   for subset in subsets
		   always (valid-subset-p subset bag))
	    (setf valid-game-ids (append (list game-id) valid-game-ids))))))
    valid-game-ids))

(defun valid-subset-p (subset bag)
  (loop with cubes = (uiop:split-string subset :separator ",")
     for cube in cubes
     for cube-info = (uiop:split-string (string-trim '(#\Space #\Tab #\Newline #\Return) cube))
     for cube-count = (parse-integer (car cube-info))
     for cube-color = (cadr cube-info)
     for count-in-bag = (gethash cube-color bag)
     always (and count-in-bag (>= count-in-bag cube-count))))

;; part 1
(reduce #'+ (get-valid-game-ids "input.txt" *bag*))

(defun get-game-powers (input-file)
  (let ((game-powers '()))
    (with-open-file (stream input-file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(let* ((min-bag (make-hash-table :test 'equal)))
	  (loop with colon-index = (search ":" line)
	     with subsets = (uiop:split-string (subseq line (+ 1 colon-index)) :separator ";")
	     for subset in subsets
	     do (update-min-bag subset min-bag))
	  (setf game-powers (append (list (get-game-power min-bag)) game-powers)))))
    game-powers))

(defun get-game-power (min-bag)
  (reduce #'* (loop for value being the hash-values of min-bag collect value)))

(defun update-min-bag (subset min-bag)
  (loop with cubes = (uiop:split-string subset :separator ",")
     for cube in cubes
     for cube-info = (uiop:split-string (string-trim '(#\Space #\Tab #\Newline #\Return) cube))
     for cube-count = (parse-integer (car cube-info))
     for cube-color = (cadr cube-info)
     for count-in-bag = (gethash cube-color min-bag)
     do (when (or (null count-in-bag) (< count-in-bag cube-count))
	  (setf (gethash cube-color min-bag) cube-count)))
  min-bag)

;; part 2
(reduce #'+ (get-game-powers "input.txt"))

