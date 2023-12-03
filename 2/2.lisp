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
	      (game-id (parse-integer (subseq line (+ 1 (search " " line)) colon-index)))
	      (subsets (uiop:split-string (subseq line (+ 1 colon-index)) :separator ";"))
	      (valid t))

	 (loop for subset in subsets
	    while valid
	    do (setf valid (is-valid-subset subset bag)))

	 (if valid (setf valid-game-ids (append (list game-id) valid-game-ids))))))

    valid-game-ids))

(defun is-valid-subset (subset bag)
  (let ((cubes (uiop:split-string subset :separator ","))
	(valid t))

    (loop for cube in cubes
       while valid
       do (let* ((cube-info (uiop:split-string (string-trim '(#\Space #\Tab #\Newline #\Return) cube)))
		 (cube-count (parse-integer (car cube-info)))
		 (cube-color (cadr cube-info))
		 (count-in-bag (gethash cube-color bag)))

	    (setf valid (and count-in-bag (>= count-in-bag cube-count)))))

    valid))

;; part 1
(reduce #'+ (get-valid-game-ids "input.txt" *bag*))

