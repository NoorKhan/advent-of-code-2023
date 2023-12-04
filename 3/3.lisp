(defun initialize-engine-schematic (input-file)
  (let ((line-length nil)
	(line-count 0)
	(engine-schematic nil))
    (with-open-file (stream input-file)
     (do ((line (read-line stream nil)
		(read-line stream nil)))
	 ((null line))
       (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
	 (when (not line-length)
	   (setf line-length (length trimmed-line)))
	 (incf line-count))))
    (setf engine-schematic (make-array `(,line-length ,line-count)))
    (let ((current-line 0))
      (with-open-file (stream input-file)
       (do ((line (read-line stream nil)
		  (read-line stream nil)))
	   ((null line))
	 (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
	   (loop for i from 0 below (length trimmed-line)
		do (setf (aref engine-schematic current-line i) (aref trimmed-line i)))
	   (incf current-line)))))
    engine-schematic))

(defun get-valid-numbers (engine-schematic)
  (let ((valid-numbers '()))
    (destructuring-bind (n m) (array-dimensions engine-schematic)
      (let ((test-number "")
	    (test-indices '()))
	(loop for i from 0 below n do
	     (loop for j from 0 below m
		for current-char = (aref engine-schematic i j)
		for is-digit = (digit-char-p current-char) do
		  (cond (is-digit
			 (setf test-number (concatenate 'string test-number (string current-char))
			       test-indices (append test-indices (list j))))
			(t (when (and (> (length test-number) 0) (valid-number-p test-indices i (- n 1) (- m 1) engine-schematic test-number))
			     (setf valid-numbers (append valid-numbers (list (parse-integer test-number)))))
			   (setf test-number ""
				 test-indices '())))))))
    valid-numbers))

(defun valid-number-p (test-indices row row-count column-count engine-schematic test-number)
  (format t "test number: ~a test indices: ~a row: ~a~%" test-number test-indices row)
  (loop for index in test-indices thereis (adjacent-symbol-p index row row-count column-count engine-schematic)))

(defun adjacent-symbol-p (index row row-count column-count engine-schematic)
  (or
   ;;; left
   (and (> index 0) (not-digit-or-period-p (aref engine-schematic row (- index 1))))
   ;;; below
   (and (< row row-count) (not-digit-or-period-p (aref engine-schematic (+ row 1) index)))
   ;;; below to the left
   (and (> index 0) (< row row-count) (not-digit-or-period-p (aref engine-schematic (+ row 1) (- index 1))))
   ;;; right
   (and (< index column-count) (not-digit-or-period-p (aref engine-schematic row (+ index 1))))
   ;;; above
   (and (> row 0) (not-digit-or-period-p (aref engine-schematic (- row 1) index)))
   ;;; above to the right
   (and (< index column-count) (> row 0) (not-digit-or-period-p (aref engine-schematic (- row 1) (+ index 1))))
   ;;; below to the right
   (and (< index column-count) (< row row-count) (not-digit-or-period-p (aref engine-schematic (+ row 1) (+ index 1))))
   ;;; above to the left
   (and (> index 0) (> row 0) (not-digit-or-period-p (aref engine-schematic (- row 1) (- index 1))))))

(defun not-digit-or-period-p (c)
  (not (or (digit-char-p c) (char= c #\.))))

;;; part 1
(defparameter *engine-schematic* (initialize-engine-schematic "input.txt"))
(reduce #'+ (get-valid-numbers *engine-schematic*))

