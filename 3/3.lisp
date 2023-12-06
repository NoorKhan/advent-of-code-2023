(ql:quickload "fset")
(use-package :fset)

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
*backtrace-frame-count*      (with-open-file (stream input-file)
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
    (destructuring-bind (row-count column-count) (array-dimensions engine-schematic)
      (loop for i from 0 below row-count
	 for test-number = ""
	 for test-indices = '()
	 do (loop for j from 0 below column-count
	       for current-char = (aref engine-schematic i j)
	       for is-digit = (digit-char-p current-char) do
		 (cond (is-digit
			(setf test-number (concatenate 'string test-number (string current-char))
			      test-indices (append test-indices (list j)))
			(when (and (= j (- column-count 1)) test-indices (valid-number-p test-indices i row-count column-count engine-schematic test-number))
			  (setf valid-numbers (append valid-numbers (list (parse-integer test-number))))))
		       (t (when (and (> (length test-number) 0) (valid-number-p test-indices i row-count column-count engine-schematic test-number))
			    (setf valid-numbers (append valid-numbers (list (parse-integer test-number)))))
			  (setf test-number ""
				test-indices '()))))))
    valid-numbers))

(defun valid-number-p (test-indices row row-count column-count engine-schematic test-number)
  (format t "test number: ~a test indices: ~a row: ~a row-count: ~a column-count: ~a~%" test-number test-indices row row-count column-count)
  (loop for index in test-indices thereis (adjacent-symbol-p index row row-count column-count engine-schematic)))

(defun adjacent-symbol-p (index row row-count column-count engine-schematic)
  (or
   ;;; left
   (and (> index 0) (not-digit-or-period-p (aref engine-schematic row (- index 1))))
   ;;; below
   (and (< row (- row-count 1)) (not-digit-or-period-p (aref engine-schematic (+ row 1) index)))
   ;;; below to the left
   (and (> index 0) (< row (- row-count 1)) (not-digit-or-period-p (aref engine-schematic (+ row 1) (- index 1))))
   ;;; right
   (and (< index (- column-count 1)) (not-digit-or-period-p (aref engine-schematic row (+ index 1))))
   ;;; above
   (and (> row 0) (not-digit-or-period-p (aref engine-schematic (- row 1) index)))
   ;;; above to the right
   (and (< index (- column-count 1)) (> row 0) (not-digit-or-period-p (aref engine-schematic (- row 1) (+ index 1))))
   ;;; below to the right
   (and (< index (- column-count 1)) (< row (- row-count 1)) (not-digit-or-period-p (aref engine-schematic (+ row 1) (+ index 1))))
   ;;; above to the left
   (and (> index 0) (> row 0) (not-digit-or-period-p (aref engine-schematic (- row 1) (- index 1))))))

(defun not-digit-or-period-p (c)
  (not (or (digit-char-p c) (char= c #\.))))

(defparameter *engine-schematic* (initialize-engine-schematic "input.txt"))

;;; part 1
(reduce #'+ (get-valid-numbers *engine-schematic*))

;; part 2
(reduce #'+ (map 'list #'get-gear-ratio (get-valid-gear-numbers *engine-schematic*)))

(defun get-gear-ratio (gears)
  (fset:reduce (lambda (x y) (* (getf x :number) (getf y :number))) gears))

(defun get-valid-gear-numbers (engine-schematic)
  (let ((valid-gear-numbers '()))
    (destructuring-bind (row-count column-count) (array-dimensions engine-schematic)
      (loop for i from 0 below row-count do
	(loop for j from 0 below column-count
	      for current-char = (aref engine-schematic i j)
	      for is-gear-char = (gear-char-p current-char) do
		(when is-gear-char
		  (let ((surrounding-numbers
			  (get-surrounding-numbers i j row-count column-count engine-schematic)))
		    (format t "surrounding numbers: ~a~%" surrounding-numbers)
		    (when (= (fset:size surrounding-numbers) 2)
		      (setf valid-gear-numbers
			    (append valid-gear-numbers (list surrounding-numbers)))))))))
    valid-gear-numbers))

(defun gear-char-p (char)
  (char= char #\*))

(defun get-surrounding-numbers (row index row-count column-count engine-schematic)
  (let ((surrounding-numbers (fset:empty-set)))
;;; above
    (when (and (> row 0) (digit-char-p (aref engine-schematic (- row 1) index)))
      (setf surrounding-numbers
	    (fset:with surrounding-numbers
		       (get-number
			(- row 1)
			index
			row-count
			column-count
			engine-schematic))))
;;; above to the right
    (when (and (> row 0)
	       (< index (- column-count 1))
	       (digit-char-p (aref engine-schematic (- row 1) (+ index 1))))
      (setf surrounding-numbers
	    (fset:with surrounding-numbers
		       (get-number
			(- row 1)
			(+ index 1)
			row-count
			column-count
			engine-schematic))))
;;; to the right
    (when (and (< index (- column-count 1))
	       (digit-char-p (aref engine-schematic row (+ index 1))))
      (setf surrounding-numbers
	    (fset:with surrounding-numbers
		       (get-number
			row
			(+ index 1)
			row-count
			column-count
			engine-schematic))))
;;; below to the right
    (when (and (< row (- row-count 1))
	       (< index (- column-count 1))
	       (digit-char-p (aref engine-schematic (+ row 1) (+ index 1))))
      (setf surrounding-numbers
	    (fset:with surrounding-numbers
		       (get-number
			(+ row 1)
			(+ index 1)
			row-count
			column-count
			engine-schematic))))
;;; below
    (when (and (< row (- row-count 1))
	       (digit-char-p (aref engine-schematic (+ row 1) index)))
      (setf surrounding-numbers
	    (fset:with surrounding-numbers
		       (get-number
			(+ row 1)
			index
			row-count
			column-count
			engine-schematic))))
;;; below to the left
    (when (and (< row (- row-count 1))
	       (> index 0)
	       (digit-char-p (aref engine-schematic (+ row 1) (- index 1))))
      (setf surrounding-numbers
	    (fset:with surrounding-numbers
		       (get-number
			(+ row 1)
			(- index 1)
			row-count
			column-count
			engine-schematic))))
;;; to the left
    (when (and (> index 0)
	       (digit-char-p (aref engine-schematic row (- index 1))))
      (setf surrounding-numbers
	    (fset:with surrounding-numbers
		       (get-number
			row
			(- index 1)
			row-count
			column-count
			engine-schematic))))
;;; above to the left
    (when (and (> row 0)
	       (> index 0)
	       (digit-char-p (aref engine-schematic (- row 1) (- index 1))))
      (setf surrounding-numbers
	    (fset:with surrounding-numbers
		       (get-number
			(- row 1)
			(- index 1)
			row-count
			column-count
			engine-schematic))))
    surrounding-numbers)) 

(defun get-number (row index row-count column-count engine-schematic)
  (let ((number (string (aref engine-schematic row index)))
	(current-index (- index 1))
	(response (list :row row)))
    (format t "[~a, ~a]: ~a~%" row index number)
    (loop while (and
		 (>= current-index 0)
		 (digit-char-p (aref engine-schematic row current-index)))
	  do (setf number
		   (concatenate 'string
				(string (aref engine-schematic row current-index))
				number)
		   current-index
		   (- current-index 1)))
    (setf (getf response :begin-index) (+ current-index 1)
	  current-index (+ index 1))
    (loop while (and
	      (< current-index column-count)
	      (digit-char-p (aref engine-schematic row current-index)))
       do (setf number
		(concatenate 'string
			     number
			     (string (aref engine-schematic row current-index)))
		current-index
		(+ current-index 1)))
    (setf (getf response :end-index) current-index
	  (getf response :number) (parse-integer number))
    (print response)
    response))

