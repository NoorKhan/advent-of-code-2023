;; part 1

(defun get-calibration-value (s)
  (let ((first-digit nil)
	(second-digit nil))
    (loop for char across s do
	 (if (digit-char-p char)
	     (if (null first-digit)
		 (setf first-digit (string char))
		 (setf second-digit (string char)))))
    (if (null second-digit)
	(parse-integer (concatenate 'string first-digit first-digit))
	(parse-integer (concatenate 'string first-digit second-digit)))))

(defun sum-calibration-values (input-file)
  (let ((sum 0))
    (with-open-file (stream input-file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(setf sum (+ sum (get-calibration-value line)))))
    sum))

(sum-calibration-values "input.txt")

;; part 2

(defparameter *search-string-values*
  (loop for i from 1 upto 9
     collect (cons (format nil "~a" i) i)
     collect (cons (format nil "~r" i) i)))

(defun find-first-number (s &key (from-end nil))
  (let ((found-index nil)
	(found-number nil))
    (loop for search-string-value in *search-string-values*
       do (let ((current-index (search (car search-string-value) s :from-end from-end)))
	    (cond ((and (null found-index) current-index)
		   (setf found-index current-index
			 found-number (cdr search-string-value)))
		  ((and found-index current-index (if from-end (> current-index found-index) (< current-index found-index)))
		   (setf found-index current-index
			 found-number (cdr search-string-value))))))
    
    found-number))

(defun get-corrected-calibration-value (s)
  (+ (* 10 (find-first-number s)) (find-first-number s :from-end t)))

(defun corrected-sum-calibration-values (input-file)
  (let ((sum 0))
    (with-open-file (stream input-file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(setf sum (+ sum (get-corrected-calibration-value line)))))
    
    sum))

(corrected-sum-calibration-values "input.txt")

