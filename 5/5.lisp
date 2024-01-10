(ql:quickload :arrow-macros)
(use-package :arrow-macros)

(defparameter *seeds-key* "seeds")
(defparameter *seed-to-soil-key* "seed-to-soil")
(defparameter *soil-to-fertilizer-key* "soil-to-fertilizer")
(defparameter *fertilizer-to-water-key* "fertilizer-to-water")
(defparameter *water-to-light-key* "water-to-light")
(defparameter *light-to-temperature-key* "light-to-temperature")
(defparameter *temperature-to-humidity-key* "temperature-to-humidity")
(defparameter *humidity-to-location-key* "humidity-to-location")

(defun parse-input (input-file-name)
  (let ((parsed-input-map (make-hash-table :test #'equal))
	(current-key ""))
    (with-open-file (stream input-file-name)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(let* ((trimmed-line (trim-string-whitespace line)))
	  (cond ((search ":" trimmed-line)
		 (format t "~a~%" (car (uiop:split-string
					(car (uiop:split-string
					      trimmed-line
					      :separator ":"))
					:separator " ")))
		 (let ((key (car (uiop:split-string
				  (car (uiop:split-string
					trimmed-line
					:separator ":"))
				  :separator " "))))
		   (cond ((equal key *seeds-key*) (let* ((seed-numbers (parse-to-integers
									(trim-string-whitespace
									 (cadr (uiop:split-string
										trimmed-line
										:separator ":"))))))
						    (format t "~a~%" Seed-numbers)
						    (setf (gethash *seeds-key* parsed-input-map) seed-numbers)))
			 (t (setf current-key key)
			    (setf (gethash current-key parsed-input-map) (make-hash-table))))))
		((> (length trimmed-line) 0)
		 (let* ((source-to-destination-map (gethash current-key parsed-input-map))
			(numbers (parse-to-integers trimmed-line))
			(destination-range-start (nth 0 numbers))
			(source-range-start (nth 1 numbers))
			(range-length (nth 2 numbers)))
		   (format t "~a~%" numbers)
		   (loop for i from 0 below range-length
			 do (setf (gethash (+ source-range-start i) source-to-destination-map)
				  (+ destination-range-start i)))))))))
    parsed-input-map))

(defun trim-string-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun parse-to-integers (string-numbers)
  (mapcar #'(lambda (s) (parse-integer s))
	  (uiop:split-string (trim-string-whitespace string-numbers) :separator " ")))

(gethash *seed-to-soil-key* (parse-input "test input.txt"))
(gethash *seeds-key* (parse-input "test input.txt"))

(parse-to-integers "1 2 44 3")
(uiop:split-string " 1 2          3" :separator " ")

(-<> 1 (+ <> <>) (-<> (+ <> <> <>)))


