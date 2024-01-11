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

(defparameter *keys* (list *seeds-key* *seed-to-soil-key* *soil-to-fertilizer-key* *fertilizer-to-water-key*
		       *water-to-light-key* *light-to-temperature-key* *temperature-to-humidity-key* *humidity-to-location-key*))

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

(defun get-min-location (parsed-input-map)
  (let ((seeds (gethash *seeds-key* parsed-input-map))
	(min-location nil))
    (loop for seed in seeds
	  do (let* ((soil (get-value (gethash *seed-to-soil-key* parsed-input-map) seed))
			 (fertilizer (get-value (gethash *soil-to-fertilizer-key* parsed-input-map) soil))
			 (water (get-value (gethash *fertilizer-to-water-key* parsed-input-map) fertilizer))
			 (light (get-value (gethash *water-to-light-key* parsed-input-map) water))
			 (temperature (get-value (gethash *light-to-temperature-key* parsed-input-map) light))
			 (humidity (get-value (gethash *temperature-to-humidity-key* parsed-input-map) temperature))
			 (location (get-value (gethash *humidity-to-location-key* parsed-input-map) humidity)))
		    (when (or (null min-location) (> min-location location))
		      (setf min-location location))))
    min-location))

(defun get-value (source-to-destination-map source)
  (let ((value (gethash source source-to-destination-map)))
    (if value value source)))

(get-min-location (parse-input "input.txt"))

(defun trim-string-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun parse-to-integers (string-numbers)
  (mapcar #'(lambda (s) (parse-integer s))
	  (uiop:split-string (trim-string-whitespace string-numbers) :separator " ")))

