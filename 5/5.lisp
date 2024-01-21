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
			    (setf (gethash current-key parsed-input-map) '())))))
		((> (length trimmed-line) 0)
		 (let* ((source-to-destination-list (gethash current-key parsed-input-map))
			(numbers (parse-to-integers trimmed-line))
			(destination-range-start (nth 0 numbers))
			(source-range-start (nth 1 numbers))
			(range-length (nth 2 numbers)))
		   (format t "~a~%" numbers)
		   (setf (gethash current-key parsed-input-map) (append source-to-destination-list (list numbers)))))))))
    parsed-input-map))

(defun source-value-in-range? (source source-range-start range-length)
  (and (<= source-range-start source) (>= (+ source-range-start (- range-length 1)) source)))

(defun get-source-destination (source-to-destination-list source-value)
  (let ((match (find-if #'(lambda (numbers) (let ((source-range-start (nth 1 numbers))
						  (range-length (nth 2 numbers)))
					      (source-value-in-range? source-value  source-range-start range-length)))
			source-to-destination-list)))
    (if match
	(let ((destination-range-start (nth 0 match))
	      (source-range-start (nth 1 match))
	      (range-length (nth 2 match)))
	  (+ destination-range-start (- source-value source-range-start)))
	source-value)))

(defun get-min-location (parsed-input-map &optional (seeds (gethash *seeds-key* parsed-input-map)))
  (let ((min-location nil))
    (loop for seed in seeds
       do (let ((location (get-location seed parsed-input-map)))
	    (when (or (null min-location) (> min-location location))
	      (setf min-location location))))
    min-location))

(defun get-location (seed parsed-input-map)
  (let* ((soil (get-source-destination (gethash *seed-to-soil-key* parsed-input-map) seed))
	 (fertilizer (get-source-destination (gethash *soil-to-fertilizer-key* parsed-input-map) soil))
	 (water (get-source-destination (gethash *fertilizer-to-water-key* parsed-input-map) fertilizer))
	 (light (get-source-destination (gethash *water-to-light-key* parsed-input-map) water))
	 (temperature (get-source-destination (gethash *light-to-temperature-key* parsed-input-map) light))
	 (humidity (get-source-destination (gethash *temperature-to-humidity-key* parsed-input-map) temperature))
	 (location (get-source-destination (gethash *humidity-to-location-key* parsed-input-map) humidity)))
    location))

(get-min-location (parse-input "test input.txt"))
(get-min-location (parse-input "input.txt"))

;;; part 2

(defun get-min-location-part-2 (parsed-input-map)
  (let ((seeds (gethash *seeds-key* parsed-input-map))
	(seed-start-range nil)
	(min-location nil))
    (loop for i from 0 below (length seeds)
       do (if (= (mod i 2) 0)
	      (setf seed-start-range (nth i seeds))
	      (loop for j from 0 below (nth i seeds)
		 for seed = (+ seed-start-range j)
		 do (let* ((location (get-location seed parsed-input-map)))
		      (when (or (null min-location) (> min-location location))
			(setf min-location location))))))
    min-location))

(get-min-location-part-2 (parse-input "test input.txt"))

(defun trim-string-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun parse-to-integers (string-numbers)
  (mapcar #'(lambda (s) (parse-integer s))
	  (uiop:split-string (trim-string-whitespace string-numbers) :separator " ")))

