(ql:quickload :arrow-macros)

(defparameter *seeds-key* "seeds")
(defparameter *seed-to-soil-key* "seed-to-soil")
(defparameter *soil-to-fertilizer-key* "soil-to-fertilizer")
(defparameter *fertilizer-to-water-key* "fertilizer-to-water")
(defparameter *water-to-light-key* "water-to-light")
(defparameter *light-to-temperature-key* "light-to-temperature")
(defparameter *temperature-to-humidity-key* "temperature-to-humidity")
(defparameter *humidity-to-location-key* "humidity-to-location")

(defun parse-input (input-file-name)
  (with-open-file (stream input-file-name)
    (do ((line (read-line stream nil)
	       (read-line stream nil)))
	((null line))
      (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
	(when (search ":" trimmed-line)
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
	    (cond ((equal key *seeds-key*) (let* ((seed-numbers (uiop:split-string
								 (trim-string-whitespace
								  (cadr (uiop:split-string
									 trimmed-line
									 :separator ":")))
								 :separator " ")))
					     (format t "~a~%" seed-numbers))))))))))

(defun trim-string-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(parse-input "test input.txt")

