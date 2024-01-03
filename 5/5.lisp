(+ 1 2)

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
				 :separator " "))))))))

(parse-input "test input.txt")

(uiop:split-string "abc" :separator " ")

(defun test (x)
  (let ((x 5))
    (print x))
  (print x))

(test 7)



