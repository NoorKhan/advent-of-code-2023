(defun initialize-engine-schematic (input-file)
  (with-open-file (stream input-file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(print line))))

(initialize-engine-schematic "test input.txt")


