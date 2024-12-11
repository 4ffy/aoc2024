(defun zip (list-1 list-2)
  "Zip LIST-1 and LIST-2 together as a cons pair of each corresponding element."
  (mapcar (lambda (x y) `(,x . ,y)) list-1 list-2))

(defun distance (list-1 list-2)
  "Calculate the sum of the distance between each element of LIST-1 and LIST-2."
  (reduce
   (lambda (a x) (+ a (abs (- (car x) (cdr x)))))
   (zip list-1 list-2)
   :initial-value 0))

(defun similarity (list-1 list-2)
  "Calculate the sum of (the product of each element of LIST-1 and the number of
occurences of said element in LIST-2)."
  (reduce
   (lambda (a x) (+ a (* x (count x list-2))))
   list-1
   :initial-value 0))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun split-string (sep str)
  "Split STR into a list using the separator SEP."
  (loop for start = 0 then (1+ finish)
        for finish = (position sep str :start start)
        collecting (subseq str start finish)
        until (null finish)))

(defun parse-line (line)
  "Given LINE from the input file, get the two integers stored."
  (let ((pair (remove "" (split-string #\  line) :test #'equalp)))
    `(,(parse-integer (car pair)) . ,(parse-integer (cadr pair)))))

(defun run (args)
  "Do the thing."
  (if (equalp 2 (length args))
      (if (probe-file (cadr args))
          (let* ((data (file-to-string (cadr args)))
                 (lines (remove "" (split-string #\u0a data) :test #'equalp))
                 (pairs (mapcar #'parse-line lines))
                 (left (sort (mapcar #'car pairs) #'<))
                 (right (sort (mapcar #'cdr pairs) #'<)))
            (format t "Distance: ~D~%" (distance left right))
            (format t "Similarity: ~D~%" (similarity left right)))
          (progn
            (format t "File not found: '~A'.~%" (cadr args))
            (sb-ext:exit :code 1)))
      (progn
        (format t "Usage: ~A inputfile~%" (car args))
        (sb-ext:exit :code 1))))

(defun main ()
  (run sb-ext:*posix-argv*))
