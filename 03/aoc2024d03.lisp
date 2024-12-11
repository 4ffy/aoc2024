(ql:quickload :cl-ppcre :silent t)
(rename-package 'cl-ppcre 're)

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun parse-mul (str)
  "Parse the mul(x,y) statement STR into a cons pair of x and y."
  (let ((re "mul\\((\\d{1,3}),(\\d{1,3})\\)"))
    (re:register-groups-bind
        (left right) (re str)
      `(,(parse-integer left) . ,(parse-integer right)))))

(defun string-to-token (str)
  "Parse STR into a do, don't or multiply command."
  (cond
    ((equal "do()" str)
     'enable)
    ((equal "don't()" str)
     'disable)
    (t
     (parse-mul str))))

(defun exec (tokens &optional no-disable)
  "Execute TOKENS, summing the enabled multiply instructions"
  (let ((sum 0)
        (disabled nil))
    (dolist (token tokens sum)
      (cond
        ((equal 'enable token)
         (setf disabled nil))
        ((equal 'disable token)
         (when (not no-disable)
           (setf disabled t)))
        (t
         (when (not disabled)
           (incf sum (* (car token) (cdr token)))))))))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let* ((data (file-to-string (cadr args)))
                 (re "do\\(\\)|don't\\(\\)|mul\\(\\d{1,3},\\d{1,3}\\)")
                 (tokens
                   (mapcar
                    #'string-to-token
                    (re:all-matches-as-strings re data))))
            (format t "Part 1: ~D~%" (exec tokens t))
            (format t "Part 2: ~D~%" (exec tokens)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
