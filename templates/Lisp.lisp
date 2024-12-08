(ql:quickload :cl-ppcre :silent t)

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\u0a) data))))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (format t "hi~%")
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))
