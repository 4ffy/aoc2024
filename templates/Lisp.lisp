(ql:quickload :cl-ppcre :silent t)

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (format t "hi~%")
          (error (format nil "File not found: '~A'" (cadr args))))
      (error "No input file")))

(defun main ()
  (handler-case (run sb-ext:*posix-argv*)
    (error (e)
      (format *error-output* "~A~%" e)
      (sb-ext:exit :code 1))))
