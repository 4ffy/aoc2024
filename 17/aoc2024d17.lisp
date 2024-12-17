(ql:quickload :cl-ppcre :silent t)

(defun int/ (a b)
  (floor (/ a b)))

(defstruct vm
  (ra 0 :type fixnum)
  (rb 0 :type fixnum)
  (rc 0 :type fixnum)
  (ip 0 :type fixnum)
  (program
   (make-array 0 :element-type 'unsigned-byte)
   :type (vector unsigned-byte)))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defun parse-vm (src)
  (let ((numbers
          (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" src))))
    (if (>= (length numbers) 4)
        (let ((ra (car numbers))
              (rb (cadr numbers))
              (rc (caddr numbers))
              (program
                (make-array
                 (length (cdddr numbers))
                 :element-type 'unsigned-byte
                 :initial-contents (cdddr numbers))))
          (make-vm :ra ra :rb rb :rc rc :program program))
        (error "Bad input."))))

(defun vm-read-byte (vm)
  (incf (vm-ip vm))
  (aref (vm-program vm) (1- (vm-ip vm))))

(defun vm-get-combo (vm arg)
  (cond
    ((= arg 0) 0)
    ((= arg 1) 1)
    ((= arg 2) 2)
    ((= arg 3) 3)
    ((= arg 4) (vm-ra vm))
    ((= arg 5) (vm-rb vm))
    ((= arg 6) (vm-rc vm))
    (t (error (format nil "Bad combo arg '~D'.~%" arg)))))

(defun vm-run (vm &optional registers)
  (when registers
    (setf (vm-ra vm) (car registers))
    (setf (vm-rb vm) (cadr registers))
    (setf (vm-rc vm) (caddr registers)))
  (setf (vm-ip vm) 0)
  (let ((result
          (make-array
           0
           :element-type 'unsigned-byte
           :adjustable t
           :fill-pointer t)))
    (loop
      until (>= (vm-ip vm) (array-total-size (vm-program vm))) do
        (let ((op (vm-read-byte vm))
              (arg (vm-read-byte vm)))
          (cond
            ((= op 0)
             (setf (vm-ra vm) (int/ (vm-ra vm) (expt 2 (vm-get-combo vm arg)))))
            ((= op 1)
             (setf (vm-rb vm) (boole boole-xor (vm-rb vm) arg)))
            ((= op 2)
             (setf (vm-rb vm) (boole boole-and (vm-get-combo vm arg) 7)))
            ((= op 3)
             (when (/= 0 (vm-ra vm))
               (setf (vm-ip vm) arg)))
            ((= op 4)
             (setf (vm-rb vm) (boole boole-xor (vm-rb vm) (vm-rc vm))))
            ((= op 5)
             (let ((temp (boole boole-and (vm-get-combo vm arg) 7)))
               (vector-push-extend temp result)))
            ((= op 6)
             (setf (vm-rb vm) (int/ (vm-ra vm) (expt 2 (vm-get-combo vm arg)))))
            ((= op 7)
             (setf (vm-rc vm) (int/ (vm-ra vm) (expt 2 (vm-get-combo vm arg)))))
            (t
             (error (format nil "Bad opcode '~D'.~%" op))))))
    result))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let* ((vm (parse-vm (file-to-string (cadr args))))
                 (out (vm-run vm)))
            (format t "~D" (aref out 0))
            (loop for x below (length out) do
              (format t ",~D" (aref out x)))
            (format t "~%"))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
