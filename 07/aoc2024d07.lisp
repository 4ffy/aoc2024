(ql:quickload :cl-ppcre :silent t)

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defstruct record
  (expected 0 :type fixnum)
  (operands nil :type list))

(defun parse-record (str)
  "Parse the record line STR to create a record with the appropriate expected
value and operands."
  (let ((numbers (mapcar #'parse-integer (ppcre:split ":? " str))))
    (make-record :expected (car numbers) :operands (cdr numbers))))

(defun parse-all-records (str)
  "Parse each record of STR and collect the result."
  (mapcar #'parse-record (ppcre:split "\\n" str)))

(defun record-is-valid (record)
  "Determine whether the expected value of RECORD can be expressed as a sequence
of addition and multiplication of the operands. Return the expected value of the
record if construction is possible. Otherwise, return NIL."
  (labels
      ((recurse
           (target sublist)
         (let ((last-item (car (last sublist))))
           (if (= 1 (length sublist))
               (when (= target last-item)
                 (record-expected record))
               (or (when (= 0 (rem target last-item))
                     (recurse (/ target last-item) (butlast sublist)))
                   (when (< 0 (- target last-item))
                     (recurse (- target last-item) (butlast sublist))))))))
    (recurse (record-expected record) (record-operands record))))

(defun sum-valid-records (records)
  "Determine the sum of the expected values of valid records in RECORDS."
  (reduce
   (lambda (a r) (+ a (or (record-is-valid r) 0)))
   records
   :initial-value 0))

(defun digits (x)
  "Return the number of digits of X."
  (if (= x 0)
      1
      (1+ (floor (log x 10)))))

(defun ends-with-p (x y)
  (let* ((x-digits (digits x))
         (y-digits (digits y))
         (slice (expt 10 y-digits)))
    (when (>= x-digits y-digits)
      (= x (+ y (* slice (floor (/ x slice))))))))

(defun record-is-valid-concat (record)
  "Determine whether the expected value of RECORD can be expressed as a sequence
of addition, multiplication, or concatenation of the operands. Return the
expected value of the record if construction is possible. Otherwise, return
NIL."
  (labels
      ((recurse
           (target sublist)
         (let ((last-item (car (last sublist))))
           (if (= 1 (length sublist))
               (when (= target last-item)
                 (record-expected record))
               (or (when (= 0 (rem target last-item))
                     (recurse (/ target last-item) (butlast sublist)))
                   (when (< 0 (- target last-item))
                     (recurse (- target last-item) (butlast sublist)))
                   (when (ends-with-p target last-item)
                     (recurse
                      (floor (/ target (expt 10 (digits last-item))))
                      (butlast sublist))))))))
    (recurse (record-expected record) (record-operands record))))

(defun sum-valid-records-concat (records)
  "Determine the sum of the expected values of valid records in RECORDS."
  (reduce
   (lambda (a r) (+ a (or (record-is-valid-concat r) 0)))
   records
   :initial-value 0))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((records (parse-all-records (file-to-string (cadr args)))))
            (format t "Sum of valid: ~D~%" (sum-valid-records records))
            (format t
                    "Sum with concat: ~D~%"
                    (sum-valid-records-concat records)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
