(ql:quickload :cl-ppcre :silent t)
(ql:quickload :cl-slice :silent t)
(rename-package 'cl-ppcre 're)
(rename-package 'cl-slice 'sl)

(defun rows (arr)
  "Collect a list of strings representing each row of ARR."
  (declare (type (array character) arr))
  (loop for y from 0 below (array-dimension arr 0)
        collecting (sl:slice arr y t)))

(defun columns (arr)
  "Collect a list of strings representing each column of ARR."
  (declare (type (array character) arr))
  (loop for x from 0 below (array-dimension arr 1)
        collecting (sl:slice arr t x)))

(defun get-diagonal (arr offset)
  "Construct a string representing the OFFSET diagonal of ARR."
  (declare (type (array character) arr)
           (type integer offset))
  (let ((x-min (if (plusp offset) offset 0))
        (y-min (if (minusp offset) (- offset) 0))
        (x-max (array-dimension arr 1))
        (y-max (array-dimension arr 0)))
    (concatenate
     'string
     (loop for y from y-min below y-max
           for x from x-min below x-max
           collecting (aref arr y x)))))

(defun diagonals (arr)
  "Collect a list of strings representing each diagonal of ARR."
  (declare (type (array character) arr))
  (let* ((smallest-diag (- 1 (array-dimension arr 0)))
         (largest-diag (1- (array-dimension arr 1))))
    (loop for d from smallest-diag to largest-diag
          collecting (get-diagonal arr d))))

(defun get-antidiagonal (arr offset)
  (declare (type (array character) arr)
           (type integer offset))
  (let ((x-min 0)
        (y-min (if (plusp offset) offset 0))
        (x-max
          (if (plusp offset)
              (1- (array-dimension arr 1))
              (1- (+ (array-dimension arr 1) offset))))
        (y-max (array-dimension arr 0)))
    (concatenate
     'string
     (loop for x from x-max downto x-min
           for y from y-min below y-max
           collecting (aref arr y x)))))

(defun antidiagonals (arr)
  (declare (type (array character) arr))
  (let* ((smallest-diag (- 1 (array-dimension arr 0)))
         (largest-diag (1- (array-dimension arr 1))))
    (loop for d from smallest-diag to largest-diag
          collecting (get-antidiagonal arr d))))

(defun rows-reverse (arr)
  (declare (type (array character) arr))
  (mapcar #'reverse (rows arr)))

(defun columns-reverse (arr)
  (declare (type (array character) arr))
  (mapcar #'reverse (columns arr)))

(defun diagonals-reverse (arr)
  (declare (type (array character) arr))
  (mapcar #'reverse (diagonals arr)))

(defun antidiagonals-reverse (arr)
  (declare (type (array character) arr))
  (mapcar #'reverse (antidiagonals arr)))

(defun count-xmas (str)
  (declare (type string str))
  (length (re:all-matches-as-strings "XMAS" str)))

(defun count-all (arr)
  (declare (type (array character) arr))
  (reduce
   (lambda (a x) (+ a (count-xmas x)))
   (concatenate
    'list
    (rows arr)
    (columns arr)
    (diagonals arr)
    (antidiagonals arr)
    (rows-reverse arr)
    (columns-reverse arr)
    (diagonals-reverse arr)
    (antidiagonals-reverse arr))
   :initial-value 0))

(defun cross-p (arr y x)
  (and (eql #\A (aref arr y x))
       (or (and (eql #\M (aref arr (1- y) (1- x)))
                (eql #\S (aref arr (1+ y) (1+ x))))
           (and (eql #\S (aref arr (1- y) (1- x)))
                (eql #\M (aref arr (1+ y) (1+ x)))))
       (or (and (eql #\M (aref arr (1- y) (1+ x)))
                (eql #\S (aref arr (1+ y) (1- x))))
           (and (eql #\S (aref arr (1- y) (1+ x)))
                (eql #\M (aref arr (1+ y) (1- x)))))))

(defun count-crosses (arr)
  (loop for y from 1 below (1- (array-dimension arr 0))
        summing (loop for x from 1 below (1- (array-dimension arr 1))
                      counting (cross-p arr y x))))

(defun file-to-string (path)
  (declare (type string path))
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defun split-string (sep str)
  (declare (type character sep)
           (type string str))
  "Split STR into a list using the separator SEP."
  (loop for start = 0 then (1+ finish)
        for finish = (position sep str :start start)
        collecting (subseq str start finish)
        until (null finish)))

(defun get-array-dimensions (src)
  (declare (type string src))
  "Find the dimensions required for a char array to store the char grid SRC."
  (let ((lines (split-string #\Newline src)))
    (list (length lines) (length (car lines)))))

(defun string-to-char-array (src)
  (declare (type string src))
  "Store the char grid SRC into an appropriately-sized char array."
  (let ((arr (make-array (get-array-dimensions src) :element-type 'character))
        (lines (split-string #\Newline src)))
    (loop for y from 0 below (length lines) do
      (loop for x from 0 below (length (car lines)) do
        (setf (aref arr y x) (char (nth y lines) x))))
    arr))

(defun run (args)
  (declare (type list args))
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((grid (string-to-char-array (file-to-string (cadr args)))))
            (format t "XMAS count: ~D~%" (count-all grid))
            (format t "X-MAS count: ~D~%" (count-crosses grid)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
