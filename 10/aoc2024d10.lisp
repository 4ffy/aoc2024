#!/usr/bin/env -S sbcl --script
(load "~/.sbclrc")
(ql:quickload :cl-ppcre :silent t)

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\u0a) data))))

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct grid
  (data (make-array 0 :element-type 'fixnum) :type (array fixnum))
  (height 0 :type fixnum)
  (width 0 :type fixnum))

(defstruct walk-result (score 0 :type fixnum) (rating 0 :type fixnum))

(defun walk-result-add (a b)
  (make-walk-result
   :score (+ (walk-result-score a) (walk-result-score b))
   :rating (+ (walk-result-rating a) (walk-result-rating b))))

(defun parse-grid (str)
  (let* ((lines (ppcre:split "\\n" str))
         (height (length lines))
         (width (length (car lines)))
         (data (make-array (list height width) :element-type 'fixnum)))
    (loop
      for y from 0 below height do
        (loop
          for x from 0 below width do
            (let ((c (char (nth y lines) x)))
              (setf (aref data y x) (digit-char-p c)))))
    (make-grid :data data :height height :width width)))

(defun walk (grid start-y start-x)
  (let ((rating 0)
        (unique (make-hash-table :test #'equalp))
        (stack (list (make-point :y start-y :x start-x))))
    (loop
      while stack do
        (let* ((current (pop stack))
               (y (point-y current))
               (x (point-x current))
               (value (aref (grid-data grid) y x))
               (target (1+ value)))
          (if (= value 9)
              (progn
                (incf rating)
                (setf (gethash current unique) t))
              (progn
                (when (and (> y 0) (= target (aref (grid-data grid) (1- y) x)))
                  (push (make-point :y (1- y) :x x) stack))
                (when (and (< y (1- (grid-height grid)))
                           (= target (aref (grid-data grid) (1+ y) x)))
                  (push (make-point :y (1+ y) :x x) stack))
                (when (and (> x 0) (= target (aref (grid-data grid) y (1- x))))
                  (push (make-point :y y :x (1- x)) stack))
                (when (and (< x (1- (grid-width grid)))
                           (= target (aref (grid-data grid) y (1+ x))))
                  (push (make-point :y y :x (1+ x)) stack))))))
    (make-walk-result :score (hash-table-count unique) :rating rating)))

(defun sum-score (grid)
  (let ((result (make-walk-result)))
    (loop
      for y from 0 below (grid-height grid) do
        (loop
          for x from 0 below (grid-width grid) do
            (when (= 0 (aref (grid-data grid) y x))
              (setf result (walk-result-add result (walk grid y x))))))
    result))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let* ((grid (parse-grid (file-to-string (cadr args))))
                 (result (sum-score grid)))
            (format t "Sum of score: ~D~%" (walk-result-score result))
            (format t "Sum of rating: ~D~%" (walk-result-rating result)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(run sb-ext:*posix-argv*)
