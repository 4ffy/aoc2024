#!/usr/bin/env -S sbcl --script
(load "~/.sbclrc")
(ql:quickload :cl-ppcre :silent t)

(defun int/ (x y)
  (floor (/ x y)))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\u0a) data))))

(defun parse-data (str)
  "Construct a vector of each integer in STR."
  (map '(vector fixnum) #'parse-integer (ppcre:split "\\W+" str)))

(defun digits (x)
  "Find the number of digits in X."
  (1+ (floor (log x 10))))

(defun split (x)
  "Split X's digits in half and return a cons pair of the result."
  (let ((factor (expt 10 (int/ (digits x) 2))))
    (cons (int/ x factor) (rem x factor))))

(defun blink (x gens cache)
  "Determine the number of stones produced if one with an initial value of X is
blinked GENS times. Memoize results in CACHE."
  (if (= 0 gens)
      1
      (let* ((key (cons x gens))
             (cached (gethash key cache))
             (result 0))
        (if cached
            cached
            (progn
              (cond
                ((= 0 x)
                 (incf result (blink 1 (1- gens) cache)))
                ((= 0 (rem (digits x) 2))
                 (let ((temp (split x)))
                   (incf result (blink (car temp) (1- gens) cache))
                   (incf result (blink (cdr temp) (1- gens) cache))))
                (t
                 (incf result (blink (* x 2024) (1- gens) cache))))
              (setf (gethash key cache) result)
              result)))))

(defun sum-blink (seq gens cache)
  "Sum blink results for SEQ over GENS blinks. Memoize results in CACHE."
  (reduce (lambda (a x) (+ a (blink x gens cache))) seq :initial-value 0))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((data (parse-data (file-to-string (cadr args))))
                (cache (make-hash-table :test #'equal)))
            (format t "25 blinks: ~D~%" (sum-blink data 25 cache))
            (format t "75 blinks: ~D~%" (sum-blink data 75 cache)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(run sb-ext:*posix-argv*)
