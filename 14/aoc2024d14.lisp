(ql:quickload :cl-ppcre :silent t)

(defconstant +grid-width+ 101)
(defconstant +grid-height+ 103)

(defstruct robot
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (dx 0 :type fixnum)
  (dy 0 :type fixnum))

(defun int/ (a b)
  (floor (/ a b)))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defun parse-robots (str)
  "Parse each robot line in STR and collect the results into a vector."
  (let ((result
          (make-array
           8
           :element-type 'robot
           :initial-element (make-robot)
           :adjustable t
           :fill-pointer 0)))
    (ppcre:do-register-groups
        ((#'parse-integer x y dx dy))
        ("p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)" str)
      (vector-push-extend (make-robot :x x :y y :dx dx :dy dy) result))
    result))

(defun move-robot (robot &optional (step 1))
  "Move ROBOT in a grid with the given dimensions, "
  (let ((new-x
          (rem (+ (robot-x robot) (* step (robot-dx robot))) +grid-width+))
        (new-y
         (rem (+ (robot-y robot) (* step (robot-dy robot))) +grid-height+)))
    (when (< new-x 0)
      (incf new-x +grid-width+))
    (when (< new-y 0)
      (incf new-y +grid-height+))
    (setf (robot-x robot) new-x)
    (setf (robot-y robot) new-y)))

(defun top-half-p (robot) (< (robot-y robot) (int/ +grid-height+ 2)))

(defun left-half-p (robot) (< (robot-x robot) (int/ +grid-width+ 2)))

(defun bottom-half-p (robot) (< (int/ +grid-height+ 2) (robot-y robot)))

(defun right-half-p (robot) (< (int/ +grid-width+ 2) (robot-x robot)))

(defun quadrant-1-p (robot)
  (and (top-half-p robot) (right-half-p robot)))

(defun quadrant-2-p (robot)
  (and (bottom-half-p robot) (right-half-p robot)))

(defun quadrant-3-p (robot)
  (and (bottom-half-p robot) (left-half-p robot)))

(defun quadrant-4-p (robot)
  (and (top-half-p robot) (left-half-p robot)))

(defun prod-quadrants (robots)
  (* (count-if #'quadrant-1-p robots)
     (count-if #'quadrant-2-p robots)
     (count-if #'quadrant-3-p robots)
     (count-if #'quadrant-4-p robots)))

(defun overlap (robots)
  (let ((result 0)
        (db (make-hash-table :test #'equal)))
    (loop for robot across robots do
      (let ((pos (cons (robot-x robot) (robot-y robot))))
        (if (gethash pos db nil)
            (incf result)
            (setf (gethash pos db) t))))
    result))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((robots (parse-robots (file-to-string (cadr args)))))
            (dotimes (i 10000)
              (when (= i 100)
                (format t "Product of quadrants: ~D~%" (prod-quadrants robots)))
              (when (= 0 (overlap robots))
                (format t "Christmas detected: ~D~%" i))
              (map nil #'move-robot robots)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
