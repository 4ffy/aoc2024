(load "~/.sbclrc")
(ql:quickload :cl-ppcre :silent t)
(rename-package 'cl-ppcre 're)

(defconstant +north+ 1)
(defconstant +south+ 2)
(defconstant +west+ 4)
(defconstant +east+ 8)

(defun bor (a b)
  (boole boole-ior a b))

(defstruct cell
  (obstacle-p nil :type boolean)
  (visit-flags 0 :type unsigned-byte))

(defmethod print-object ((this cell) stream)
  (cond ((cell-obstacle-p this) (format stream "#"))
        ((< 0 (cell-visit-flags this)) (format stream "@"))
        (t (format stream "."))))

(defclass grid ()
  ((data
    :type (array cell)
    :accessor grid-data
    :initarg :data
    :documentation "Cell array.")
   (width
    :type fixnum
    :accessor grid-width
    :initarg :width
    :documentation "Width of cell array for quick access.")
   (height
    :type fixnum
    :accessor grid-height
    :initarg :height
    :documentation "Height of cell array for quick access.")
   (start-y
    :type fixnum
    :accessor grid-start-y
    :initarg :start-y
    :documentation "Grid walk start y position.")
   (start-x
    :type fixnum
    :accessor grid-start-x
    :initarg :start-x
    :documentation "Grid walk start x position.")
   (walked
    :type boolean
    :accessor grid-walked
    :initform nil
    :documentation "Whether the grid is in a walked state")))

(defgeneric grid-get (this y x)
  (:method ((this grid) y x)
    (aref (grid-data this) y x)))

(defgeneric grid-clear-walk (this)
  (:method ((this grid))
    (loop
      for i from 0 below (array-total-size (grid-data this)) do
        (setf (cell-visit-flags (row-major-aref (grid-data this) i)) 0))
    (setf (grid-walked this) nil)))

(defgeneric grid-walk (this)
  (:method ((this grid))
    (when (grid-walked this)
      (grid-clear-walk this))
    (setf (grid-walked this) t)
    (let ((y (grid-start-y this))
          (x (grid-start-x this))
          (dir +north+)
          (done nil))
      (loop
        until done do
          (let* ((cell (grid-get this y x))
                 (visit-flags (cell-visit-flags cell)))
            (when (= visit-flags (bor dir visit-flags))
              (setf done 'loop))
            (setf (aref (grid-data this) y x)
                  (make-cell :visit-flags (bor dir visit-flags))))
          (cond
            ((= dir +north+)
             (cond
               ((> 0 (1- y))
                (setf done 'exit))
               ((cell-obstacle-p (grid-get this (1- y) x))
                (setf dir +east+))
               (t
                (decf y))))
            ((= dir +south+)
             (cond
               ((= (1+ y) (grid-height this))
                (setf done 'exit))
               ((cell-obstacle-p (grid-get this (1+ y) x))
                (setf dir +west+))
               (t
                (incf y))))
            ((= dir +west+)
             (cond
               ((> 0 (1- x))
                (setf done 'exit))
               ((cell-obstacle-p (grid-get this y (1- x)))
                (setf dir +north+))
               (t
                (decf x))))
            ((= dir +east+)
             (cond
               ((= (1+ x) (grid-width this))
                (setf done 'exit))
               ((cell-obstacle-p (grid-get this y (1+ x)))
                (setf dir +south+))
               (t
                (incf x))))))
      done)))

(defgeneric grid-count-visited (obj)
  (:method ((this grid))
    (when (not (grid-walked this))
      (grid-walk this))
    (loop
      for i from 0 below (array-total-size (grid-data this))
      counting (< 0 (cell-visit-flags (row-major-aref (grid-data this) i))))))

(defmethod print-object ((this grid) stream)
  (loop for y from 0 below (grid-height this) do
    (loop for x from 0 below (grid-width this) do
      (format stream "~A" (grid-get this y x)))
    (format stream "~%"))
  (format stream "Start: y=~A x=~A~%" (grid-start-y this) (grid-start-x this)))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\u0a) data))))

(defun split-lines (str)
  "Split STR into a list of lines."
  (re:split "\\n" str))

(defun chars (str)
  "Construct a list of the characters in STR."
  (map 'list #'identity str))

(defun grid-from-string (str)
  (let* ((lines (split-lines str))
         (height (the fixnum (length lines)))
         (width (the fixnum (length (car lines))))
         (start-x 0)
         (start-y 0)
         (array
           (make-array
            (list height width)
            :element-type 'cell
            :initial-element (make-cell))))
    (loop
      for line in lines for y = 0 then (1+ y) do
        (loop
          for char in (chars line) for x = 0 then (1+ x) do
            (cond
              ((char= char #\#)
               (setf (aref array y x) (make-cell :obstacle-p t)))
              ((char= char #\^)
               (progn
                 (setf start-y y)
                 (setf start-x x))))))
    (make-instance
     'grid
     :data array
     :height height
     :width width
     :start-y start-y
     :start-x start-x)))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((grid (grid-from-string (file-to-string (cadr args)))))
            (grid-walk grid)
            (format t "~A~%" grid)
            (format t "Cells visited: ~D~%" (grid-count-visited grid)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))
