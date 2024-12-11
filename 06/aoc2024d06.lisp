(ql:quickload :cl-ppcre :silent t)
(rename-package 'cl-ppcre 're)

(declaim (optimize (speed 3) (safety 0))) ; Best I can do.

(defconstant +north+ 1)
(defconstant +south+ 2)
(defconstant +west+ 4)
(defconstant +east+ 8)

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

(defun bor (a b)
  "Return the bitwise or of A and B. Because that expression is confusing."
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
   (height
    :type fixnum
    :accessor grid-height
    :initarg :height
    :documentation "Height of cell array for quick access.")
   (width
    :type fixnum
    :accessor grid-width
    :initarg :width
    :documentation "Width of cell array for quick access.")
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

(defun grid-from-string (str)
  "Construct a grid from input STR."
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

(defun grid-copy (grid)
  "Create a new grid that is a copy of GRID."
  (let* ((width (grid-width grid))
         (height (grid-height grid))
         (start-y (grid-start-y grid))
         (start-x (grid-start-x grid))
         (grid-array (grid-data grid))
         (array
           (make-array
            (array-dimensions grid-array)
            :element-type 'cell
            :initial-element (make-cell))))
    (loop
      for i from 0 below (array-total-size grid-array) do
        (let ((grid-item (row-major-aref grid-array i)))
          (setf (row-major-aref array i)
                (make-cell
                 :obstacle-p (cell-obstacle-p grid-item)
                 :visit-flags (cell-visit-flags grid-item)))))
    (make-instance
     'grid
     :data array
     :height height
     :width width
     :start-y start-y
     :start-x start-x)))

(defun grid-get (grid y x)
  "Get the cell at Y X in GRID."
  (aref (grid-data grid) y x))

(defun grid-clear-walk (grid)
  "Clear visited cells and walked status of GRID."
  (loop
    for i from 0 below (array-total-size (grid-data grid)) do
      (setf (cell-visit-flags (row-major-aref (grid-data grid) i)) 0))
  (setf (grid-walked grid) nil))

(defun grid-walk (grid)
  "Walk GRID, turning at obstacles, until out of bounds or a loop is found."
  (declare (optimize (speed 3) (safety 0)))
  (when (grid-walked grid)
    (grid-clear-walk grid))
  (setf (grid-walked grid) t)
  (let ((y (grid-start-y grid))
        (x (grid-start-x grid))
        (dir +north+)
        (done nil))
    (loop
      until done do
        (let* ((cell (grid-get grid y x))
               (visit-flags (cell-visit-flags cell)))
          (when (= visit-flags (bor dir visit-flags))
            (setf done 'loop))
          (setf (aref (grid-data grid) y x)
                (make-cell :visit-flags (bor dir visit-flags))))
        (cond
          ((= dir +north+)
           (cond
             ((> 0 (1- y))
              (setf done 'exit))
             ((cell-obstacle-p (grid-get grid (1- y) x))
              (setf dir +east+))
             (t
              (decf y))))
          ((= dir +south+)
           (cond
             ((= (1+ y) (grid-height grid))
              (setf done 'exit))
             ((cell-obstacle-p (grid-get grid (1+ y) x))
              (setf dir +west+))
             (t
              (incf y))))
          ((= dir +west+)
           (cond
             ((> 0 (1- x))
              (setf done 'exit))
             ((cell-obstacle-p (grid-get grid y (1- x)))
              (setf dir +north+))
             (t
              (decf x))))
          ((= dir +east+)
           (cond
             ((= (1+ x) (grid-width grid))
              (setf done 'exit))
             ((cell-obstacle-p (grid-get grid y (1+ x)))
              (setf dir +south+))
             (t
              (incf x))))))
    done))

(defun grid-count-visited (grid)
  "Walk GRID and return the number of unique cells visited."
  (when (not (grid-walked grid))
    (grid-walk grid))
  (loop
    for i from 0 below (array-total-size (grid-data grid))
    counting (< 0 (cell-visit-flags (row-major-aref (grid-data grid) i)))))

(defun grid-obstacle-creates-loop-p (grid y x)
  "Place an obstacle at Y X in GRID and determine whether walking the grid
results in a loop."
  (when (not (and (= y (grid-start-y grid)) (= x (grid-start-x grid))))
    (grid-clear-walk grid)
    (let ((result nil))
      (setf (aref (grid-data grid) y x) (make-cell :obstacle-p t))
      (setf result (grid-walk grid))
      (setf (aref (grid-data grid) y x) (make-cell :obstacle-p nil))
      (equal result 'loop))))

(defun grid-count-possible-loops (grid)
  "Count the number of possible loops created by the addition of an obstacle to
GRID."
  (when (not (grid-walked grid))
    (grid-walk grid))
  (let ((copy (grid-copy grid)))
    (loop
      for y from 0 below (grid-height grid) summing
        (loop
          for x from 0 below (grid-width grid) counting
          (and (< 0 (cell-visit-flags (grid-get grid y x)))
               (grid-obstacle-creates-loop-p copy y x))))))

(defmethod print-object ((this grid) stream)
  (loop for y from 0 below (grid-height this) do
    (loop for x from 0 below (grid-width this) do
      (format stream "~A" (grid-get this y x)))
    (format stream "~%"))
  (format stream "Start: y=~A x=~A~%" (grid-start-y this) (grid-start-x this)))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((grid (grid-from-string (file-to-string (cadr args)))))
            (format t "Cells visited: ~D~%" (grid-count-visited grid))
            (format t "Possible loops: ~D~%" (grid-count-possible-loops grid)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
