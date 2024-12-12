(ql:quickload :cl-ppcre :silent t)

(defconstant +north+ (the unsigned-byte 1))
(defconstant +south+ (the unsigned-byte 2))
(defconstant +west+ (the unsigned-byte 4))
(defconstant +east+ (the unsigned-byte 8))

(declaim
 (optimize (speed 3) (safety 0))
 (ftype (function (unsigned-byte unsigned-byte) unsigned-byte) band)
 (ftype (function (unsigned-byte unsigned-byte) unsigned-byte) bor)
 (ftype (function (cell unsigned-byte) boolean) has-neighbor)
 (ftype (function (grid fixnum fixnum) cell) grid-get)
 (ftype (function (grid)) clear-visited)
 (ftype (function (walk-result walk-result)) walk-result-incf)
 (ftype (function (string) string) file-to-string)
 (ftype (function (string) grid) parse-grid)
 (ftype (function (grid)) find-regions)
 (ftype (function (grid fixnum fixnum) walk-result) walk)
 (ftype (function (grid) cons) price)
 (ftype (function (list) run))
 (ftype (function () main))
 (inline band)
 (inline bor)
 (inline has-neighbor)
 (inline grid-get)
 (inline clear-visited)
 (inline walk-result-incf))

(defun band (x y)
  "Shorthand for the bitwise and of X and Y."
  (boole boole-and x y))

(defun bor (x y)
  "Shorthand for the bitwise or of X and Y."
  (boole boole-ior x y))

(defmacro borf (x y)
  "Set X to the bitwise or of X and Y. This must be a macro."
  `(setf ,x (bor ,x ,y)))

(defstruct cell
  (id #\Nul :type character)
  (neighbors 0 :type unsigned-byte)
  (visited nil :type boolean))

(defun has-neighbor (cell dir)
  "Shorthand for determining if CELL has a neighbor in DIR."
  (/= 0 (band (cell-neighbors cell) dir)))

(defstruct grid
  (cells (make-array 0 :element-type 'cell) :type (array cell))
  (height 0 :type fixnum)
  (width 0 :type fixnum))

(defun grid-get (grid y x)
  "Get the cell from GRID at (Y, X). Return nil if out of bounds."
  (when (and (<= 0 y (1- (grid-height grid))) (<= 0 x (1- (grid-width grid))))
    (aref (grid-cells grid) y x)))

(defun clear-visited (grid)
  "Clear visited status from cells in GRID."
  (loop for i below (array-total-size (grid-cells grid)) do
    (setf (cell-visited (row-major-aref (grid-cells grid) i)) nil)))

(defstruct walk-result
  (area 0 :type fixnum)
  (perimeter 0 :type fixnum)
  (sides 0 :type fixnum))

(defun walk-result-incf (w1 w2)
  "Add W2's fields to W1."
  (incf (walk-result-area w1) (walk-result-area w2))
  (incf (walk-result-perimeter w1) (walk-result-perimeter w2))
  (incf (walk-result-sides w1) (walk-result-sides w2)))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\u0a) data))))

(defun parse-grid (str)
  "Read a grid from STR."
  (let* ((lines (ppcre:split "\\n" str))
         (height (length lines))
         (width (length (car lines)))
         (cells
           (make-array
            (list height width)
            :element-type 'cell
            :initial-element (make-cell))))
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (setf (aref cells y x) (make-cell :id (char (nth y lines) x)))))
    (make-grid :cells cells :height height :width width)))

(defun find-regions (grid)
  "Connect adjacent cells in GRID that belong to the same region."
  (loop for y from 0 below (grid-height grid) do
    (loop for x from 0 below (grid-width grid) do
      (let ((cell (grid-get grid y x))
            (north (grid-get grid (1- y) x))
            (south (grid-get grid (1+ y) x))
            (west (grid-get grid y (1- x)))
            (east (grid-get grid y (1+ x))))
        (when (and north (char= (cell-id cell) (cell-id north)))
          (borf (cell-neighbors cell) +north+)
          (borf (cell-neighbors north) +south+))
        (when (and south (char= (cell-id cell) (cell-id south)))
          (borf (cell-neighbors cell) +south+)
          (borf (cell-neighbors south) +north+))
        (when (and west (char= (cell-id cell) (cell-id west)))
          (borf (cell-neighbors cell) +west+)
          (borf (cell-neighbors west) +east+))
        (when (and east (char= (cell-id cell) (cell-id east)))
          (borf (cell-neighbors cell) +east+)
          (borf (cell-neighbors east) +west+))))))

(defun walk (grid y x)
  "Walk GRID starting at (Y, X), counting the area, perimeter, and number of
sides of the starting region."
  (let* ((result (make-walk-result))
         (cell (grid-get grid y x))
         (north (grid-get grid (1- y) x))
         (south (grid-get grid (1+ y) x))
         (west (grid-get grid y (1- x)))
         (east (grid-get grid y (1+ x))))
    (when (not (cell-visited cell))
      (setf (cell-visited cell) t)
      (incf (walk-result-area result))
      ;; Visit neighbors.
      (if (and north (has-neighbor cell +north+))
          (walk-result-incf result (walk grid (1- y) x))
          (incf (walk-result-perimeter result)))
      (if (and south (has-neighbor cell +south+))
          (walk-result-incf result (walk grid (1+ y) x))
          (incf (walk-result-perimeter result)))
      (if (and west (has-neighbor cell +west+))
          (walk-result-incf result (walk grid y (1- x)))
          (incf (walk-result-perimeter result)))
      (if (and east (has-neighbor cell +east+))
          (walk-result-incf result (walk grid y (1+ x)))
          (incf (walk-result-perimeter result)))
      ;; Check for concave corners.
      (when (and (not (has-neighbor cell +north+))
                 (not (has-neighbor cell +east+)))
        (incf (walk-result-sides result)))
      (when (and (not (has-neighbor cell +south+))
                 (not (has-neighbor cell +west+)))
        (incf (walk-result-sides result)))
      (when (and (not (has-neighbor cell +east+))
                 (not (has-neighbor cell +south+)))
        (incf (walk-result-sides result)))
      (when (and (not (has-neighbor cell +west+))
                 (not (has-neighbor cell +north+)))
        (incf (walk-result-sides result)))
      ;; Check for convex corners.
      (when (and (has-neighbor cell +north+)
                 (has-neighbor cell +west+)
                 (not (has-neighbor (grid-get grid (1- y) x) +west+))
                 (not (has-neighbor (grid-get grid y (1- x)) +north+)))
        (incf (walk-result-sides result)))
      (when (and (has-neighbor cell +south+)
                 (has-neighbor cell +east+)
                 (not (has-neighbor (grid-get grid (1+ y) x) +east+))
                 (not (has-neighbor (grid-get grid y (1+ x)) +south+)))
        (incf (walk-result-sides result)))
      (when (and (has-neighbor cell +east+)
                 (has-neighbor cell +north+)
                 (not (has-neighbor (grid-get grid y (1+ x)) +north+))
                 (not (has-neighbor (grid-get grid (1- y) x) +east+)))
        (incf (walk-result-sides result)))
      (when (and (has-neighbor cell +west+)
                 (has-neighbor cell +south+)
                 (not (has-neighbor (grid-get grid y (1- x)) +south+))
                 (not (has-neighbor (grid-get grid (1+ y) x) +west+)))
        (incf (walk-result-sides result))))
    result))


(defun price (grid)
  (clear-visited grid)
  (let ((normal 0)
        (bulk 0))
    (loop for y below (grid-height grid) do
      (loop for x below (grid-width grid) do
        (when (not (cell-visited (grid-get grid y x)))
          (let ((result (walk grid y x)))
            (incf normal (* (walk-result-area result)
                            (walk-result-perimeter result)))
            (incf bulk
                  (* (walk-result-area result)
                     (walk-result-sides result)))))))
    (cons normal bulk)))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((grid (parse-grid (file-to-string (cadr args)))))
            (find-regions grid)
            (let ((prices (price grid)))
              (format t "Normal price: ~D~%" (car prices))
              (format t "Bulk price: ~D~%" (cdr prices))))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
