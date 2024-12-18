(ql:quickload :cl-ppcre :silent t)

(defstruct tile
  (visited nil :type boolean)
  (start-dist 0 :type fixnum)
  (end-dist 0 :type fixnum))

(defstruct grid
  (tiles (make-hash-table :test #'equal) :type hash-table)
  (height 0 :type fixnum)
  (width 0 :type fixnum))

(declaim
 (optimize (speed 3) (safety 0))
 (ftype (function (grid (cons fixnum fixnum)) grid-get))
 (ftype (function (string) string) file-to-string)
 (ftype (function (list) list) pairs)
 (ftype (function (string) list) parse-obstacles)
 (ftype (function (list fixnum) grid) parse-grid)
 (ftype (function (grid)) print-grid)
 (ftype (function ((cons fixnum fixnum) (cons fixnum fixnum)) fixnum) manhattan)
 (ftype (function (grid (cons fixnum fixnum)) fixnum) queue-weight)
 (ftype (function (grid (cons fixnum fixnum) (cons fixnum fixnum)) fixnum) a*)
 (inline grid-get)
 (inline manhattan)
 (inline queue-weight))

(defun grid-get (grid pos)
  "Shorthand for fetching a grid tile."
  (gethash pos (grid-tiles grid)))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defun pairs (lst)
  "Construct a list of cons pairs of each adjacent pair of items in LST."
  (loop for pair = lst then (cddr pair)
        collecting `(,(car pair) . ,(cadr pair))
        until (= 2 (length pair))))

(defun parse-obstacles (str)
  "Read each pair of numbers in STR and return a list of cons pairs containing
the result."
  (pairs (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" str))))

(defun parse-grid (obstacles num-obstacles)
  "Construct a grid of appropriate size given OBSTACLES, and add NUM-OBSTACLES
obstacles to the grid."
  (let* ((tiles (make-hash-table :test #'equal))
         (height (1+ (loop for x in obstacles maximize (cdr x))))
         (width (1+ (loop for x in obstacles maximize (car x)))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (gethash (cons y x) tiles) (make-tile))))
    (dotimes (i num-obstacles)
      (let ((pos (nth i obstacles)))
        (remhash (cons (cdr pos) (car pos)) tiles)))
    (make-grid :tiles tiles :height height :width width)))

(defun print-grid (grid)
  "Print GRID to standard output."
  (dotimes (y (grid-height grid))
    (dotimes (x (grid-width grid))
      (if (grid-get grid (cons y x))
          (format t ".")
          (format t "#")))
    (format t "~%")))

(defun manhattan (a b)
  "Given (Y . X) pairs A and B, return the Manhattan distance."
  (+ (abs (- (car b) (car a)))
     (abs (- (cdr b) (cdr a)))))

(defun queue-weight (grid pos)
  "Calculate the weight for POS in the A* priority queue."
  (+ (tile-start-dist (grid-get grid pos))
     (tile-end-dist (grid-get grid pos))))

(defun A* (grid start end)
  "Find the shortest path between (Y . X) pairs START and END over GRID."
  (print-grid grid)
  (loop for tile being each hash-value of (grid-tiles grid) do
    (setf (tile-visited tile) nil)
    (setf (tile-start-dist tile) (floor (/ most-positive-fixnum 2)))
    (setf (tile-end-dist tile) (floor (/ most-positive-fixnum 2))))
  (setf (tile-start-dist (grid-get grid start)) 0)
  (setf (tile-end-dist (grid-get grid start)) (manhattan start end))
  (let ((queue
          (make-array
           0
           :element-type '(cons fixnum fixnum)
           :adjustable t
           :fill-pointer t))
        (curr nil))
    (vector-push-extend start queue)
    (loop
      while (/= 0 (length queue))
      while (not (equal end (setf curr (vector-pop queue))))
      do
         (let ((y (car curr))
               (x (cdr curr))
               (tile (grid-get grid curr)))
           (setf (tile-visited tile) t)
           (dolist (dir
                    (list
                     (cons (1- y) x)
                     (cons (1+ y) x)
                     (cons y (1- x))
                     (cons y (1+ x))))
             (let ((neighbor (grid-get grid dir)))
               (when (and neighbor (not (tile-visited neighbor)))
                 (setf (tile-start-dist neighbor) (1+ (tile-start-dist tile)))
                 (setf (tile-end-dist neighbor) (manhattan dir end))
                 (vector-push-extend dir queue))))
           (sort queue #'> :key (lambda (x) (queue-weight grid x)))))
    (tile-start-dist (grid-get grid end))))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let* ((obstacles (parse-obstacles (file-to-string (cadr args))))
                 (grid (parse-grid obstacles 1024)))
            (format t
                    "Shortest path: ~D~%"
                    (a*
                     grid
                     '(0 . 0)
                     (cons (1- (grid-height grid)) (1- (grid-width grid))))))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
