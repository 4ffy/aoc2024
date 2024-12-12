(ql:quickload :cl-ppcre :silent t)

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct grid
  (point-sets
   (make-array 62 :element-type 'list :initial-element nil)
   :type (vector list 62))
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defun char-to-grid-index (char)
  "Convert CHAR to an index in the grid point-sets array."
  (cond
    ((digit-char-p char)
     (digit-char-p char))
    ((upper-case-p char)
     (+ 10 (- (char-code char) (char-code #\A))))
    ((lower-case-p char)
     (+ 36 (- (char-code char) (char-code #\a))))
    (t
     -1)))

(defun parse-grid (str)
  "Read a grid from STR."
  (let* ((lines (ppcre:split "\\n" str))
         (grid (make-grid :height (length lines) :width (length (car lines)))))
    (loop
      for y below (grid-height grid) do
        (loop
          for x below (grid-width grid) do
            (let ((c (char (nth y lines) x)))
              (when (/= -1 (char-to-grid-index c))
                (push (make-point :y y :x x)
                      (aref (grid-point-sets grid) (char-to-grid-index c)))))))
    grid))

(defun pairs (lst)
  "Construct cons pairs that pair the head of LST to each subsequent element."
  (let ((head (car lst))
        (tail (cdr lst)))
    (mapcar (lambda (x) (cons head x)) tail)))

(defun combinations (lst)
  "Construct cons pairs representing each 2-combination of items in LIST."
  (mapcon #'pairs lst))

(defun grid-in-bounds (grid y x)
  (and (<= 0 y (1- (grid-height grid)))
       (<= 0 x (1- (grid-width grid)))))

(defun find-antinodes (grid)
  (let ((result (make-hash-table :test #'equalp))
        (groups (map 'list #'identity (grid-point-sets grid))))
    (dolist (group groups)
      (dolist (pair (combinations group))
        (let* ((p1 (car pair))
               (p2 (cdr pair))
               (dy (- (point-y p2) (point-y p1)))
               (dx (- (point-x p2) (point-x p1)))
               (anti-1
                 (make-point :y (- (point-y p1) dy) :x (- (point-x p1) dx)))
               (anti-2
                 (make-point :y (+ (point-y p2) dy) :x (+ (point-x p2) dx))))
          (when (grid-in-bounds grid (point-y anti-1) (point-x anti-1))
            (setf (gethash anti-1 result) t))
          (when (grid-in-bounds grid (point-y anti-2) (point-x anti-2))
            (setf (gethash anti-2 result) t)))))
    result))

(defun find-antinodes-2 (grid)
  (let ((result (make-hash-table :test #'equalp))
        (groups (map 'list #'identity (grid-point-sets grid))))
    (dolist (group groups)
      (dolist (pair (combinations group))
        (let* ((p1 (car pair))
               (p2 (cdr pair))
               (dy (- (point-y p2) (point-y p1)))
               (dx (- (point-x p2) (point-x p1))))
          (loop
            for i = 0 then (1+ i)
            for done = nil do
              (setf done t)
              (let ((anti-1
                      (make-point
                       :y (- (point-y p1) (* i dy))
                       :x (- (point-x p1) (* i dx))))
                    (anti-2
                      (make-point
                       :y (+ (point-y p2) (* i dy))
                       :x (+ (point-x p2) (* i dx)))))
                (when (grid-in-bounds grid (point-y anti-1) (point-x anti-1))
                  (setf done nil)
                  (setf (gethash anti-1 result) t))
                (when (grid-in-bounds grid (point-y anti-2) (point-x anti-2))
                  (setf done nil)
                  (setf (gethash anti-2 result) t)))
            until done)
          )))
    result))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((grid (parse-grid (file-to-string (cadr args)))))
            (format t
                    "Antinode count: ~D~%"
                    (hash-table-count (find-antinodes grid)))
            (format t
                    "Antinode count 2: ~D~%"
                    (hash-table-count (find-antinodes-2 grid))))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
