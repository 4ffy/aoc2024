(ql:quickload :cl-containers :silent t)

(declaim
 (ftype (function (string) string) file-to-string)
 (ftype (function (character string) list) split-string)
 (ftype (function (string) grid) parse-grid)
 (ftype (function (grid (complex fixnum))) grid-bfs)
 (ftype (function (grid number number) fixnum) time-saved)
 (ftype (function (grid) (vector fixnum)) all-time-saved)
 (ftype (function (grid fixnum) fixnum) count-good-cheats)
 (ftype (function (grid) t) find-start-pos)
 (inline grid-get))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defun split-string (sep str)
  "Split STR into a list using the separator SEP."
  (loop for start = 0 then (1+ finish)
        for finish = (position sep str :start start)
        collecting (subseq str start finish)
        until (null finish)))

(defstruct tile
  (char #\Nul :type character)
  (visited nil :type boolean)
  (dist 0 :type fixnum))

(defmethod print-object ((obj tile) stream)
  (format stream "~C" (tile-char obj)))

(defstruct grid
  (tiles (make-hash-table) :type hash-table)
  (height 0 :type fixnum)
  (width 0 :type fixnum))

(defun grid-get (grid pos)
  (gethash pos (grid-tiles grid)))

(defmethod print-object ((obj grid) stream)
  (dotimes (y (grid-height obj))
    (dotimes (x (grid-width obj))
      (let ((tile (grid-get obj (complex y x))))
        (format stream
                "~3A"
                (if (char= #\#(tile-char tile))
                    "██"
                    (tile-dist tile)))))
    (format stream "~%")))

(defun parse-grid (str)
  "Read a grid from STR."
  (let* ((lines (split-string #\Newline str))
         (height (length lines))
         (width (length (car lines)))
         (data (make-hash-table)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (gethash (complex y x) data)
              (make-tile :char (char (nth y lines) x)))))
    (make-grid :height height :width width :tiles data)))

(defun grid-bfs (grid start-pos)
  "Walk over open tiles in GRID, keeping track of the distance from START-POS."
  (loop for tile being each hash-value of (grid-tiles grid) do
    (setf (tile-visited tile) nil))
  (let ((queue (make-instance 'containers:basic-queue)))
    (setf (tile-visited (grid-get grid start-pos)) t
          (tile-dist (grid-get grid start-pos)) 0)
    (containers:insert-item queue start-pos)
    (loop until (containers:empty-p queue) do
      (let* ((pos (containers:delete-first queue))
             (tile (grid-get grid pos)))
        (dolist (dir (list (1- pos) (1+ pos) (- pos #C(0 1)) (+ pos #C(0 1))))
          (let ((neighbor (grid-get grid dir)))
            (when (and neighbor
                       (not (tile-visited neighbor))
                       (char/= #\# (tile-char neighbor)))
              (setf (tile-visited neighbor) t
                    (tile-dist neighbor) (1+ (tile-dist tile)))
              (containers:insert-item queue dir))))))))

(defun time-saved (grid pos dir)
  "Given POS in GRID and DIR to cheat, determine the time saved by passing
through a wall."
  (let ((tile (grid-get grid pos))
        (neighbor-1 (grid-get grid (+ pos dir)))
        (neighbor-2 (grid-get grid (+ pos (* 2 dir)))))
    (if (and tile
             neighbor-1
             neighbor-2
             (char/= #\# (tile-char tile))
             (char= #\# (tile-char neighbor-1))
             (char/= #\# (tile-char neighbor-2))
             (> (tile-dist neighbor-2) (tile-dist tile)))
        (- (tile-dist neighbor-2) (+ 2 (tile-dist tile)))
        0)))

(defun all-time-saved (grid)
  (let ((result
          (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)))
    (dotimes (y (grid-height grid))
      (dotimes (x (grid-width grid))
        (let* ((pos (complex y x))
               (tile (grid-get grid pos)))
          (when (char/= #\# (tile-char tile))
            (dolist (dir '(#C(-1 0) #C(1 0) #C(0 -1) #C(0 1)))
              (let ((saved (time-saved grid pos dir)))
                (when (> saved 0)
                  (vector-push-extend saved result))))))))
    result))

(defun count-good-cheats (grid threshold)
  (count-if (lambda (x) (>= x threshold)) (all-time-saved grid)))

(defun find-start-pos (grid)
  (loop for pos being the hash-key
          using (hash-value tile) of (grid-tiles grid)
        when (char= #\S (tile-char tile))
          return pos))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((grid (parse-grid (file-to-string (cadr args)))))
            (grid-bfs grid (find-start-pos grid))
            (format t "Good cheats: ~D~%" (count-good-cheats grid 100)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
