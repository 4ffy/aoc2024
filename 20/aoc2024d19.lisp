(ql:quickload :cl-containers :silent t)

(declaim
 (optimize (speed 3) (safety 0))
 (ftype (function (string) string) file-to-string)
 (ftype (function (character string) list) split-string)
 (ftype (function (tile) boolean) wall-p)
 (ftype (function (tile) boolean) space-p)
 (ftype (function (grid number) t) grid-get)
 (ftype (function (string) grid) parse-grid)
 (ftype (function (grid (complex fixnum))) grid-bfs)
 (ftype (function ((complex fixnum) (complex fixnum)) fixnum) manhattan)
 (ftype (function ((complex fixnum) fixnum) list) manhattan-neighborhood)
 (ftype (function (grid (complex fixnum) (complex fixnum)) fixnum) time-saved)
 (ftype (function (grid fixnum) (vector fixnum)) all-time-saved)
 (ftype (function (grid fixnum fixnum) fixnum) count-good-cheats)
 (ftype (function (grid) t) find-start-pos)
 (ftype (function (list)) run)
 (ftype (function ()) main)
 (inline wall-p)
 (inline space-p)
 (inline grid-get)
 (inline manhattan))

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

(defun wall-p (tile)
  "Return T if TILE is a wall."
  (char= #\# (tile-char tile)))

(defun space-p (tile)
  "Return T if TILE is not a wall."
  (char/= #\# (tile-char tile)))

(defstruct grid
  (tiles (make-hash-table) :type hash-table)
  (height 0 :type fixnum)
  (width 0 :type fixnum))

(defun grid-get (grid pos)
  "Shorthand for retrieving POS from GRID."
  (gethash pos (grid-tiles grid)))

(defmethod print-object ((obj grid) stream)
  (dotimes (y (grid-height obj))
    (dotimes (x (grid-width obj))
      (let ((tile (grid-get obj (complex y x))))
        (format stream
                "~3A"
                (if (wall-p tile)
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
    (setf
     (tile-visited (grid-get grid start-pos)) t
     (tile-dist (grid-get grid start-pos)) 0)
    (containers:insert-item queue start-pos)
    (loop until (containers:empty-p queue) do
      (let* ((pos (containers:delete-first queue))
             (tile (grid-get grid pos)))
        (dolist (dir (list (1- pos) (1+ pos) (- pos #c(0 1)) (+ pos #c(0 1))))
          (let ((neighbor (grid-get grid dir)))
            (when (and neighbor (not (tile-visited neighbor)) (space-p neighbor))
              (setf
               (tile-visited neighbor) t
               (tile-dist neighbor) (1+ (tile-dist tile)))
              (containers:insert-item queue dir))))))))

(defun manhattan (a b)
  "Calculate the Manhattan distance between complex numbers A and B."
  (+ (abs (- (realpart b) (realpart a)))
     (abs (- (imagpart b) (imagpart a)))))

(defun manhattan-neighborhood (pos dist)
  "Generate a list of complex numbers representing the Manhattan neighborhood of
POS within range DIST."
  (loop for y from (- dist) to dist
        nconc (let ((x-threshold (- dist (abs y))))
                (loop for x from (- x-threshold) to x-threshold
                      collect (+ pos (complex y x))))))

(defun time-saved (grid start-pos end-pos)
  "Find the amount of time saved if cheating from START-POS to END-POS in GRID.
Assume that both START-POS and END-POS are valid open spaces, and that a BFS on
the grid has been run. If the end point has a shorter distance than the start
point, return 0."
  (let ((start-tile (grid-get grid start-pos))
        (end-tile (grid-get grid end-pos))
        (dist (manhattan start-pos end-pos)))
    (if (> (tile-dist end-tile) (tile-dist start-tile))
        (- (tile-dist end-tile) (+ dist (tile-dist start-tile)))
        0)))

(defun all-time-saved (grid cheat-dist)
  "Construct a vector of all positive time saves walking over GRID if allowed to
cheat for CHEAT-DIST moves."
  (let ((result
          (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t)))
    (dotimes (y (grid-height grid))
      (dotimes (x (grid-width grid))
        (let* ((pos (complex y x))
               (tile (grid-get grid pos)))
          (when (space-p tile)
            (dolist (neighbor (manhattan-neighborhood pos cheat-dist))
              (let ((neighbor-tile (grid-get grid neighbor)))
                (when (and neighbor-tile
                           (space-p neighbor-tile)
                           (> (tile-dist neighbor-tile) (tile-dist tile)))
                  (vector-push-extend
                   (time-saved grid pos neighbor) result))))))))
    result))

(defun count-good-cheats (grid cheat-dist threshold)
  "Count the number of cheats in GRID of up to CHEAT-DIST steps that save at
least THRESHOLD moves."
  (count-if (lambda (x) (>= x threshold)) (all-time-saved grid cheat-dist)))

(defun find-start-pos (grid)
  "Find the start postition of GRID."
  (loop for pos being the hash-key
          using (hash-value tile) of (grid-tiles grid)
        when (char= #\S (tile-char tile))
          return pos))

(defun run (args)
  (if (= 4 (length args))
      (let ((grid (parse-grid (file-to-string (cadr args))))
            (cheat-dist (parse-integer (caddr args)))
            (threshold (parse-integer (cadddr args))))
        (grid-bfs grid (find-start-pos grid))
        (format t
                "Good cheats: ~D~%"
                (count-good-cheats grid cheat-dist threshold)))
      (error "Usage: program input cheat-dist threshold")))

(defun main ()
  (handler-case (run sb-ext:*posix-argv*)
    (error (c)
      (format *error-output* "~A~%" c))))
