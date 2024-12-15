(ql:quickload :cl-ppcre :silent t)

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defstruct grid
  (data (make-hash-table) :type hash-table)
  (height 0 :type fixnum)
  (width 0 :type fixnum)
  (robot (complex 0) :type (complex fixnum)))

(defun print-grid (grid)
  "Print GRID to standard output."
  (loop for y from 0 below (grid-height grid) do
    (loop for x from 0 below (grid-width grid) do
      (format t "~C" (gethash (complex y x) (grid-data grid))))
    (format t "~%")))

(defun parse-grid (str)
  "Read a grid from STR."
  (let* ((lines (ppcre:split "\\n" str))
         (height (length lines))
         (width (length (car lines)))
         (data (make-hash-table :size (* height width)))
         (robot-pos (complex 0)))
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let ((pos (complex y x))
              (chr (char (nth y lines) x)))
          (when (char= chr #\@)
            (setf robot-pos pos))
          (setf (gethash pos data) chr))))
    (make-grid :data data :height height :width width :robot robot-pos)))

(defun move-unconditional (grid pos dir)
  "Move the item at POS in GRID in the direction DIR. Overwrite the tile that
was present at that location and leave an empty tile in the previous space."
  (cond
    ((char= dir #\^)
     (progn
       (setf (gethash (1- pos) (grid-data grid)) (gethash pos (grid-data grid)))
       (setf (gethash pos (grid-data grid)) #\.)))
    ((char= dir #\v)
     (progn
       (setf (gethash (1+ pos) (grid-data grid)) (gethash pos (grid-data grid)))
       (setf (gethash pos (grid-data grid)) #\.)))
    ((char= dir #\<)
     (progn
       (setf (gethash (- pos #C(0 1)) (grid-data grid))
             (gethash pos (grid-data grid)))
       (setf (gethash pos (grid-data grid)) #\.)))
    ((char= dir #\>)
     (progn
       (setf (gethash (+ pos #C(0 1)) (grid-data grid))
             (gethash pos (grid-data grid)))
       (setf (gethash pos (grid-data grid)) #\.)))
    (t
     (error (format nil "Invalid direction ~@C~%" dir)))))

(defun can-move-p (grid pos dir)
  "Determine whether the GRID item at POS can move in the direction DIR. In
general, a tile is allowed to move in a direction if the current tile in that
direction is either empty or also allowed to move in that direction. Walls block
movement, so movability can be determined by scanning each tile across the given
direction until either a wall or an empty space is reached. Wide boxes require
extra handling across the vertical axis."
  (let ((tile (gethash pos (grid-data grid))))
    (cond
      ((char= #\. tile) t)
      ((char= #\# tile) nil)
      ((char= #\^ dir)
       (cond
         ((char= #\[ tile)
          (and (can-move-p grid (1- pos) #\^)
               (can-move-p grid (+ pos #C(-1 1)) #\^)))
         ((char= #\] tile)
          (and (can-move-p grid (1- pos) #\^)
               (can-move-p grid (+ pos #C(-1 -1)) #\^)))
         (t
          (can-move-p grid (1- pos) #\^))))
      ((char= #\v dir)
       (cond
         ((char= #\[ tile)
          (and (can-move-p grid (1+ pos) #\v)
               (can-move-p grid (+ pos #C(1 1)) #\v)))
         ((char= #\] tile)
          (and (can-move-p grid (1+ pos) #\v)
               (can-move-p grid (+ pos #C(1 -1)) #\v)))
         (t
          (can-move-p grid (1+ pos) #\v))))
      ((char= #\< dir)
       (can-move-p grid (- pos #C(0 1)) #\<))
      ((char= #\> dir)
       (can-move-p grid (+ pos #C(0 1)) #\>))
      (t
       (error (format nil "Invalid direction ~@C~%" dir))))))

(defun move (grid pos dir)
  "Move the GRID item at POS in the direction DIR if possible. If this movement
affects other objects, move them as well. Return T if a movement was actually
performed, otherwise return NIL."
  ;; I hate it here.
  (let ((tile (gethash pos (grid-data grid))))
    (cond
      ((not (can-move-p grid pos dir)) nil)
      ((char= #\. tile) nil)
      ((char= #\^ dir)
       (cond
         ((char= #\[ tile)
          (progn
            (move grid (+ pos #C(-1 0)) #\^)
            (move grid (+ pos #C(-1 1)) #\^)
            (move-unconditional grid pos #\^)
            (move-unconditional grid (+ pos #C(0 1)) #\^)
            t))
         ((char= #\] tile)
          (progn
            (move grid (+ pos #C(-1 0)) #\^)
            (move grid (+ pos #C(-1 -1)) #\^)
            (move-unconditional grid pos #\^)
            (move-unconditional grid (+ pos #C(0 -1)) #\^)
            t))
         (t
          (progn
            (move grid (1- pos) #\^)
            (move-unconditional grid pos #\^)
            t))))
      ((char= #\v dir)
       (cond
         ((char= #\[ tile)
          (progn
            (move grid (+ pos #C(1 0)) #\v)
            (move grid (+ pos #C(1 1)) #\v)
            (move-unconditional grid pos #\v)
            (move-unconditional grid (+ pos #C(0 1)) #\v)
            t))
         ((char= #\] tile)
          (progn
            (move grid (+ pos #C(1 0)) #\v)
            (move grid (+ pos #C(1 -1)) #\v)
            (move-unconditional grid pos #\v)
            (move-unconditional grid (+ pos #C(0 -1)) #\v)
            t))
         (t
          (progn
            (move grid (1+ pos) #\v)
            (move-unconditional grid pos #\v)
            t))))
      ((char= #\< dir)
       (progn
         (move grid (- pos #C(0 1)) #\<)
         (move-unconditional grid pos #\<)
         t))
      ((char= #\> dir)
       (progn
         (move grid (+ pos #C(0 1)) #\>)
         (move-unconditional grid pos #\>)
         t))
      (t
       (error (format nil "Invalid direction ~@C~%" dir))))))

(defun move-robot (grid moves)
  "Move the robot in GRID according to the sequence of moves MOVES."
  (loop for move across moves do
    (cond
      ((char= #\^ move)
       (when (move grid (grid-robot grid) #\^)
         (decf (grid-robot grid))))
      ((char= #\v move)
       (when (move grid (grid-robot grid) #\v)
         (incf (grid-robot grid))))
      ((char= #\< move)
       (when (move grid (grid-robot grid) #\<)
         (decf (grid-robot grid) #C(0 1))))
      ((char= #\> move)
       (when (move grid (grid-robot grid) #\>)
         (incf (grid-robot grid) #C(0 1))))
      (t
       nil))))

(defun score-grid (grid)
  "For each box in GRID, sum 100 times the y position plus the x position."
  (loop for pos being each hash-key using (hash-value chr) of (grid-data grid)
        when (or (char= #\O chr) (char= #\[ chr))
          sum (+ (imagpart pos) (* 100 (realpart pos)))))

(defun widen-grid (grid)
  "Construct a wide version of GRID."
  (let* ((height (grid-height grid))
         (width (* 2 (grid-width grid)))
         (robot-pos
           (+ (grid-robot grid) (complex 0 (imagpart (grid-robot grid)))))
         (data (make-hash-table :size (* width height))))
    (loop
      for pos being each hash-key
        using (hash-value chr) of (grid-data grid) do
          (let ((new-pos (+ pos (complex 0 (imagpart pos)))))
            (cond
              ((char= #\O chr)
               (progn
                 (setf (gethash new-pos data) #\[)
                 (setf (gethash (+ new-pos #C(0 1)) data) #\])))
              ((char= #\@ chr)
               (progn
                 (setf (gethash new-pos data) #\@)
                 (setf (gethash (+ new-pos #C(0 1)) data) #\.)))
              (t
               (progn
                 (setf (gethash new-pos data) chr)
                 (setf (gethash (+ new-pos #C(0 1)) data) chr))))))
    (make-grid :data data :height height :width width :robot robot-pos)))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let* ((data (file-to-string (cadr args)))
                 (split (ppcre:split "\\n\\n" data))
                 (grid (parse-grid (car split)))
                 (wide (widen-grid grid))
                 (moves (cadr split)))
            (move-robot grid moves)
            (move-robot wide moves)
            ;; (print-grid grid)
            ;; (print-grid wide)
            (format t "Narrow GPS sum: ~D~%" (score-grid grid))
            (format t "Wide GPS sum: ~D~%" (score-grid wide)))
          (format *error-output* "File not found: '~A'~%" (cadr args)))
      (format *error-output* "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
