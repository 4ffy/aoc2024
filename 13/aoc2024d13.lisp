(ql:quickload :cl-ppcre :silent t)

(defstruct game
  (target-x 0 :type fixnum)
  (target-y 0 :type fixnum)
  (a-dx 0 :type fixnum)
  (a-dy 0 :type fixnum)
  (b-dx 0 :type fixnum)
  (b-dy 0 :type fixnum))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defun parse-game (str)
  "Read numbers from game record STR to produce a game."
  (let ((numbers
          (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" str))))
    (make-game
     :a-dx (first numbers)
     :a-dy (second numbers)
     :b-dx (third numbers)
     :b-dy (fourth numbers)
     :target-x (fifth numbers)
     :target-y (sixth numbers))))

(defun parse-games (str)
  "Parse all games in STR, separated by a blank line."
  (mapcar #'parse-game (ppcre:split "\\n\\n" str)))

(defun solve-game (game)
  "Find the optimal solution for GAME."
  (let ((p (game-a-dx game))
        (q (game-a-dy game))
        (r (game-b-dx game))
        (s (game-b-dy game))
        (x (game-target-x game))
        (y (game-target-y game)))
    (cons
     (/ (- (* s x) (* r y)) (- (* p s) (* q r)))
     (/ (- (* q x) (* p y)) (- (* q r) (* p s))))))

(defun score-game (game)
  "Get the number of tokens required for the optimal solution of GAME."
  (let* ((solution (solve-game game))
         (a-presses (car solution))
         (b-presses (cdr solution)))
    (if (and (integerp a-presses) (integerp b-presses))
        (+ b-presses (* 3 a-presses))
        0)))

(defun score-games (games)
  "Sum the score of all GAMES."
  (reduce (lambda (a x) (+ a (score-game x))) games :initial-value 0))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let ((games (parse-games (file-to-string (cadr args)))))
            (format t "Minimum tokens: ~D~%" (score-games games))
            (mapc
             (lambda (game)
               (incf (game-target-x game) 10000000000000)
               (incf (game-target-y game) 10000000000000))
             games)
            (format t "Minimum tokens: ~D~%" (score-games games)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main ()
  (run sb-ext:*posix-argv*))
