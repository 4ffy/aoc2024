(defconstant +prune-mask+ #xFFFFFF)

(defconstant +step-1-multiplier+ 64)

(defconstant +step-2-divisor+ 32)

(defconstant +step-3-multiplier+ 2048)

(declaim (ftype (function (fixnum fixnum) fixnum) mix)
         (ftype (function (fixnum) fixnum) prune)
         (ftype (function (fixnum) fixnum) step-1)
         (ftype (function (fixnum) fixnum) step-2)
         (ftype (function (fixnum) fixnum) step-3)
         (ftype (function (fixnum) fixnum) next-secret-number)
         (inline mix)
         (inline prune)
         (inline step-1)
         (inline step-2)
         (inline step-3)
         (inline next-secret-number))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\Newline) data))))

(defun split-string (sep str)
  "Split STR into a list using the separator SEP."
  (loop for start = 0 then (1+ finish)
        for finish = (position sep str :start start)
        collecting (subseq str start finish)
        until (null finish)))

(defun parse-data (str) (mapcar #'parse-integer (split-string #\Newline str)))

(defun mix (x y) (boole boole-xor x y))

(defun prune (x) (boole boole-and x +prune-mask+))

(defun step-1 (x) (prune (mix x (* x +step-1-multiplier+))))

(defun step-2 (x) (prune (mix x (floor x +step-2-divisor+))))

(defun step-3 (x) (prune (mix x (* x +step-3-multiplier+))))

(defun next-secret-number (x) (step-3 (step-2 (step-1 x))))

(defun nth-secret-number (start n)
  (let ((result start))
    (dotimes (_ n) (setf result (next-secret-number result)))
    result))

(defun all-secret-numbers (start n)
  (loop for secret = start then (next-secret-number secret)
        for _ from 0 to n
        collecting secret))

(defun secret-number-prices (numbers) (mapcar (lambda (x) (rem x 10)) numbers))

(defun secret-number-deltas (numbers)
  (butlast
   (maplist
    (lambda (x)
      (when (cadr x)
        (- (cadr x) (car x))))
    numbers)))

(defun register-sell-times (cache prices deltas)
  (let ((cache-2-lmao (make-hash-table :test #'equal)))
    (loop for d = deltas then (cdr d)
          for p = (nthcdr 4 prices) then (cdr p)
          until (null (fourth d))
          do (let* ((delta (subseq d 0 4)) (price (car p)))
               (setf (gethash delta cache-2-lmao)
                     (nconc (gethash delta cache-2-lmao) (list price)))))
    (loop for key being each hash-key using (hash-value val) of cache-2-lmao
          do (setf (gethash key cache)
                   (+ (or (gethash key cache) 0) (reduce #'max val))))))

(defun find-sell-times (start-numbers n)
  (let ((cache (make-hash-table :test #'equal)))
    (dolist (start start-numbers)
      (let* ((numbers (all-secret-numbers start n))
             (prices (secret-number-prices numbers))
             (deltas (secret-number-deltas prices)))
        (register-sell-times cache prices deltas)))
    cache))

(defun best-sell-result (cache)
  (loop for val being the hash-values of cache
        maximize val))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let* ((numbers (parse-data (file-to-string (cadr args))))
                 (two-thousandth
                   (mapcar (lambda (x) (nth-secret-number x 2000)) numbers))
                 (sell-result (find-sell-times numbers 2000)))
            (format t "Sum of 2000th: ~D~%" (reduce #'+ two-thousandth))
            (format t "Best sell result: ~D~%" (best-sell-result sell-result)))
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(defun main () (run *posix-argv*))
