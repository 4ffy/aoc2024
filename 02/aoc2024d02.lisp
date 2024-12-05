#!/usr/bin/env -S sbcl --script

(defun pairs (lst)
  "Construct a list of cons pairs of each adjacent pair of items in LST."
  (loop for pair = lst then (cdr pair)
        collecting `(,(car pair) . ,(cadr pair))
        until (= 2 (length pair))))

(defun sublists (lst)
  "Construct sublists of LST where each sublist has one element removed."
  (loop for i = 0 then (1+ i)
        collecting (concatenate 'list (subseq lst 0 i) (subseq lst (1+ i)))
        until (= i (1- (length lst)))))

(defun strictly-increasing-p (lst)
  "Determine whether LST is strictly increasing."
  (notany #'null
          (mapcar (lambda (cons) (< (car cons) (cdr cons)))
                  (pairs lst))))

(defun strictly-decreasing-p (lst)
  "Determine whether LST is strictly decreasing."
  (notany #'null
          (mapcar (lambda (cons) (> (car cons) (cdr cons)))
                  (pairs lst))))

(defun smoothly-changing-p (lst)
  "Determine whether LST has adjacent values with a difference of
no more than 3."
  (notany #'null
          (mapcar (lambda (cons) (<= (abs (- (car cons) (cdr cons))) 3))
                  (pairs lst))))

(defun naively-safe-p (lst)
  "Determine whether LST is safe without removal of any element."
  (and (smoothly-changing-p lst)
       (or (strictly-increasing-p lst) (strictly-decreasing-p lst))))

(defun safe-p (lst)
  "Determine whether LST is safe."
  (or (naively-safe-p lst)
      (some #'naively-safe-p (sublists lst))))

(defun count-naively-safe (records)
  "Given a list of lists of numbers RECORDS, determine how many records are safe
without removing any elements."
  (count t (mapcar #'naively-safe-p records)))

(defun count-safe (records)
  "Given a list of records, determine how many are safe."
  (count t (mapcar #'safe-p records)))

(defun file-to-string (path)
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun split-string (sep str)
  "Split STR into a list using the separator SEP."
  (loop for start = 0 then (1+ finish)
        for finish = (position sep str :start start)
        collecting (subseq str start finish)
        until (null finish)))

(defun split-lines (str)
  "Split STR into a list for each line. Remove empty lines."
  (remove "" (split-string #\u0a str) :test #'equalp))

(defun parse-line (str)
  "Parse STR as a space-delimited list of numbers."
  (mapcar #'parse-integer (split-string #\  str)))

(defun run (args)
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let* ((data (file-to-string (cadr args)))
                 (lines (split-lines data))
                 (records (mapcar #'parse-line lines)))
            (format t "Naively safe: ~D.~%" (count-naively-safe records))
            (format t "Safe: ~D.~%" (count-safe records)))
          (progn
            (format t "File not found: '~A'.~%" (cadr args))))
      (progn
        (format t "No input file.~%"))))

(run sb-ext:*posix-argv*)
