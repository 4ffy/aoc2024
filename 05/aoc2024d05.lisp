#!/usr/bin/env -S sbcl --script
(load "~/.sbclrc")
(ql:quickload :cl-ppcre :silent t)
(rename-package 'cl-ppcre 're)

(defun file-to-string (path)
  (declare (type string path))
  "Read the file PATH to a string."
  (with-open-file
      (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (string-right-trim '(#\u0a) data))))

(defun parse-rule (rule-str)
  (let ((split (re:split "\\|" rule-str)))
    `(,(parse-integer (car split)) . ,(parse-integer (cadr split)))))

(defun parse-rule-section (rules-str)
  (mapcar #'parse-rule (re:split "\\n" rules-str)))

(defun parse-pages (pages-str)
  (mapcar #'parse-integer (re:split "," pages-str)))

(defun parse-pages-section (pages-str)
  (mapcar #'parse-pages (re:split "\\n" pages-str)))

(defun graph-add-pair (graph value-1 value-2)
  (setf (gethash value-1 graph) (cons value-2 (gethash value-1 graph))))

(defun rules-to-graph (rules)
  (let ((graph (make-hash-table)))
    (mapc (lambda (pair) (graph-add-pair graph (car pair) (cdr pair))) rules)
    graph))

(defun graph-has-neighbor-p (graph value-1 value-2)
  (find value-2 (gethash value-1 graph)))

(defun permutations (seq)
  (cond
    ((> 2 (length seq))
     nil)
    ((= 2 (length seq))
     (list (cons (car seq) (cadr seq))))
    (t
     (concatenate
      'list
      (cdr (maplist (lambda (x) (cons (car seq) (car x))) seq))
      (permutations (cdr seq))))))

(defun score-pages (graph pages)
  (if (some
       #'null
       (mapcar
        (lambda (x) (graph-has-neighbor-p graph (car x) (cdr x)))
        (permutations pages)))
      0
      (nth (floor (/ (length pages) 2)) pages)))

(defun score-invalid (graph pages)
  (if (some
       #'null
       (mapcar
        (lambda (x) (graph-has-neighbor-p graph (car x) (cdr x)))
        (permutations pages)))
      (let ((fixed
             (sort pages (lambda (a b) (graph-has-neighbor-p graph a b)))))
        (nth (floor (/ (length fixed) 2)) fixed))
    0))

(defun run (args)
  (declare (type list args))
  (if (= 2 (length args))
      (if (probe-file (cadr args))
          (let* ((data (file-to-string (cadr args)))
                 (split (re:split "\\n\\n" data))
                 (rule-section (car split))
                 (pages-section (cadr split))
                 (rules (parse-rule-section rule-section))
                 (pages (parse-pages-section pages-section))
                 (graph (rules-to-graph rules)))
            (format t
                    "Sum of valid: ~D~%"
                    (reduce
                     (lambda (a x) (+ a (score-pages graph x)))
                     pages
                     :initial-value 0))
            (format t
                    "Sum of fixed: ~D~%"
                    (reduce
                     (lambda (a x) (+ a (score-invalid graph x)))
                     pages
                     :initial-value 0))
            nil)
          (format t "File not found: '~A'~%" (cadr args)))
      (format t "No input file.~%")))

(run sb-ext:*posix-argv*)
