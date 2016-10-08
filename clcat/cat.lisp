#! /usr/bin/sbcl --script

(defparameter *option-hash* (make-hash-table))

(defun set-line-number (n)
  (if (gethash '-n *option-hash*)
      (format nil "~5T~a~a" n #\TAB)
      ""))

(defun read-file (x)
  (let ((in (open x :if-does-not-exist :error)) (count 1))
    (when in
      (loop for line = (read-line in nil)
         while line do
           (format t "~a~a~%" (set-line-number count) line)
           (incf count))
      (close in))))

(defun recursive-read (files)
  (maphash #'(lambda (key value)
               (format t "~A => ~A~%" key value))
           *option-hash*)
  (if (car files)
      (progn
        (handler-case (read-file (car files))
          (file-error (c) (format t "~a~%" c)))
        (recursive-read (cdr files)))
      nil))

(defun option-parser (argv &key n)
  (let ((s (car argv)))
    (if (or (equal "-n" s) (equal "-number" s))
        (progn
          (setf (gethash '-n *option-hash*) t)
          (option-parser (cdr argv) :n t))
        argv)))

(defun read-argv-file (argv)
  (let ((files (option-parser (cdr argv))))
    (recursive-read files)))

;; run cat
(read-argv-file *posix-argv*)
