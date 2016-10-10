#! /usr/bin/sbcl --script

(defparameter *option-hash* (make-hash-table))
(defparameter *default-space-number* 5)

(defun number-of-digits (n)
  (- *default-space-number*
     (truncate (log n 10))))

(defun set-line-number (n)
  (if (gethash '-n *option-hash*)
      (format nil "~vT~a~a" (number-of-digits n) n #\TAB)
      ""))

(defun read-file (x)
  (with-open-file (in x :if-does-not-exist :error)
    (let ((count 1))
      (when in
        (loop for line = (read-line in nil)
           while line do
             (format t "~a~a~%" (set-line-number count) line)
             (incf count))))))

(defun recursive-read (files)
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
