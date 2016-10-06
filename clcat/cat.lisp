#! /usr/bin/sbcl --script

(defun read-file (x)
  (let ((in (open x :if-does-not-exist :error)))
    (when in
      (loop for line = (read-line in nil)
            while line do (format t "~a~%" line))
      (close in))))

(defun recursive-read (argv)
  (if (car argv)
      (progn
        (handler-case (read-file (car argv))
          (file-error (c) (format t "~a~%" c)))
        (recursive-read (cdr argv)))
      nil))

(defun read-argv-file (argv)
  (recursive-read (cdr argv)))

(read-argv-file *posix-argv*)
