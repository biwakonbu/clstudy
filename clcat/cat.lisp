#! /usr/bin/sbcl --script

(let ((in (open (car (cdr *posix-argv*)) :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do (format t "~a~%" line))
    (close in)))

(prin1 (car (cdr *posix-argv*)))
