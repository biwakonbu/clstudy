#! /usr/bin/sbcl --script

(handler-case
 (let ((in (open (car (cdr *posix-argv*)) :if-does-not-exist :error)))
   (when in
     (loop for line = (read-line in nil)
           while line do (format t "~a~%" line))
     (close in)))
 ;; file-error.
 (file-error (c) (format t "~a~%" c)))

;; print argv 1.
(format t "~%PATH: ~a~%" (car (cdr *posix-argv*)))
