#! /usr/bin/sbcl --script

(defun read-file (x)
  (let ((in (open x :if-does-not-exist :error)))
    (when in
      (loop for line = (read-line in nil)
            while line do (format t "~a~%" line))
      (close in))))

(handler-case
 (loop for x in (cdr *posix-argv*) do
       (read-file x))
       ;; file-error.
 (file-error (c) (format t "~a~%" c)))

;; print argv 1.
;; (format t "~%PATH: ~a~%" (car (cdr *posix-argv*)))
