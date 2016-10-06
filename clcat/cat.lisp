#! /usr/bin/sbcl --script

(defun read-file (f)
  (loop for x in (cdr f) do
        (let ((in (open x :if-does-not-exist :error)))
          (when in
            (loop for line = (read-line in nil)
                  while line do (format t "~a~%" line))
            (close in)))))

(handler-case (read-file *posix-argv*)
 ;; file-error.
 (file-error (c) (format t "~a~%" c)))

;; print argv 1.
;; (format t "~%PATH: ~a~%" (car (cdr *posix-argv*)))
