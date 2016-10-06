#! /usr/bin/sbcl --script

(defun read-file (x)
  (let ((in (open x :if-does-not-exist :error)))
    (when in
      (loop for line = (read-line in nil)
            while line do (format t "~a~%" line))
      (close in))))

(defun recursive-read (files)
  (if (car files)
      (progn
        (handler-case (read-file (car files))
          (file-error (c) (format t "~a~%" c)))
        (recursive-read (cdr files)))
      nil))

(defun option-parser (argv &key n)
  (let ((s (car argv)))
    (if (or
         (equal "-n" s)
         (equal "-number" s))
        (option-parser (cdr argv) :n t)
        `('(,n) ,argv))))

(defun read-argv-file (argv)
  (let ((files (option-parser (cdr argv))))
    (recursive-read (cadr files))))

;; run cat
(read-argv-file *posix-argv*)
