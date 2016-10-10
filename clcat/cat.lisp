#! /usr/bin/sbcl --script

(defparameter *option-hash* (make-hash-table))
(defparameter *default-space-number* 5)

(defun number-of-digits (n)
  (- *default-space-number*
     (truncate (log n 10))))

(defun set-line-number (n)
  (if (number-option-p)
      (format nil "~vT~a~a" (number-of-digits n) n #\TAB)
      ""))

(defun blank-line-p (line)
  (equal line ""))

(defun not-blank-line-p (line)
  (not (blank-line-p line)))

(defun squeeze-blank-option-p ()
  (gethash '-s *option-hash*))

(defun number-option-p ()
  (gethash '-n *option-hash*))

(defun read-file (x)
  (with-open-file (in x :if-does-not-exist :error)
    (let ((count 1) (pre-line-blank-p nil))
      (when in
        (loop for line = (read-line in nil)
           while line do
             (when (or (not (squeeze-blank-option-p))
                       (not-blank-line-p line)
                       (and (not pre-line-blank-p) (blank-line-p line)))
               (format t "~a~a~%" (set-line-number count) line)
               (setf pre-line-blank-p nil)
               (incf count))
             (when (and (blank-line-p line) (not pre-line-blank-p))
               (setf pre-line-blank-p t)))))))

(defun recursive-read (files)
  (if (car files)
      (progn
        (handler-case (read-file (car files))
          (file-error (c) (format t "~a~%" c)))
        (recursive-read (cdr files)))
      nil))

(defun option-parser (argv)
  (if (option-check (car argv))
      (option-parser (cdr argv))
      argv))

(defun option-check (s)
  (cond
    ((or (equal "-n" s) (equal "--number" s))
     (setf (gethash '-n *option-hash*) t))
    ((or (equal "-s" s) (equal "--squeeze-blank" s))
     (setf (gethash '-s *option-hash*) t))))

(defun read-argv-file (argv)
  (let ((files (option-parser (cdr argv))))
    (recursive-read files)))

;; run cat
(read-argv-file *posix-argv*)
