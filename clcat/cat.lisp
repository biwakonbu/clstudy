#! /usr/bin/sbcl --script

(defparameter *option-hash* (make-hash-table))
(defparameter *default-space-number* 5)

(defun number-of-digits (n)
  (- *default-space-number*
     (truncate (log n 10))))

(defun set-line-number (n line)
  (if (or (and (number-option-p)
               (not (number-nonblank-option-p)))
          (number-nonblank-line-p line))
      (format nil "~vT~a~a" (number-of-digits n) n #\TAB)
      ""))

(defun blank-line-p (line)
  (equal line ""))

(defun not-blank-line-p (line)
  (not (blank-line-p line)))

(defun squeeze-blank-option-p ()
  (gethash '-s *option-hash*))

(defun number-nonblank-option-p ()
  (gethash '-b *option-hash*))

(defun number-option-p ()
  (gethash '-n *option-hash*))

(defun show-ends-option-p ()
  (gethash '-large-e *option-hash*))

(defun squeeze-blank-line-p (line pre-line-blank-p)
  (and (not pre-line-blank-p)
       (blank-line-p line)))

(defun number-nonblank-line-p (line)
  (and (number-nonblank-option-p)
       (not-blank-line-p line)))

(defun output-lines (in)
  (let ((count 1)
        (pre-line-blank-p nil)
        (ends (if (show-ends-option-p) "$" "")))
    (loop for line = (read-line in nil)
       while line do
         (when (or (not (squeeze-blank-option-p))
                   (not-blank-line-p line)
                   (number-nonblank-line-p line)
                   (squeeze-blank-line-p line pre-line-blank-p))
           (setf pre-line-blank-p nil)
           (format t "~a~a~a~%" (set-line-number count line) line ends)
           (when (or (not (number-nonblank-option-p))
                     (number-nonblank-line-p line))
             (incf count)))
         (when (and (blank-line-p line) (not pre-line-blank-p))
           (setf pre-line-blank-p t)))))

(defun read-file (x)
  (with-open-file (in x :if-does-not-exist :error)
    (when in
      (output-lines in))))

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
    ((or (equal "-b" s) (equal "--number-nonblank" s))
     (setf (gethash '-b *option-hash*) t))
    ((or (equal "-n" s) (equal "--number" s))
     (setf (gethash '-n *option-hash*) t))
    ((or (equal "-s" s) (equal "--squeeze-blank" s))
     (setf (gethash '-s *option-hash*) t))
    ((or (equal "-E" s) (equal "--show-ends" s))
     (setf (gethash '-large-e *option-hash*) t))))

(defun read-argv-file (argv)
  (let ((files (option-parser (cdr argv))))
    (recursive-read files)))

;; run cat
(read-argv-file *posix-argv*)
