(in-package :cl-user)
(defpackage utils
  (:use #:cl)
  (:export #:char-append-to-string
           #:split-string
           #:is-number
           #:parse-float
           #:string-sanitize))
(in-package :utils)


(defun char-append-to-string (char string )
  (concatenate 'string string (string char)))

(defun split-string (line &optional (delimiter #\,))
  (let ((tokens '()) (current ""))
    (dotimes (i (length line))
      (if (char= (char line i) delimiter)
          (progn
            (push current tokens)
            (setq current ""))
          (setq current (char-append-to-string (char line i) current ))))
    (push current tokens)
    (reverse tokens)))

(defun is-number (text)
  (dotimes (i (length text))
    (unless (or (parse-integer (string (char text i)) :junk-allowed t) (char= (char text i) #\.))
      (return-from is-number nil)))
  t)


(defun parse-float (text)
  (let* (
         (num (split-string text #\.))
         (lhs (nth 0 num))
         (rhs (nth 1 num))
         )
    (/ (parse-integer (concatenate 'string lhs rhs)) (expt 10 (length rhs)))
    )
  )

(defun string-sanitize (text)
  (let ((sanitized-text ""))
    (dotimes (i (length text))
      (let ((char (char text i)))
        (when (and (graphic-char-p char)
                   (not (char= char #\u00a0)))
          (setf sanitized-text (concatenate 'string sanitized-text (string char))))))

    ;; NOTE: we don't assume the user intends the numerical representation
    ;; instead, we will convert it in an on-demand basis
    ;; (if (is-number sanitized-text) (print (parse-float sanitized-text)))

    sanitized-text))
