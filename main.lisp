(defvar max-file-len 10000000)

(defun append-char-to-string (string char)
  (concatenate 'string string (string char)))

(defun split-line (line &optional (delimiter #\,))
  (let ((tokens '()) (current ""))
    (dotimes (i (length line))
      (if (char= (char line i) delimiter)
          (progn
            (push current tokens)
            (setq current ""))
          (setq current (append-char-to-string current (char line i)))))
    (push current tokens)
    (reverse tokens)))

(defun read-csv-file (filepath &optional (delem ","))
  (let ((data '()))
  (with-open-file (file filepath :external-format :iso-8859-1)
    (dotimes (i max-file-len)
      (let ((line (read-line file nil)))
        (if (not line) (return))
        (push (split-line line (char delem 0)) data)
        )
      ) 
    (nreverse data))
  ))

(defun string-sanitize (text)
  (let ((sanitized-text ""))
    (dotimes (i (length text))
      (let ((char (char text i)))
        (when (and (graphic-char-p char)
                   (not (char= char #\u00a0)))
          (setf sanitized-text (concatenate 'string sanitized-text (string char))))))
    sanitized-text))


(defun clean-csv (data)
  (let ((cleaned-data '()))
    (dotimes (i (length data))
      (let ((row '()))
        (dotimes (j (length (nth i data)))
          (push (string-sanitize (nth j (nth i data))) row))
        (push (nreverse row) cleaned-data))) ; Reverse the row to maintain the original order
    (nreverse cleaned-data))) ; Reverse the cleaned-data to maintain the original order


;;(defun remove-special-characters (string) (let ((special-chars "!@#$%^&*()_+|{}[];':\",.<>/?\\`~-=")) (replace-regexp-in-string (format "[%s]" special-chars) "" string)))


;; expects flat data only
(defun transpose-row-to-col (data)
  (let ((output '()) (top (nth 0 data)))
    (dotimes (i (length top))
      (push '() output)
      (loop :for j :from 0 :below (length data)
            :do (push (nth i (nth j data)) (nth 0 output)))
        (setf (nth 0 output) (reverse (nth 0 output)))
      )
    (reverse output)))


(print (clean-csv (read-csv-file "data.csv" ";")))
