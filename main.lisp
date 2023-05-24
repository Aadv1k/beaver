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

(defun read-csv (filepath &optional (delem ","))
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



(defun util-is-integer (text)
  (dotimes (i (length text))
    (unless (or (parse-integer (string (char text i)) :junk-allowed t))
      (return-from util-is-integer nil)))
  t)


(defun string-sanitize (text)
  (let ((sanitized-text ""))
    (dotimes (i (length text))
      (let ((char (char text i)))
        (when (and (graphic-char-p char)
                   (not (char= char #\u00a0)))
          (setf sanitized-text (concatenate 'string sanitized-text (string char))))))

    (if (util-is-integer sanitized-text)
        (setq sanitized-text (parse-integer sanitized-text))
    )

    sanitized-text))

(defun clean-csv (data)
  (let ((cleaned-data '()))
    (dotimes (i (length data))
      (let ((row '()))
        (dotimes (j (length (nth i data)))
          (push (string-sanitize (nth j (nth i data))) row))
        (push (nreverse row) cleaned-data)))
    (nreverse cleaned-data)))

(defun get-col (data &optional (name nil))


  (let ((idx nil)
        (head (first data))
        (result '()))

  (if (not name) (return-from get-col head))
    
    (dotimes (i (length head))
      (when (string= (nth i head) name)
        (setq idx i)
        (return)))
    
    (if (not idx) (return-from get-col nil))

    (loop :for i :from 1 :below (length data)
          :do (push (nth idx (nth i data)) result)
          )
    (nreverse result)))


(defun transpose-row-to-col (data)
  (let ((output '()) (top (nth 0 data)))
    (dotimes (i (length top))
      (push '() output)
      (loop :for j :from 0 :below (length data)
            :do (push (nth i (nth j data)) (nth 0 output)))
        (setf (nth 0 output) (reverse (nth 0 output)))
      )
    (reverse output)))

(defun melt (data from-cols to-cols)
  (assert (eq (length to-cols) 2))
  (assert (> (length from-cols) 1))

  (let ((melted-values '()) (melted-types '()))
    (dotimes (i (length from-cols))
      (dolist (elem (reverse (get-col data (nth i from-cols))))
      (push (nth i from-cols) melted-types)
        (push elem melted-values)
        )
      )
    (list to-cols melted-values  melted-types)
    ))

(defun print-matrix (matrix)
  (format t "~{~{~a~^ ~}~%~}" matrix)
)

(defun get-mean (data)
)




(print (get-col (clean-csv (read-csv "btc.csv" ))  "High"))
