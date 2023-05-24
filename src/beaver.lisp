(in-package :cl-user)
(defpackage beaver
  (:use #:cl)
  (:export
    #:read-csv
    #:clean
    #:get-column
    #:drop-column
    #:get-mean
    #:get-median
    #:melt
    #:transpose
   )
)

(in-package beaver)

(load "./src/utils.lisp")

(defvar max-file-len 10000000)


(defun clean (data)
  "Removes any non-standard characters and whitespaces from matrix"
  (let ((cleaned-data '()))
    (dotimes (i (length data))
      (let ((row '()))
        (dotimes (j (length (nth i data)))
          (push (utils:string-sanitize (nth j (nth i data))) row))
        (push (nreverse row) cleaned-data)))
    (nreverse cleaned-data)))

(defun read-csv (filepath &optional (delem ","))
  "Serializes a CSV file into a 2D matrix"
  (let ((data '()))
  (with-open-file (file filepath :external-format :iso-8859-1)
    (dotimes (i max-file-len)
      (let ((line (read-line file nil)))
        (if (not line) (return))
        (push (utils:split-string line (char delem 0)) data)
        )
      ) 
    (clean (nreverse data)))
  ))

(defun get-mean (seq)
    "Get the mean from a sequence of numbers"    
     (assert seq)
     (/ (reduce '+ (mapcar'utils:parse-float seq)) (length seq))
)

(defun get-median (seq)
  "Get the median from a sequence of numbers"    
  (if (eql (mod (length seq) 2) 0)
      (nth (/ (length seq) 2) seq))
  (let* ((mid (/ (length seq) 2)) (upper (ceiling mid)) (lower (floor mid)))
    (nth (floor (/ (+ upper lower) 2)) seq)
    )
  )

(defun drop-column (data &optional names-to-remove)
  (if (not names-to-remove)
      (return-from drop-column data))
  (let ((output '())
        (indices (loop for name in names-to-remove
                       collect (utils:find-index (nth 0 data) (lambda (x) (string= x name))))))
    (dolist (elem data)
      (let ((row '()))
        (dotimes (i (length elem))
          (unless (member i indices)
            (push (nth i elem) row)))
        (push (reverse row) output)))
    (reverse output)))

(defun get-column (data &optional (name nil))
  (let ((idx nil)
        (head (first data))
        (result '()))

  (if (not name) (return-from get-column head))
    (dotimes (i (length head))
      (when (string= (nth i head) name)
        (setq idx i)
        (return)))
    
    (if (not idx) (return-from get-column nil))

    (loop :for i :from 1 :below (length data)
          :do (push (nth idx (nth i data)) result)
          )
    (nreverse result)))

(defun transpose (data)
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
      (dolist (elem (reverse (get-column data (nth i from-cols))))
      (push (nth i from-cols) melted-types)
        (push elem melted-values)
        )
      )
    (list to-cols melted-values  melted-types)
    ))
