(defparameter data
  (with-open-file
    (in (merge-pathnames "contact-lenses.lisp" (truename "./")) :direction :input)
      (read in nil)))

(defun extract (data label)
  (let* ((col (position label (cdr (assoc :label data)) :test #'equal))
         (dat (cdr (assoc :data data)))
         (len (length dat))
         (arr (make-array len)))
    (dotimes (i len)
      (setf (aref arr i) (aref (aref dat i) col)))
    arr))

(defun equal-all (seq target)
  (loop for item across seq
        always (equal item target)))

(defun freq (seq)
  (let ((tbl (make-hash-table :test #'equal))
        (kinds (remove-duplicates seq)))
    (loop for k across kinds
          do (setf (gethash k tbl) 0))
    (loop for d across seq
          for p = (gethash d tbl)
          do (setf (gethash d tbl) (1+ p)))
    tbl))

(defun mode (seq)
  (let ((tbl (freq seq))
        (m nil)
        (v 0))
    (with-hash-table-iterator
      (iterator tbl)
      (loop (multiple-value-bind (entry-p key value) (iterator)
              (if entry-p
                (when (< v value)
                  (setf m key v value))
                (return m)))))))

(defun entropy (seq)
  (let ((tbl (freq seq))
        (len (length seq))
        (sum 0))
    (with-hash-table-iterator
      (iterator tbl)
      (loop (multiple-value-bind (entry-p key cnt) (iterator)
              (if entry-p
                (let ((p (float (/ cnt len))))
                  (decf sum (* p (log p 2))))
                (return sum)))))))

#|
Gain(D,label)
= (entropy D) - (neg(x in label) / all) * (entropy D, x in neg)
              - (neg(y in label) / all) * (entropy D, y in neg)
              - (neg(z in label) / all) * (entropy D, z in neg)
              ...
|#
(defun gain (data attr label)
  (let ((tbl (freq seq)))
    ()))

(defun id3 (data attr)
  (let ((root (list))
        (ans (remove-duplicates data)))
    (cond
      ((= 1 (length data)) (aref data 0))
      ((= 0 (length attr)) (mode data))
      (t
        (let ((alpha nil))
 
          alpha)))))

(print (extract data "contact-lenses"))
(print (id3 (extract data "contact-lenses") data))
(assert (equal (mode #("a" "b" "c" "d" "c" "d" "d" "c" "c" "e" "a")) "c"))
(assert (= (entropy #(1 2 1 1 2 2)) 1))
