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


(defun matrix-vertical-split (mat idx)
  (let* ((rows (length mat))
         (cols (length (aref mat 0)))
         (m (make-array rows))
         (v (make-array rows)))
    (loop for i from 0 below rows
          for row = (aref mat i)
          do (setf (aref m i)
                   (concatenate 'vector
                     (subseq row 0 idx)
                     (subseq row (1+ idx) cols))
                   (aref v i)
                   (aref row idx)))
    (values m v)))
(assert (multiple-value-bind (mat vec)
          (matrix-vertical-split
            #(#(1 2 3)
              #(2 4 6)
              #(3 6 9))
            1)
          (let ((m #(#(1 3)
                     #(2 6)
                     #(3 9))))
            (and (loop for i from 0 below (length m)
                       always (equalp (aref m i) (aref mat i)))
                 (equalp vec #(2 4 6))))))


(defun remove-data-column (data label)
  (multiple-value-bind (m v)
    (matrix-vertical-split
      (cdr (assoc :data data))
      (position label
                (cdr (assoc :label data))
                :test #'equal))
    (values (list `(:label . ,(remove label (cdr (assoc :label data)) :test #'equal))
                  `(:data . ,m))
            v)))


(defun equal-all (seq target)
  (loop for item across seq
        always (equal item target)))
(assert (equal-all #(41 41 41) 41))


(defun freq (seq)
  (let ((tbl (make-hash-table :test #'equal))
        (kinds (remove-duplicates seq :test #'equalp)))
    (loop for k across kinds
          do (setf (gethash k tbl) 0))
    (loop for d across seq
          for p = (gethash d tbl)
          do (setf (gethash d tbl) (1+ p)))
    tbl))
(assert (equalp (let ((tbl (make-hash-table :test #'equal)))
                  (setf (gethash "a" tbl) 2
                        (gethash "b" tbl) 3
                        (gethash "c" tbl) 1)
                  tbl)
                (freq #("a" "b" "a" "c" "b" "b"))))


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
(assert (= 3 (mode #(4 1 3 4 1 2 3 2 2 3 1 3))))


(defun entropy (seq)
  (let ((tbl (freq seq))
        (len (length seq))
        (sum 0))
    (with-hash-table-iterator
      (iterator tbl)
      (loop (multiple-value-bind (entry-p key cnt) (iterator)
              (declare (ignore key))
              (if entry-p
                (let ((p (float (/ cnt len))))
                  (decf sum (* p (log p 2))))
                (return sum)))))))
(assert (and (= 0 (entropy #(1 1 1 1)))
             (= 1 (entropy #(3 1 1 3)))))


(defun where (test seq)
  (loop for i from 0 below (length seq)
        when (funcall test (elt seq i))
        collect i))
(assert (equal (where #'evenp #(1 2 3 4 5 6)) '(1 3 5)))


(defun col-where (test seq idx)
  (loop for row across seq
        when (funcall test row)
        collect (aref row idx)))
(assert (equal '("one" "three" "five")
               (col-where (lambda (row) (oddp (aref row 1)))
                          #(#("one" 1) #("two" 2) #("three" 3) #("four" 4) #("five" 5))
                          0)))


(defun select (arr pos)
  (let ((res (make-array (length pos))))
    (loop for i from 0 below (length pos)
          do (setf (aref res i) (aref arr (elt pos i))))
    res))
(assert (equalp #(5 1 9) (select #(9 3 1 5 2 8) #(3 2 0))))


(defun partial (fn &rest before)
  (lambda (&rest after)
    (apply fn (append before after))))
(assert (= (funcall (partial #'* 10) 2 3) (* 10 2 3)))


(defun gain (data attr label)
  (let* ((label-values (extract attr label))
         (tbl (freq label-values))
         (entd (entropy data))
         (sum 0)
         (len (length data)))
    (with-hash-table-iterator
      (iterator tbl)
      (loop (multiple-value-bind (entry-p key cnt) (iterator)
              (if entry-p
                (let ((p (float (/ cnt len))))
                  (incf sum
                        (* p (entropy (select data (where (partial #'equal key) label-values))))))
                (return (- entd sum))))))))


(defun id3 (data attr)
  (let ((label (cdr (assoc :label attr))))
    (cond
      ((= 1 (length (remove-duplicates data :test #'equal))) (aref data 0))
      ((= 0 (length label)) (mode data))
      (t
        (let* ((alpha (let ((ml (elt label 0)))
                        (loop for l in label
                              with mg = 0
                              for g = (gain data attr l)
                              when (< mg g)
                              do (setf ml l))
                        ml))
               (alpha-values (extract attr alpha))
               (value (remove-duplicates alpha-values :test #'equal)))
          (list alpha (loop for v across value
                for place = (where (partial #'equal v) alpha-values)
                for dv = (select data place)
                if (= 0 (length dv))
                collect (list v (mode data))
                else
                collect (let ((attr2 (remove-data-column attr alpha)))
                          (setf (cdr (assoc :data attr2))
                                (select (cdr (assoc :data attr2)) place))
                          (list v (id3 dv attr2))))))))))


; Print decision tree of contact-lenses
(multiple-value-bind (attr ans)
  (remove-data-column data "contact-lenses")
  (print (id3 ans attr)))
