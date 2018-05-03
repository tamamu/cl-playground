(defun d (x y)
  "Euclidean distance between two sequences x and y"
  (let ((total 0)
        (len (length x)))
    (dotimes (i len)
      (incf total
            (expt (- (aref x i) (aref y i)) 2)))
    (sqrt total)))

(defun distance-matrix (dataset &optional (operator #'-))
  (let* ((len (length dataset))
         (matrix (make-array `(,len ,len) :initial-element 0)))
    (dotimes (i len)
      (dotimes (j len)
        (setf (aref matrix i j)
              (if (< j i)
                  (aref matrix j i)
                  (funcall operator
                           (aref dataset i)
                           (aref dataset j))))))
    matrix))

(defun range (a &optional (b nil))
  (if (null b)
      (loop for i from 0 below a collect i)
      (loop for i from a below b collect i)))

(defun combination (n list)
  (cond
    ((or (= n 0) (null list)) nil)
    ((= n 1) (mapcar (lambda (inner-list) (cons inner-list nil)) list))
    (t
     (append
       (mapcar (lambda (inner-list)
                 (cons (car list) inner-list))
               (combination (1- n) (rest list)))
       (combination n (rest list))))))

(defun k-medoids (k dataset)
  (let* ((num-instances (length dataset))
         (medoid-indices (combination k (range num-instances)))
         (matrix (distance-matrix dataset #'d))
         (min-medoid nil)
         (min-distance nil)
         (min-assign nil)
         (iter-count 0)
         (max-iter (length medoid-indices))
         (hundreadth (truncate max-iter 100)))
    (format t "Created distance matrix~%")
    (loop for pair in medoid-indices do
          (let ((assign (make-array num-instances :initial-element nil)))
            (loop for row from 0 below num-instances do
                  (loop for idx in pair
                        with m = (aref matrix (first pair) row)
                        with midx = (first pair) do
                        (if (< (aref matrix idx row) m)
                            (setf m (aref matrix idx row)
                                  midx idx))
                        finally (setf (aref assign row) midx)))
            (let ((current-distance
                    (loop for i from 0 below num-instances
                          for medoid = (aref assign i)
                          sum (aref matrix medoid i))))
              (when (or (null min-distance)
                        (< current-distance min-distance))
                (format t "update:~A < ~A~%" current-distance min-distance)
                (setf min-distance current-distance
                      min-medoid pair
                      min-assign assign))))
          (when (= (mod (incf iter-count) hundreadth) 0)
            (format t "iter:~A/~A~%" (incf iter-count) max-iter)))
    (format t "medoid:~A~%distance:~A~%assign:~A~%"
            min-medoid min-distance min-assign)
    (values min-medoid min-distance min-assign)))

(defun split (str delim)
  (let ((res (make-array 0 :element-type 'string
                           :fill-pointer 0
                           :adjustable t)))
    (loop for i from 0 below (length str)
          with start = 0
          when (eq (char str i) delim)
          do (vector-push-extend (subseq str start i) res)
             (setf start (1+ i))
          finally (let ((tail (subseq str start)))
                    (when tail
                      (vector-push-extend tail res))))
    res))

(defun loadtxt (path delimiter)
  (let ((datum (make-array 0 :element-type 'array
                             :fill-pointer 0
                             :adjustable t)))
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil) while line do
            (vector-push-extend (split line delimiter) datum)))
    datum))

(defun loadraw (path delimiter)
  (map 'vector
       (lambda (row) (map 'vector #'read-from-string row))
       (loadtxt path delimiter)))

(defun calc-sse (dataset centers membership)
  (loop for center in centers ;; for all centers
        sum (loop for i from 0 below (length membership) ;; for all members in the cluster
                  if (= center (aref membership i))
                  sum (d (aref dataset i) (aref dataset center)))))

(defun main (path)
  (let* ((dataset (loadraw path #\,))
         (height (length dataset))
         (width (length (aref dataset 0))))
    (multiple-value-bind (medoids distance assign)
      (k-medoids 3 dataset)
      (declare (ignorable medoids))
      (with-open-file (out "./distance" :direction :output :if-exists :supersede)
        (format out "Distance:~A~%SSE:~A~%" distance (calc-sse dataset medoids assign)))
      (with-open-file (out "./output" :direction :output :if-exists :supersede)
        (dotimes (i height)
          (format out "~A" (aref assign i))
          (let ((row (aref dataset i)))
            (loop for j from 0 below width do
                  (format out " ~A" (aref row j))
                  finally (format out "~%"))))))))
