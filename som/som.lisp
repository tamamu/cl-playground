(setf *read-default-float-format* 'double-float)

(defun d (x y)
  (progn
    (loop for xi across x
          for yi across y
          sum (expt (- xi yi) 2))))

(defun rand-matrix (w h dim &key (mean 0.5) (range 0.05))
  (let ((mat (make-array `(,w ,h))))
    (dotimes (i h)
      (dotimes (j w)
        (let ((data (make-array dim)))
          (dotimes (k dim)
            (setf (aref data k)
                  (+ mean (* range (- (/ (random 2000) 1000) 1.0)))))
          (setf (aref mat i j)
                data))))
    mat))

(defun find-nearest-index (mat data &key (distance #'d))
  (let ((dim (array-dimensions mat))
        (x 0)
        (y 0))
    (loop for i from 0 below (first dim)
          with mind = (funcall distance data (aref mat 0 0)) do
          (loop for j from 0 below (second dim)
                for v = (aref mat i j) do
                ;(format t "(~A,~A):~A~%" i j v)
                (let ((dist (funcall distance data v)))
                  (when (< dist mind)
                    (setf x j
                          y i
                          mind dist)))))
    (values x y)))

(defun neighborhood (num-of-teachers size tm)
  (* (/ size 2) (exp (/ (- tm) (/ num-of-teachers 4)))))

(defun learning-radius (num-of-teachers size tm dist)
  (let ((d2 (expt (- dist) 2))
        (n2 (expt (neighborhood num-of-teachers size tm) 2)))
    (exp (/ d2 (* 2 n2)))))

(defun learning-ratio (num-of-teachers tm &key (rate 0.2))
  (* rate (exp (/ (- tm) (/ num-of-teachers 4)))))

(defun drop (mat x y v rate num-of-teachers tm)
  (let ((dim (array-dimensions mat))
        (num-of-features (length (aref mat 0 0))))
    (loop for i from 0 below (first dim) do
          (loop for j from 0 below (second dim)
                for n = (max (abs (- y i))
                             (abs (- x j)))
                for data = (aref mat x y) do
                (loop for k from 0 below num-of-features do
                      (let* ((ratio (learning-ratio num-of-teachers tm :rate rate))
                             (radius (learning-radius num-of-teachers (first dim) tm n))
                             (diff (- (aref v k) (aref data k)))
                             (next (+ (aref data k) (* ratio radius diff))))
                        (setf (aref data k)
                              (cond
                                ((< 0.99d0 next) 1.0d0)
                                ((< next 0.01d0) 0.0d0)
                                (t next)))
                        ;(format t "ratio:~A,radius:~A,diff:~A~%" ratio radius diff)
                        ))))))

(defun som (dataset &key (w 6) (h 6) (rate 0.2d0))
  (let* ((num-of-features (length (aref dataset 0)))
         (num-of-instances (length dataset))
         (nodes (rand-matrix w h num-of-features)))
    (dotimes (p 5000)
          (format t "~A~%" nodes)
    (loop for i from 0 below num-of-instances
          for data = (aref dataset i) do
          ;(format t "~A/5000 in ~A~%" p i)
          (multiple-value-bind (x y) (find-nearest-index nodes data)
            (drop nodes x y data rate num-of-instances p))))
    nodes))

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

(defun print-array (arr)
  (loop for v across arr do
        (format t " ~f" v)))

(defun normalize (dataset)
  (let* ((num-of-instances (length dataset))
         (num-of-features (length (aref dataset 0)))
         (result (make-array num-of-instances)))
    (dotimes (i num-of-instances)
      (setf (aref result i)
            (make-array num-of-features)))
    (dotimes (i num-of-features)
      (let ((max (aref (aref dataset 0) i))
            (min (aref (aref dataset 0) i)))
        (loop for row across dataset
              for v = (aref row i) do
              (when (< max v)
                (setf max v))
              (when (< v min)
                (setf min v)))
        (dotimes (j num-of-instances)
          (setf (aref (aref result j) i)
                (/ (- (aref (aref dataset j) i) min) (- max min))))))
    result))

(defun main (path &key (w 6) (h 6) (rate 0.2d0))
  (let ((result (som (normalize (loadraw path #\,)) :w w :h h :rate rate)))
    (loop for i from 0 below w do
          (loop for j from 0 below h do
                (format t "~A ~A" (1+ i) (1+ j))
                (print-array (aref result j i))
                (terpri))
          (terpri))))
