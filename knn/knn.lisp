;;; k-nearest neighbor

(defparameter training
  #(
    (#(1 4) . 0)
    (#(2 5) . 1)
    (#(3 2) . 0)
    (#(2 1) . 0)
    (#(3 3) . 1)
    (#(5 7) . 1)
    (#(4 9) . 0)
    (#(0 2) . 0)
    (#(6 1) . 1)
    (#(5 5) . 1)))

(defun calc-distance (a b)
  (sqrt (loop for alpha across a
              for beta across b
              sum (expt (- alpha beta) 2))))

(defun get-all-distances (training x)
  (map 'vector
       (lambda (data)
        (cons (calc-distance (car data) x) (cdr data)))
       training))

(defun k-nearest-neighbor (k input)
  (let ((distances (get-all-distances training input))
        (class-0 0)
        (class-1 0))
    (sort distances
          (lambda (a b) (< (car a) (car b))))
    (loop for i from 0 below k
          for data = (aref distances i)
          if (= 0 (cdr data))
          do (incf class-0)
          else
          do (incf class-1)
          end)
    (if (< class-1 class-0)
        0
        1)))
