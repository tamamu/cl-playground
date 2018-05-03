

(defun make-matrix (row col)
  (make-array `(,row ,col)
              :initial-element 0.0
              :element-type 'float))

(defun m* (a b)
  (let ((arow (array-dimension a 0))
        (acol (array-dimension a 1))
        (brow (array-dimension b 0))
        (bcol (array-dimension b 1)))
    (assert (= acol brow))
    (let ((res (make-array (list arow bcol)
                           :initial-element 0.0
                           :element-type 'float)))
      (dotimes (row arow)
        (dotimes (col bcol)
          (dotimes (n acol)
            (incf (aref res row col)
                  (* (aref a row n)
                     (aref b n col))))))
      res)))

(defun m+ (a b)
  (let* ((arow (array-dimension a 0))
         (acol (array-dimension a 1))
         (res (make-array (list arow acol)
                          :initial-element 0.0
                          :element-type 'float)))
    (typecase b
      (integer
        (dotimes (row arow)
          (dotimes (col acol)
            (incf (aref res row col)
                  (+ b (aref a row col))))))
      (array
        (let ((dim (array-dimensions b)))
          (case (length dim)
            (1
             (dotimes (row arow)
               (dotimes (col acol)
                 (incf (aref res row col)
                       (+ (aref b col)
                          (aref a row col))))))
            (2
             (let ((brow (first dim))
                   (bcol (second dim)))
               (assert (and (= acol bcol)
                            (= arow brow)))
               (dotimes (row arow)
                 (dotimes (col acol)
                   (incf (aref res row col)
                         (+ (aref b row col)
                            (aref a row col)))))))))))
    res))

(defun transpose (a)
  (let* ((arow (array-dimension a 0))
         (acol (array-dimension a 1))
         (res (make-array (list acol arow)
                          :initial-element 0.0
                          :element-type 'float)))
    (dotimes (row arow)
      (dotimes (col acol)
        (incf (aref res row col)
              (aref a col row))))
    res))

(defun heaviside (a)
  (if (< 0 a) 1 0))


(defstruct layer
  (units 1 :type integer)
  (activation #'identity :type function)
  (weights nil :type array))

(defstruct neural-network
  (inputs 1 :type integer)
  (layers nil :type list))

;
; input 4
; hidden 2
;
; 1
; 2
; 3
; 4
;
; 1 3 5 7
; 2 4 6 8
;
;
; 10
; 20

(defun forward (nn input)
  (let* ((layers (neural-network-layers nn))
         (inputs (neural-network-inputs nn)))
    (assert (= (length input) inputs))
    (reduce
      (lambda (x layer)
        (let* ((units (layer-units layer))
               (activation (layer-activation layer))
               (weights (layer-weights layer))
               (next (m* weights x)))
          (dotimes (row units)
            (setf (aref next row 0)
                  (funcall activation (aref next row 0))))
          next))
      layers
      :initial-value input)))
