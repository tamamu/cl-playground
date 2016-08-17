;;;; Cover

(defun combine (elm dst)
  (if (null dst)
    nil
    (mapcar (lambda (x) (cons elm x)) dst)))

(defun make-power-set (set)
  "Convert: set => power-set"
  (if (null set)
    (list nil)
    (append (combine (first set) (make-power-set (rest set)))
      (make-power-set (rest set)))))
