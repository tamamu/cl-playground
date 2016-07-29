;;;; Finite Group

(defclass finite-group nil
  ((set :initform nil :accessor group-set :initarg :set)
   (operator :initform #'*
             :accessor group-operator
             :initarg :op)
   (identity :initform nil
             :accessor group-identity
             :initarg :ie)))

(defmethod closed ((g finite-group))
  "Check that group is closed."
  (let ((gset (group-set g))
        (gop (group-operator g))
        (miss 0))
    (dolist (e1 gset)
      (dolist (e2 gset)
        (if (not (find (funcall gop e1 e2) gset :test #'equal))
          (incf miss))))
    (= miss 0)))

(defmethod satisfies-associative-law ((g finite-group))
  "Check that group is satisfy associative law."
  (let ((gset (group-set g))
        (gop (group-operator g))
        (miss 0))
    (dolist (e1 gset)
      (dolist (e2 gset)
        (dolist (e3 gset)
          (let ((a (funcall gop e1 (funcall gop e2 e3)))
                (b (funcall gop (funcall gop e1 e2) e3)))
          (if (not (and (equal a b)
                        (find a gset :test #'equal)))
            (incf miss))))))
    (= miss 0)))

(defun commutative (op e1 e2)
  "Check the operator is commutative on 2 values."
  (equal (funcall op e1 e2)
         (funcall op e2 e1)))

(defmethod satisfies-commutative-property ((g finite-group))
  "Check that group is satisfy commutative property."
  (let ((gset (group-set g))
        (gop (group-operator g))
        (miss 0))
    (dolist (e1 gset)
      (dolist (e2 gset)
        (if (not (commutative gop e1 e2))
          (incf miss))))
    (= miss 0)))

(defmethod exists-identity-element ((g finite-group))
  "Check that group has identity element."
  (let ((gset (group-set g))
        (gop (group-operator g))
        (ie nil))
    (dolist (e1 gset)
      (let ((res (list)))
        (dolist (e2 gset)
          (if (commutative gop e1 e2)
            (push (if (equal (funcall gop e1 e2) e2)
                      e2
                      :f)
                  res)))
        (if (not (find :f res))
          (setf ie e1))))
    (setf (group-identity g) ie)))

(defmethod exists-inverse-element ((g finite-group))
  "Check that group has inverse element."
  (if (null (group-identity g))
    (princ "This set has no identity element."))
  (let ((gset (group-set g))
        (gop (group-operator g))
        (gie (group-identity g))
        (miss 0))
    (dolist (e1 gset)
      (let ((cntr 0))
        (dolist (e2 gset)
          (if (equal (funcall gop e1 e2) gie)
            (incf cntr 1)))
        (if (equal cntr 0)
          (incf miss))
        (setf cntr 0)))
    (= miss 0)))

(defun range (m &optional (n 1))
  "Make range from 1 to M."
  (if (> n m)
    nil
    (cons n (range m (1+ n)))))

(defun connect-amida (a1 a2)
  "Connect 2 amida-kuji lengthwise."
  (if (not (eq (length a1) (length a2)))
    (princ "Can't connect!"))
  (let* ((len (length a1))
         (res (range len)))
  (dotimes (x len)
    (let* ((p1 (position (1+ x) a1))
           (p2 (position (1+ p1) a2)))
      (setf (elt res p2) (1+ x))))
    res))

(defvar *amida*
  (make-instance 'finite-group :set '((1 2 3) (2 3 1) (3 1 2)
                                      (1 3 2) (3 2 1) (2 1 3))
                               :op #'connect-amida))

(format t "Amida-kuji is a group.~%~%")
(format t "Proof:~%")
(format t "G = ~A~%" (group-set *amida*))
(format t "(G0) : Closure -> ~A~%"
  (closed *amida*))
(format t "(G1) : Associativity -> ~A~%"
  (satisfies-associative-law *amida*))
(format t "(G2) : Identity -> ~A~%"
  (exists-identity-element *amida*))
(format t "(G3) : Inverse -> ~A~%"
  (exists-inverse-element *amida*))
(format t "(C)  : Commutativity -> ~A~%"
  (satisfies-commutative-property *amida*))
