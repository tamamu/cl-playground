(defmacro if-let (variable then-form else-form)
  `(let ((@ ,variable))
     (if @ ,then-form ,else-form)))

(if-let (or nil "foo")
  (format t "True ~A~%" @)
  (format t "False ~A~%" @))

(if-let (and 1 2 nil)
  @
  (format t "False ~A~%" @))
