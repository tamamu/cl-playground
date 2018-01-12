(in-package :cl-user)
(defpackage cl.trait
  (:use :cl))
(in-package :cl.trait)

(defstruct trait
  (name "" :type string)
  (methods (make-hash-table) :type hash-table)
  (attributes (make-hash-table) :type hash-table))

(defstruct trait-method
  (name "" :type string)
  (parameter-types nil :type list)
  (result-type nil)
  (implements (make-hash-table :test #'equal) :type hash-table))

(defparameter *trait-method-table*
  (make-hash-table :test #'equal))

(defparameter *trait-table*
  (make-hash-table :test #'equal))

(defmacro deftrait (name &body rest)
  (let* ((trait-name (symbol-name name))
         (trait (make-trait :name trait-name)))
    (dolist (statement rest)
      (let ((statement-type (first statement))
            (statement-name (symbol-name (second statement))))
        (case statement-type
          (:method
            (let ((method-dummy-types (third statement))
                  (method-result-type (fourth statement)))
              (setf (gethash statement-name (trait-methods trait))
                    (make-trait-method :name statement-name
                                       :parameter-types method-dummy-types
                                       :result-type method-result-type)
                    (gethash statement-name *trait-method-table*)
                    trait-name)))
          (:parameter
            nil))))
    (setf (gethash trait-name *trait-table*)
          trait)
    `(format t "Define ~A trait~%" ,trait-name)))

(defmacro impl ((trait-name trait-method) target types arguments &body body)
  ())

(deftrait multiply
  (:method * (:T) :T))

(impl (multiply *) integer
  (:T integer) (n)
  (* self n))

(print *trait-method-table*)
(print *trait-table*)
