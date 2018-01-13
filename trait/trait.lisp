(in-package :cl-user)
(defpackage cl.trait
  (:use :cl))
(in-package :cl.trait)

(defstruct trait
  (name "" :type string)
  (methods (make-hash-table :test #'equal) :type hash-table)
  (attributes (make-hash-table :test #'equal) :type hash-table))

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
      (let* ((statement-type (first statement))
             (statement-name (second statement))
             (statement-name-string (symbol-name statement-name)))
        (case statement-type
          (:method
            (let ((method-dummy-types (third statement))
                  (method-result-type (fourth statement)))
              (setf (gethash statement-name-string (trait-methods trait))
                    (make-trait-method :name statement-name-string
                                       :parameter-types method-dummy-types
                                       :result-type method-result-type)
                    (gethash statement-name-string *trait-method-table*)
                    trait-name)
              (eval `(defmacro ,statement-name (&rest rest)
                       (let* ((self (first rest))
                              (self-type (symbol-name (car (type-of self))))
                              (trait-name (gethash ,statement-name-string *trait-method-table*))
                              (trait (gethash trait-name *trait-table*))
                              (implement (gethash self-type
                                                  (trait-method-implements
                                                    (gethash ,statement-name-string (trait-methods trait))))))
                         `(funcall ,implement ,@rest))))))
          (:parameter
            nil))))
    (setf (gethash trait-name *trait-table*)
          trait)
    `(format t "Define ~A trait~%" ,trait-name)))

(defmacro impl ((trait-name trait-method) target types arguments &body body)
  (let* ((trait-name-string (symbol-name trait-name))
         (trait-method-string (symbol-name trait-method))
         (target-string (symbol-name target))
         (implements (trait-method-implements
                       (gethash trait-method-string
                                (trait-methods
                                  (gethash trait-name-string *trait-table*))))))
    (setf (gethash target-string implements)
          (eval `(lambda (self ,@arguments) ,@body)))
    `(format t "Define ~A method for ~A~%" ,trait-method-string ,target-string)))

;(defun * (self &rest rest)
;  (let* ((self-type (symbol-name (type-of self)))
;         (trait-name (gethash "*" *trait-method-table*))
;         (trait (gethash trait-name *trait-table*))
;         (implement (gethash self-type
;                             (trait-method-implements
;                               (gethash "*" (trait-methods trait))))))
;    `(funcall ,implement self ,@rest)))

(deftrait multiply
  (:method mul (:T) :T))

(impl (multiply mul) integer
  (:T integer) (n)
  (* self n))

(impl (multiply mul) string
  (:T string) (n)
  (format nil "~A*~A" self n))

(print *trait-method-table*)
(print *trait-table*)
