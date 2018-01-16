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
  (implements (make-hash-table :test #'equal) :type hash-table)
  (implemented-types nil :type list))

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
            (let* ((method-dummy-types (third statement))
                   (method-result-type (fourth statement))
                   (trait-method (make-trait-method :name statement-name-string
                                                    :parameter-types method-dummy-types
                                                    :result-type method-result-type)))
              (setf (gethash statement-name-string (trait-methods trait))
                    trait-method
                    (gethash statement-name-string *trait-method-table*)
                    trait-name)
              (eval `(defmacro ,statement-name (&rest rest)
                       (let* ((self (let ((s (first rest))) (if (symbolp s) (symbol-value s) s)))
                              (trait-name (gethash ,statement-name-string *trait-method-table*))
                              (trait (gethash trait-name *trait-table*))
                              (trait-method (gethash ,statement-name-string (trait-methods trait)))
                              (implemented-types (trait-method-implemented-types trait-method))
                              (implements (trait-method-implements trait-method)))
                         (loop for it in implemented-types
                               if (typep self it)
                               return `(funcall ,(gethash (symbol-name it) implements) ,@rest)))))))
          (:parameter
            nil))))
    (setf (gethash trait-name *trait-table*)
          trait)
    `(format t "Define ~A trait~%" ,trait-name)))

(defmacro impl ((trait-name trait-method) target types arguments &body body)
  (declare (ignore types))
  (let* ((trait-name-string (symbol-name trait-name))
         (trait-method-string (symbol-name trait-method))
         (target-string (symbol-name target))
         (trait-method (gethash trait-method-string
                                (trait-methods
                                  (gethash trait-name-string *trait-table*))))
         (implements (trait-method-implements trait-method)))
    (push target (trait-method-implemented-types trait-method))
    (setf (gethash target-string implements)
          (eval `(lambda (self ,@arguments) ,@body)))
    `(format t "Define ~A method for ~A~%" ,trait-method-string ,target-string)))


(deftrait multiply
  (:method mul (:T) :T))

(impl (multiply mul) integer
  (:T integer) (n)
  (format nil "INTEGER ~A" (* self n)))

(impl (multiply mul) string
  (:T string) (n)
  (format nil "STRING \"~A ~A\"" self n))

(impl (multiply mul) float
  (:T float) (n)
  (format nil "FLOAT ~A" (* self n)))

