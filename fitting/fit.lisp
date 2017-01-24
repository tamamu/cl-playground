(require 'asdf)
(require 'prove)

(in-package :cl-user)
(defpackage fitting
  (:use :cl
        :prove))
(in-package :fitting)

(defun curve(x w)
  (loop for i from 0 below (length w)
        sum (* (aref w i) (expt x i))))

(defun fit (x y))

(print (curve 3 #(2 3)))