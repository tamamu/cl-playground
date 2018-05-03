(require 'asdf)
(require 'cl-opengl)
(require 'cl-glut)

(in-package :cl-user)
(defpackage bezier
  (:use :cl))
(in-package :bezier)

(defparameter *width* 800)
(defparameter *height* 600)

(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)

(defun bezier2 (p0 p1 p2 p3 &optional (div 16))
  (loop for i from 0 below div
        for u = (* i (float (/ 1 (1- div))))
        for nu = (- 1 u)
        for mp0 = (expt nu 3)
        for mp1 = (* 3 u (expt nu 2))
        for mp2 = (* 3 (expt u 2) nu)
        for mp3 = (expt u 3)
        do (gl:vertex (+ (* (aref p0 0) mp0)
                         (* (aref p1 0) mp1)
                         (* (aref p2 0) mp2)
                         (* (aref p3 0) mp3))
                      (+ (* (aref p0 1) mp0)
                         (* (aref p1 1) mp1)
                         (* (aref p2 1) mp2)
                         (* (aref p3 1) mp3)))))

(defun draw ()
  (gl:color 1 1 1 1)
  (gl:with-primitive :line-strip
    (bezier2 #(0.0 0.0) #(120.0 30.0) #(600.0 210.0) #(400.0 300.0) 32)
  ))

(defclass main-window (glut:window)
  ()
  (:documentation "The main window class")
  (:default-initargs :title "Bezier Drawing"
                     :width *width*
                     :height *height*
                     :mode '(:stencil :multisample)
                     :tick-interval (round (/ 1000 60))))

(defmethod glut:display-window :before ((w main-window))
  (declare (ignorable w)))

(defmethod glut:reshape ((w main-window) width height)
  (setf *width* width
        *height* height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glut:post-redisplay))

(defmethod glut:mouse ((w main-window) button state x y)
  (declare (ignore w state))
  (setf *mouse-x* x
        *mouse-y* y)
  (case button
    (:left-button
      (format t "Left click at X:~A Y:~A~%" x y))))

(defmethod glut:tick ((w main-window))
  (glut:post-redisplay))

(defmethod glut:display ((w main-window))
  (declare (ignorable w))
  (gl:viewport 0 0 *width* *height*)
  (gl:clear-color 0.21 0.35 0.68 1)
  (gl:clear :color-buffer-bit)
  (gl:color 0.5 0.0 0.0 1.0)
  (draw)
  (gl:flush))

(glut:display-window (make-instance 'main-window))
