;;
;;  Particle Rendering with OpenGL
;;

(in-package :cl)
(defpackage pixel-sprite
  (:nicknames :ps)
  (:use :cl))
(in-package :pixel-sprite)
(require "asdf")
(require 'cffi)
(require 'alexandria)
(require 'trivial-main-thread)
(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glfw3)

(setf *random-state* (make-random-state t))

(defstruct texture
  (data nil)
  (width 0)
  (height 0)) 

(defstruct mouse
  (start-x 0)
  (start-y 0)
  (pushed nil)
  (weight 0.5))

(defstruct particle
  (life 0.0)
  (fade-speed 0.01)
  (x 0.0) (y 0.0) (z 0.0)
  (r 1.0) (g 1.0) (b 1.0) (a 1.0)
  (move-x 10) (move-y 10) (move-z 10)
  (xg 0) (yg -0.8) (zg 0)) 

(defparameter *keys-pressed* nil)
(defparameter *mouse* (make-mouse))
(defparameter *window-size* nil)

(defparameter *particles0* nil)
(defparameter *particles1* nil)
(defparameter *particles2* nil)
(defparameter *particles3* nil)

(defparameter *angle-x* 0)
(defparameter *angle-y* 0)

(defparameter *textures* '())
(defparameter *texnames* nil)
(defparameter *current-texture* 0)
 
;; aref for texture-objects [GLuint* textures]
(defmacro texaref (texture-objects idx)
  `(cffi:mem-aref ,texture-objects '%gl:uint ,idx)) 

;; Load raw RGBA byte array for texture
(defun load-rgba-texture (path width height)
  (let ((texture (cffi:foreign-alloc '%gl:ubyte :count (* width height 4)))
        (image (alexandria:read-file-into-byte-vector path)))
    (loop for i from 0 to (1- (length image)) do
      (setf (cffi:mem-aref texture '%gl:ubyte i) (aref image i)))
    (make-texture :data texture :width width :height height)))

;; Call just once
(defun bind-texture-objects (texlist)
  (let* ((tc (list-length texlist))
        (texnames (cffi:foreign-alloc '%gl:uint :count tc)))
    (%gl:gen-textures tc texnames)
    (loop for i from 0 to (1- tc) do
      (%gl:bind-texture :texture-2d (texaref texnames i))
      (%gl:pixel-store-i :unpack-alignment 4)
      (%gl:tex-parameter-i :texture-2d :generate-mipmap #x1)
      (%gl:tex-parameter-i :texture-2d :texture-mag-filter #x2601)
      (%gl:tex-parameter-i :texture-2d :texture-min-filter #x2703)
      (%gl:tex-parameter-i :texture-2d :texture-wrap-s #x2900)
      (%gl:tex-parameter-i :texture-2d :texture-wrap-t #x2900)
      (%gl:tex-env-i :texture-env :texture-env-mode #x2100)
      (%gl:tex-env-i :point-sprite :coord-replace #x1)  
      (%gl:tex-image-2d :texture-2d 0 #x1908 (texture-width (nth i texlist)) (texture-height (nth i texlist))
        0 #x1908 :unsigned-byte (texture-data (nth i texlist))))
    texnames))

;; Loading textures
(defun initialize ()
  (%gl:pixel-store-i :unpack-alignment 4)
  (push (load-rgba-texture "yellow.raw" 32 32) *textures*)
  (format t "Loaded texture 3~%")
  (push (load-rgba-texture "blue.raw" 32 32) *textures*)
  (format t "Loaded texture 2~%")
  (push (load-rgba-texture "green.raw" 32 32) *textures*)
  (format t "Loaded texture 1~%")
  (push (load-rgba-texture "red.raw" 32 32) *textures*)
  (format t "Loaded texture 0~%")
  (setf *texnames* (bind-texture-objects *textures*))
  (format t "Succeeded to bind textures~%")
  (%gl:alpha-func :greater 0.5))

;; Destroy textures
(defun destroy ()
  (%gl:delete-textures (list-length *textures*) *texnames*)
  (format t "Deleted textures~%"))

;; Draw 4 colors particles
(defun draw-particles ()
  (gl:enable :blend)
  (gl:enable :texture-2d)
  (gl:enable :point-sprite)
  (gl:point-size 32)
  ; Draw red particles
  (%gl:bind-texture :texture-2d (texaref *texnames* 0))
  (gl:begin :points)
  (loop for p across *particles0*
        do (gl:color 1.0 1.0 1.0 (particle-life p))
        do (gl:vertex (particle-x p) (particle-y p) (particle-z p)))
  (gl:end)
  ; Draw green particles
  (%gl:bind-texture :texture-2d (texaref *texnames* 1))
  (gl:begin :points)
  (loop for p across *particles1*
        do (gl:color 1.0 1.0 1.0 (particle-life p))
        do (gl:vertex (particle-x p) (particle-y p) (particle-z p)))
  (gl:end)
  ; Draw blue particles
  (%gl:bind-texture :texture-2d (texaref *texnames* 2))
  (gl:begin :points)
  (loop for p across *particles2*
        do (gl:color 1.0 1.0 1.0 (particle-life p))
        do (gl:vertex (particle-x p) (particle-y p) (particle-z p)))
  (gl:end)
  ; Draw yellow particles
  (%gl:bind-texture :texture-2d (texaref *texnames* 3))
  (gl:begin :points)
  (loop for p across *particles3*
        do (gl:color 1.0 1.0 1.0 (particle-life p))
        do (gl:vertex (particle-x p) (particle-y p) (particle-z p)))
  (gl:end)
  (gl:disable :point-sprite)
  (gl:disable :texture-2d)
  (gl:disable :blend)
  (gl:point-size 1))

;; Add particle to vector for initialization
(defun add-particle (ptcls)
  (let ((p (make-particle)))
    (vector-push-extend p ptcls))) 

;; Initialize the particle
(defun init-particle (p)
  (setf (particle-life p) 1.0
        (particle-x p) 0
        (particle-y p) 0
        (particle-z p) 0
        (particle-fade-speed p) (+ (/ (random 99) 1000) 0.001)
        (particle-move-x p) (* (- (random 50) 25.0) 10)
        (particle-move-y p) (* (- (random 50) 25.0) 10)
        (particle-move-z p) (* (- (random 50) 25.0) 10)))

;; Move the particle
(defun move-particle (p)
  (let ((slow-down 10.0))
    (incf (particle-x p) (/ (particle-move-x p) (* 1000 slow-down)))
    (incf (particle-y p) (/ (particle-move-y p) (* 1000 slow-down)))
    (incf (particle-z p) (/ (particle-move-z p) (* 1000 slow-down)))
    (incf (particle-move-x p) (particle-xg p))
    (incf (particle-move-y p) (particle-yg p))
    (incf (particle-move-z p) (particle-zg p))))

;; Update particles
(defun update (ptcls id)
  (loop for i from 0 below (length ptcls) do
    (let ((p (aref ptcls i)))
      (decf (particle-life p) (particle-fade-speed p))
      (move-particle p) 
      (when (and (= *current-texture* id) (<= (particle-life p) 0))
        (init-particle p)))))

;; ESC -> quit
;;  Z  -> change particle color to Red
;;  X  -> Change particle color to Green
;;  A  -> Change particle color to Blue
;;  S  -> Change particle color to Yellow
(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close))
  (if (eq action :press)
    (progn
      (when (eq key :z) (setf *current-texture* 0))
      (when (eq key :x) (setf *current-texture* 1))
      (when (eq key :a) (setf *current-texture* 2))
      (when (eq key :s) (setf *current-texture* 3))
      (pushnew key *keys-pressed*)))
  (if (eq action :release)
    (alexandria:deletef *keys-pressed* key)))

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys))
  (if (eq action :press)
    (progn (if (eq button :left)
             (let ((pos (glfw:get-cursor-position window)))
               (setf (mouse-start-x *mouse*) (first pos))
               (setf (mouse-start-y *mouse*) (second pos)))
           (pushnew button (mouse-pushed *mouse*))))
    (alexandria:deletef (mouse-pushed *mouse*) button)))

;; It's called when on mouse move event
;; Change the camera angle by mouse position
(glfw:def-cursor-pos-callback cursor-callback (window x y)
  (declare (ignore window))
  (if (not (find :left (mouse-pushed *mouse*)))
    (let ((xdir (- x (mouse-start-x *mouse*)))
          (ydir (- y (mouse-start-y *mouse*))))
      (incf *angle-x* (* ydir (mouse-weight *mouse*)))
      (incf *angle-y* (* xdir (mouse-weight *mouse*)))
      (setf (mouse-start-x *mouse*) x)
      (setf (mouse-start-y *mouse*) y))))

;; It's called when on resize event
;; Scale the viewport by the window size
(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h)) 

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 30.0 (/ 600 400) 1.0 100.0)
  (gl:matrix-mode :modelview))

;; Draw X-axis, Y-axis and Z-axis
(defun draw-xyz ()
  (gl:begin :lines)
  
  (gl:color 0 1 0)
  (gl:vertex -100 0)
  (gl:vertex 100 0)
  
  (gl:color 1 0 0)
  (gl:vertex 0 0)
  (gl:vertex 0 100)
  
  (gl:color 0 0 1)
  (gl:vertex 0 0 -100)
  (gl:vertex 0 0 100)
  
  (gl:end))

(defun main ()
  (trivial-main-thread:with-body-in-main-thread ()
    (glfw:with-init-window (:title "Particle Render" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
      (initialize)
      (glfw:set-key-callback 'key-callback)
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-cursor-position-callback 'cursor-callback)
      (glfw:set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (gl:blend-func :src-alpha :one)
      (gl:disable :depth-test)
      (set-viewport 600 400)
      (setf *particles0* (make-array 4096 :adjustable t :fill-pointer 0))
      (setf *particles1* (make-array 4096 :adjustable t :fill-pointer 0))
      (setf *particles2* (make-array 4096 :adjustable t :fill-pointer 0))
      (setf *particles3* (make-array 4096 :adjustable t :fill-pointer 0))
      (loop repeat 4096 do (add-particle *particles0*))
      (loop repeat 4096 do (add-particle *particles1*))
      (loop repeat 4096 do (add-particle *particles2*))
      (loop repeat 4096 do (add-particle *particles3*))
      (format t "Particles generated~%")
      (loop until (glfw:window-should-close-p)
         do (gl:clear :color-buffer-bit :depth-buffer-bit)
            (gl:load-identity)
            (glu:look-at -6.0 7.0 8.0 0.0 0.0 0.0 0.0 1.0 0.0)
            (%gl:rotate-d *angle-x* 1 0 0)
            (%gl:rotate-d *angle-y* 0 1 0)
            (draw-xyz)
            (draw-particles)
            (update *particles0* 0)
            (update *particles1* 1)
            (update *particles2* 2)
            (update *particles3* 3)
            (gl:color 0 0 0)
            (glfw:swap-buffers)
            (glfw:poll-events))
      (destroy))))

(main)
