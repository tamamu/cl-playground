;;; sbcl --load ws.lisp
;;; then open index.html on a Web browser

(ql:quickload '(:websocket-driver :clack :bordeaux-threads :jsown))

(in-package :cl)
(defpackage async-alert-server
  (:use :cl :websocket-driver :bordeaux-threads))
(in-package :async-alert-server)

(defstruct task-entity
  (initial-value nil)
  (body nil)
  (output nil)
  (should-kill-p nil)
  (exit-p nil)
  (thread nil))

(defstruct task-memory
  (data nil)
  (should-kill-p nil)
  (exit-p nil)
  (thread nil))

(defvar *share*
  (make-hash-table :test #'equal))

(defun make-task (id initial-value)
  (setf (gethash id *share*)
        (make-task-memory :data initial-value)))

(defun exists-task (id)
  (gethash id *share* nil))

(defun task-attach-thread (id fun)
  (setf (task-memory-thread (gethash id *share*))
                            (make-thread fun)))

(defun get-task-thread (id)
  (task-memory-thread (gethash id *share*)))

(defun set-task-kill (id)
  (setf (task-memory-should-kill-p (gethash id *share*)) t))

(defun set-task-exit (id)
  (setf (task-memory-exit-p (gethash id *share*)) t))

(defun get-task-exit (id)
  (task-memory-exit-p (gethash id *share*)))

(defun should-task-kill (id)
  (task-memory-should-kill-p (gethash id *share*)))

(defun set-share-hash (id value)
  (setf (task-memory-data (gethash id *share*)) value))

(defun delete-share-hash (id)
  (remhash id *share*))

(defun get-share-hash (id)
  (task-memory-data (gethash id *share*)))

(defun macroexpand-all (form)
  (let ((form (macroexpand form)))
    (cons (car form) (mapcar #'macroexpand (cdr form)))))

(defmacro checkpoint (value &optional (kill nil))
  "Alert client that has the id to progress"
  `(if (should-task-kill $<id>)
       (progn
         (unless (null ,kill)
           (set-share-hash $<id> ,kill))
         (return))
       (set-share-hash $<id> ,value)))

(defmacro runtask (initial &body body)
  "Run task which takes an id asynchronous"
  (make-task-entity
    :initial-value initial
    :body
    `(let (($<id> id-form)
           ($<result> nil))
       (lambda ()
         (unwind-protect
           (setf $<result> (progn ,@body))
           (unless (null $<result>)
             (set-share-hash $<id> $<result>)
             (set-task-exit $<id>)))))))

(defun replace-all (src dest form)
  (mapcar (lambda (obj)
            (if (listp obj)
                (replace-all src dest obj)
                (if (and (symbolp obj)
                         (equalp (symbol-name obj)
                                 (symbol-name src)))
                    dest
                    obj)))
          form))

(defun attach-runtask (form)
  (replace-all 'runtask 'runtask form))

(defun attach-checkpoint (form)
  (replace-all 'checkpoint 'checkpoint form))

(defun attach-id (id form)
  (replace-all 'id-form id form))

(defun sender (ws id)
  "Alert to client that has the id"
  (send ws (jsown:to-json
             `(:obj ("message" . "update")
                    ("value" . ,(get-share-hash id)))))
  (when (or (get-task-exit id)
            (should-task-kill id))
      (progn
        (delete-share-hash id)
        (send ws (jsown:to-json
                   `(:obj ("message" . "exit")))))))

(defmacro ->$ (value &body forms)
  (reduce (lambda (acc form)
            (append form (list acc)))
          forms :initial-value value))

(defun startswith (a b)
  (let ((alen (length a))
        (blen (length b)))
    (if (>= alen blen)
      (string= (subseq a 0 blen) b)
      nil)))

(defvar *server*
  (lambda (env)
    (let ((ws (make-server env))
          (addr (getf env :remote-addr))
          (port (format nil "~A" (getf env :remote-port))))
      (on :message ws
          (lambda (message)
            (let ((json (jsown:parse message)))
              (if (string= (jsown:val json "message") "init")
                  (let* ((id (concatenate 'string addr
                                              ":" port
                                              ":" (jsown:val json "token")
                                              ":" (jsown:val json "optional")))
                         (lo (->$ (jsown:val json "value")
                                  (read-from-string)
                                  (attach-runtask)
                                  (eval))))
                    (when (task-entity-p lo)
                      (progn
                        (->$ (task-entity-body lo)
                             (attach-checkpoint)
                             (macroexpand-all)
                             (attach-id id)
                             (eval)
                             (setf (task-entity-body lo)))
                        (when (null (exists-task id))
                          (progn
                            (make-task id (task-entity-initial-value lo))
                            (task-attach-thread id (task-entity-body lo))))
                        (send ws (jsown:to-json
                                   `(:obj ("message" . "init")
                                          ("id" . ,id)))))))
                  (if (string= (jsown:val json "message") "kill")
                      (let ((id (jsown:val json "id")))
                        (set-task-kill id)
                        (join-thread (get-task-thread id))
                        (sender ws id))
                      (let* ((id (jsown:val json "id")))
                        (sender ws id)))))))
      (lambda (responder)
        (declare (ignorable responder))
        (start-connection ws)))))

(clack:clackup *server* :server :woo :port 5000)
