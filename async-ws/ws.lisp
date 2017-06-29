;;; sbcl --load ws.lisp
;;; then open index.html on a Web browser

(ql:quickload '(:websocket-driver :clack :bordeaux-threads :jsown))

(in-package :cl)
(defpackage async-alert-server
  (:use :cl :websocket-driver :bordeaux-threads))
(in-package :async-alert-server)

(defvar *taskbase*
  (make-hash-table :test #'equal))

(defstruct task-entity
  (body nil)
  (output nil)
  (should-kill-p nil)
  (exit-p nil)
  (thread nil))

(defun regist-task (id task)
  (setf (gethash id *taskbase*) task))

(defun exists-task (id)
  (gethash id *taskbase* nil))

(defun attach-task-thread (id)
  (setf (task-entity-thread (gethash id *taskbase*))
        (make-thread (task-entity-body (gethash id *taskbase*)))))

(defun get-task-thread (id)
  (task-entity-thread (gethash id *taskbase*)))

(defun set-task-kill (id)
  (setf (task-entity-should-kill-p (gethash id *taskbase*)) t))

(defun set-task-exit (id)
  (setf (task-entity-exit-p (gethash id *taskbase*)) t))

(defun get-task-exit (id)
  (task-entity-exit-p (gethash id *taskbase*)))

(defun should-task-kill-p (id)
  (task-entity-should-kill-p (gethash id *taskbase*)))

(defun set-task-output (id value)
  (setf (task-entity-output (gethash id *taskbase*)) value))

(defun delete-task (id)
  (remhash id *taskbase*))

(defun get-task-output (id)
  (task-entity-output (gethash id *taskbase*)))

(defun macroexpand-all (form)
  (let ((form (macroexpand form)))
    (cons (car form) (mapcar #'macroexpand (cdr form)))))

(defmacro checkpoint (value &optional (kill nil))
  "Alert client that has the id to progress"
  `(if (should-task-kill-p $<id>)
       (progn
         (unless (null ,kill)
           (set-task-output $<id> ,kill))
         (return))
       (set-task-output $<id> ,value)))

(defmacro runtask (initial &body body)
  "Run task which takes an id asynchronous"
  (make-task-entity
    :output initial
    :body
    `(let (($<id> id-form)
           ($<result> nil))
       (lambda ()
         (unwind-protect
           (setf $<result> (progn ,@body))
           (unless (null $<result>)
             (set-task-output $<id> $<result>)
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
                    ("value" . ,(get-task-output id)))))
  (when (or (get-task-exit id)
            (should-task-kill-p id))
      (progn
        (delete-task id)
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

(defun attach-task (id task)
  (->$ (task-entity-body task)
       (attach-checkpoint)
       (macroexpand-all)
       (attach-id id)
       (eval)
       (setf (task-entity-body task))))

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
                        (unless (null (exists-task id))
                          (set-task-kill id)
                          (join-thread (get-task-thread id)))
                        (attach-task id lo)
                        (regist-task id lo)
                        (attach-task-thread id))
                        (send ws (jsown:to-json
                                   `(:obj ("message" . "init")
                                          ("id" . ,id))))))
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
