(in-package :cl-user)
(defpackage nlp/search
  (:use :cl))
(in-package :nlp/search)

(defun make-queue ()
  (cons nil nil))

(defun enqueue (queue item)
  (let ((head (car queue))
        (tail (cdr queue)))
    (if (null head)
        (rplaca queue item)
        (rplacd queue (append tail (list item))))))

(defun dequeue (queue)
  (prog1
    (car queue)
    (rplaca queue (second queue))
    (rplacd queue (cddr queue))))

(defun queue-emptyp (queue)
  (null (car queue)))

(let ((q (make-queue)))
  (enqueue q 'hello)
  (enqueue q 'world)
  (assert (eq (dequeue q) 'hello))
  (assert (eq (dequeue q) 'world))
  (assert (queue-emptyp q)))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun unwrap (x)
  (if (and (not (null x))
           (listp x))
      (unwrap (car x))
      x))

(defclass persist-array ()
  ((entity :reader entity
           :initarg :entity
           :initform (vector)
           :type array)))

(defun make-persist-array (length &key (initial-element 0))
  (make-instance 'persist-array
                 :entity (make-array length
                                     :initial-element initial-element)))

(defmethod insert ((pa persist-array) index item)
  (let ((copied (copy-seq (entity pa))))
    (setf (aref copied index) item)
    (make-instance 'persist-array :entity copied)))

(defmethod lookup ((pa persist-array) index)
  (let ((e (entity pa)))
    (if (<= (length e) index)
        nil
        (aref e index))))

(defmethod size ((pa persist-array))
  (length (entity pa)))

(defun exists (entry dict)
  (member (if (characterp entry)
              (string entry)
              entry)
          dict))

(defun starts-with (all part)
  (if (< (length all) (length part))
      nil
      (let ((cut (subseq all 0 (length part))))
        (string= cut part))))

(defun retrieve (str dict)
  (loop for entry in dict
        when (starts-with str entry)
        collect entry))

(defun make-table (str dict table &optional (index 0))
  (if (and (< index (1- (length str)))
           (not (null (lookup table index))))
      (let ((next (1+ index)))
        (make-table str dict
                    (reduce (lambda (tbl entry)
                              (let* ((j (+ index (length entry)))
                                     (tbl2 (insert tbl j (append (lookup tbl j) (list (cons index entry))))))
                                (insert tbl2 j (append (lookup tbl2 j) (list (cons j nil))))))
                            (retrieve (subseq str index) dict)
                            :initial-value table)
                    next))
        table))

(defun extract (table &optional index acc)
  (if (null index)
      (let ((end (unwrap (last (lookup table (1- (size table)))))))
        (extract table end))
      (if (zerop index)
          acc
          (let* ((tuple (first (lookup table index)))
                 (next (car tuple))
                 (morph (cdr tuple)))
            (extract table next (cons morph acc))))))

(defun analyze (str dict)
  (let ((table
          (make-table str
                      dict
                      (insert (make-persist-array
                                (1+ (length str))
                                :initial-element (list))
                              0
                              (list '(0 nil))))))
    (loop for i from 0 below (size table)
          do (format t "~A:~A~%" i (lookup table i)))
    (format t "--------------------------------~%RESULT:~A~%~%" (extract table))))

(analyze "全国都道府県議会議長会"
         (list "全国都" "全国" "全" "国" "国都" "都道府県" "都" "道" "府" "県" "県議" "会" "議会" "会議" "議長"))

(analyze "すもももももももものうち"
         (list "すもも" "す" "もも" "も" "もの" "の" "のう" "うち" "ち"))
