
(defmacro nand (&rest rest)
  `(not (and ,@rest)))

(defun my-not (a)
  (nand a a))

(defmacro my-or (&rest rest)
  `(nand ,@(mapcar #'my-not rest)))


