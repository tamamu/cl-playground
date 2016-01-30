(let ((*print-case* :capitalize))
  (format t "~{~A~^,~}~%"
          (loop for i from 1 to 40
                collect i
                if (or (zerop (mod i 3))
                       (find #\3 (princ-to-string i))) collect 'aho
                if (zerop (mod i 5)) collect 'naru
                if (zerop (mod i 8)) collect 'doko)))
