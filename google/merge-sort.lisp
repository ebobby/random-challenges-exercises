(defun merge-sort (collection)
  (labels ((merge-helper (l1 l2)
             (loop
                with result = (list)
                with l1-len = (length l1)
                with l2-len = (length l2)
                with i = 0
                with j = 0
                when (= (+ i j) (+ l1-len l2-len)) return (reverse result)
                do
                  (cond ((and (< i l1-len) (< j l2-len))
                         (if (< (nth i l1) (nth j l2))
                             (progn
                               (push (nth i l1) result)
                               (incf i))
                             (progn
                               (push (nth j l2) result)
                               (incf j))))
                        ((< i l1-len)
                         (push (nth i l1) result)
                         (incf i))
                        ((< j l2-len)
                         (push (nth j l2) result)
                         (incf j))))))
    (let ((len (length collection)))
      (if (<= len 1)
          collection
          (let ((middle (ash len -1)))
            (merge-helper (merge-sort (subseq collection 0 middle))
                          (merge-sort (subseq collection middle len))))))))
