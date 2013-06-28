;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; merge sort implementation
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merge-sort (arr)
  (labels ((merge-helper (l1 l2)
             (macrolet
                 ((move-value (dst dst-index src src-index)
                    `(progn
                       (setf (aref ,dst ,dst-index) (aref ,src ,src-index))
                       (incf ,dst-index)
                       (incf ,src-index))))
               (loop
                  with l1-len = (array-total-size l1)
                  with l2-len = (array-total-size l2)
                  with result = (make-array (+ l1-len l2-len))
                  with i = 0
                  with j = 0
                  with m = 0
                  when (= (+ i j) (+ l1-len l2-len)) return result
                  do
                    (cond ((and (< i l1-len) (< j l2-len))
                           (if (< (aref l1 i) (aref l2 j))
                               (move-value result m l1 i)
                               (move-value result m l2 j)))
                          ((< i l1-len)
                           (move-value result m l1 i))
                          ((< j l2-len)
                           (move-value result m l2 j)))))))
    (let ((len (array-total-size arr)))
      (if (<= len 1)
          arr
          (let ((middle (ash len -1)))
            (merge-helper (merge-sort (subseq arr 0 middle))
                          (merge-sort (subseq arr middle len))))))))
