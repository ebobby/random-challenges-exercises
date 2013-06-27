;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quick sort implementation
;;
;; Practicing for Google's interveiews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quick-sort (arr &optional (left 0) (right (1- (array-total-size arr))))
  (labels ((partition (arr left right)
             (let* ((pivot (+ left (ash (- right left) -1)))
                    (pivot-value (aref arr pivot))
                    (store-index left))
               (rotatef (aref arr pivot) (aref arr right))
               (loop
                  for i from left to (1- right)
                  when (< (aref arr i) pivot-value)
                  do
                    (unless (= i store-index) (rotatef (aref arr i) (aref arr store-index)))
                    (incf store-index))
               (rotatef (aref arr store-index) (aref arr right))
               store-index)))
    (when (< left right)
      (let ((index (partition arr left right)))
        (quick-sort arr left (1- index))
        (quick-sort arr (1+ index) right)
        arr))))
