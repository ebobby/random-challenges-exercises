;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kadane's algorithm
;;
;; Given a sequence of n real numbers A(1) ... A(n), determine a
;; contiguous subsequence A(i) ... A(j) for which the sum of elements in
;; the subsequence is maximized.
;; The  sequence has to have some negative numbers in it or you just have to pick the
;; entire array.
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mvcs-brute-force (sequence)
  (loop
     for i from 0 to (1- (length sequence))
     maximize
       (loop
          for j from (1+ i) to (1- (length sequence))
          maximize
            (loop for m from i to j sum (aref sequence m)))))

(defun mvcs-linear (sequence)
  (loop
     with max-ending-here = 0
     with max-so-far = 0
     for i in sequence
     do
       (setf max-ending-here (max 0 (+ max-ending-here i)))
       (setf max-so-far (max max-so-far max-ending-here))
     finally (return max-so-far)))
