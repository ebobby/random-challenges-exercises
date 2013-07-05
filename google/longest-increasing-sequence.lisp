;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; longest increasing sequence problem.
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ( 2 4 3 5  1 7 6 9 8 )

(defun lis-brute-force (sequence)
  "Finds the longest increasing sequence in O(N^3) time."
  (loop
     with best = '()
     for i from 0 to (1- (length sequence))
     do
       (loop
          for j from (1+ i) to (1- (length sequence))
          when (> (elt sequence j) (elt sequence i))
          do
            (let ((run (loop
                          with previous = (elt sequence i)
                          with how-many = 1
                          with seq = (list (elt sequence i))
                          for m from j to (1- (length sequence))
                          when (> (elt sequence m) previous)
                          do
                            (setf previous (elt sequence m))
                            (push (elt sequence m) seq)
                            (incf how-many)
                          finally (return (reverse seq)))))
              (when (>= (length run)
                        (length best))
                (setf best run))))
     finally (return best)))
