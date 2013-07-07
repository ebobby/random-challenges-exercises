;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiplication
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multiply-peasant (multiplier multiplicand)
  (if (zerop multiplier)
      0
      (+ (if (plusp (logand multiplier 1))
             multiplicand
             0)
         (multiply (ash multiplier -1) (ash multiplicand 1)))))

(defun multiply-shift-add (multiplier multiplicand)
  (loop
     with accum = 0
     for i = 0 then (1+ i)
     while (plusp multiplier)
     do
       (when (plusp (logand multiplier 1))
         (incf accum (ash multiplicand i)))
       (setf multiplier (ash multiplier -1))
     finally (return accum)))
