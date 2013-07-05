;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; itoa implementation.
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun itoa (number &optional (base 10))
  "Returns a string of the given number in the given base."
  (if (zerop number)
      "0"
      (loop
         with str = ()
         for j = number then (truncate (/ j base))
         for i = (mod j base)
         do
           (push (code-char (if (< i 10)
                                (+ i (char-code #\0))
                                (+ (- i 10) (char-code #\A)))) str)
         until (zerop j)
         finally (return (coerce (cdr str) 'string)))))

(defun itoa-recursive (number &optional (base 10))
  (if (zerop number)
      "0"
      (let ((j (truncate (/ number base)))
            (i (mod number base)))
        (concatenate 'string
                     (itoa-recursive j base)
                     (coerce (list (code-char (if (< i 10)
                                                  (+ i (char-code #\0))
                                                  (+ (- i 10) (char-code #\A)))))
                             'string)))))
