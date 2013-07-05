;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Odd word problem
;;
;; http://c2.com/cgi/wiki?OddWordProblem
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-printer ()
  "Returns a function that takes characters and buffers them until a space is found then everything is printed.
   Odd words are printed reversed. (First word is 0 so not odd)"
  (let ((words 0)
        (chars ()))
    (lambda (ch)
      (if (equal ch #\space)
          (progn
            (princ (coerce (if (oddp words)
                               chars
                               (reverse chars)) 'string))
            (princ " ")
            (incf words)
            (setf chars '()))
          (push ch chars)))))
