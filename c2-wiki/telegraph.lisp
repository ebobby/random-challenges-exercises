;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Telegraph problem
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-line-writer (max-line-length)
  "Returns a function that prints words out separated by a space and outputs a new line when the max line
   length is reached."
  (let ((written 0))
    (lambda (word)
      (let ((len (length word)))
        (when (> (+ written (1+ len)) max-line-length)
          (terpri)
          (setf written 0))
        (princ word)
        (princ " ")
        (incf written (1+ len))))))
