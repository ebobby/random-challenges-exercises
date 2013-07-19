;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a file of one millon unique random integers out of ten millions.
;;
;; Exercise 1.4 from Programming Pearls
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +set-max+ 100000000)
(defconstant +number-of-elements+ 10000000)
(defparameter *already-seen* (make-hash-table :size +number-of-elements+))

(defun generate-number-list ()
  (loop
     for i = (random +set-max+)
     unless (gethash i *already-seen*)
     do (setf (gethash i *already-seen*) t)
     while (< (hash-table-count *already-seen*) +number-of-elements+)))

(defun write-number-list ()
  (with-open-file (s #P"numbers.txt" :direction :output :if-exists :overwrite)
    (loop
       for k being the hash-keys in *already-seen*
       do
         (prin1 k s)
         (fresh-line s))))

(princ "Generating number list...")
(fresh-line)
(generate-number-list)

(princ "Writing file...")
(fresh-line)
(write-number-list)

(princ "Done.")
(fresh-line)
