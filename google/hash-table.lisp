;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash table implementation
;;
;; Practicing for Google's interveiews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +dictionary-table-size+ 10000 "Fixed size for our dictionaries internal tables.")

(defstruct dictionary
  (size +dictionary-table-size+ :type fixnum)
  (table (make-array +dictionary-table-size+ :initial-element nil))
  (hash (lambda (key) (mod (sxhash key) +dictionary-table-size+))))

(defun dictionary-insert (dictionary key value)
  "Add elements to our dictionary."
  (when (and (dictionary-p dictionary) (not (null key)))
    (let* ((hash (funcall (dictionary-hash dictionary) key))
           (table (dictionary-table dictionary))
           (elements (aref table hash)))
      (when (null elements)
        (setf elements (setf (aref table hash) (make-array 1 :fill-pointer 0 :adjustable t))))
      (let ((current (find key elements :key #'first :test #'equal)))
        (if current
            (setf (second current) value)
            (vector-push-extend (list key value) (aref table hash)))))))

(defun dictionary-get (dictionary key)
  "Finds the value in the dictionary with the given key."
  (when (and (dictionary-p dictionary) (not (null key)))
    (let* ((hash (funcall (dictionary-hash dictionary) key))
           (table (dictionary-table dictionary))
           (elements (aref table hash)))
      (when elements
        (second (find key elements :key #'first :test #'equal))))))

(defun dictionary-delete (dictionary key)
  (dictionary-insert dictionary key nil))
