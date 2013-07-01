;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trie implementation
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct trie
  (root (make-hash-table :test #'equal :size 60)))

(defun trie-insert (trie word)
  "Inserts a word into the given trie."
  (when (trie-p trie)
    (let ((root (trie-root trie)))
      (if (zerop (length word))
          (setf (gethash :end root) t)
          (let* ((ch (elt word 0))
                 (next (gethash ch root)))
            (unless next
              (setf (gethash ch root) (setf next (make-trie))))
            (trie-insert next (subseq word 1)))))))

(defun trie-find-trie (trie prefix)
  "Find the trie thati is down the level that corresponds to this prefix if any."
  (when (trie-p trie)
    (if (zerop (length prefix))
        trie
        (let* ((ch (elt prefix 0))
               (next (gethash ch (trie-root trie))))
          (when next
            (trie-find-trie next (subseq prefix 1)))))))

(defun trie-get-all-words (trie)
  "Get a list of all the words contained in the trie."
  (when (trie-p trie)
    (let ((result '()))
      (maphash (lambda (key value)
                 (if (eq key :end)
                     (push "" result)
                     (mapcar (lambda (suffix)
                               (push (concatenate 'string (list key) suffix) result))
                             (trie-get-all-words value))))
               (trie-root trie))
      result)))

(defun trie-get-words (trie prefix)
  "Get a list of all the words with the given prefix in trie."
  (when (trie-p trie)
    (let ((sub-trie (trie-find-trie trie prefix)))
      (when sub-trie
        (mapcar (lambda (suffix) (concatenate 'string prefix suffix)) (trie-get-all-words sub-trie))))))

(defun trie-has-word-p (trie word)
  "Is the word contained in this trie?"
  (trie-get-words trie word))
