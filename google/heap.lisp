;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; max/min heap implementation
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +heap-max-size+ 100)

(defstruct (heap (:constructor %make-heap (&key table size order comparer)))
  table size  (order nil :read-only t) (comparer nil :read-only t))

(defun make-heap (&optional (type :max))
  (setf type (if (member type '(:max :min)) type :max))
  (%make-heap :table (make-array (1+ +heap-max-size+) :initial-element nil)
              :size 0
              :order type
              :comparer (if (eq type :max) #'> #'<)))

(defun heap-parent-index (i)
  (unless (<= i 1) (ash i -1)))

(defun heap-left-index (i)
  (ash i 1))

(defun heap-right-index (i)
  (1+ (ash i 1)))

(defun heap-last-index (heap)
  (1+ (heap-size heap)))

(defun heap-heapify (heap i)
  "Assumes the parents obey the heap property."
  (when (heap-p heap)
    (let ((left (heap-left-index i))
          (right (heap-right-index i))
          (arr (heap-table heap))
          (size (heap-size heap))
          (target))
      (setf target (if (and (<= left size) (funcall (heap-comparer heap) (aref arr left) (aref arr i)))
                       left
                       i))
      (setf target (if (and (<= right size) (funcall (heap-comparer heap) (aref arr right) (aref arr target)))
                       right
                       target))
      (when (/= i target)
        (rotatef (aref arr i) (aref arr target))
        (heap-heapify heap target))
      heap)))

(defun heap-insert (heap key)
  (labels ((heap-bubble (heap i)
             (when (and (heap-parent-index i)
                        (funcall (heap-comparer heap)
                                 (aref (heap-table heap) i)
                                 (aref (heap-table heap) (heap-parent-index i))))
               (rotatef (aref (heap-table heap) i)
                        (aref (heap-table heap) (heap-parent-index i)))
               (heap-bubble heap (heap-parent-index i)))))
    (when (and (heap-p heap) (< (heap-size heap) +heap-max-size+))
      (setf (aref (heap-table heap) (heap-last-index heap)) key)
      (incf (heap-size heap))
      (heap-bubble heap (1- (heap-last-index heap)))
      heap)))

(defun heap-topmost-extract (heap)
  (when (and (heap-p heap) (plusp (heap-size heap)))
    (let ((top (aref (heap-table heap) 1)))
      (setf (aref (heap-table heap) 1) (aref (heap-table heap) (heap-size heap)))
      (setf (aref (heap-table heap) (heap-size heap)) nil)
      (decf (heap-size heap))
      (heap-heapify heap 1)
      top)))

(defun heap-topmost-peek (heap)
  (when (and (heap-p heap) (plusp (heap-size heap)))
    (aref (heap-table heap) 1)))
