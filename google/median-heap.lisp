;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; median heap implementation
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mix/max heaps
(load "heap")

(defstruct (median-heap (:constructor make-median-heap ()))
  (left (make-heap :max))
  (right (make-heap :min)))

(defun median-heap-empty-p (heap)
  (and (median-heap-p heap)
       (zerop (heap-size (median-heap-left heap)))
       (zerop (heap-size (median-heap-right heap)))))

(defun median-heap-rebalance (heap)
  (let (from to)
    (cond ((> (heap-size (median-heap-left heap)) (1+ (heap-size (median-heap-right heap))))
           (setf from (median-heap-left heap))
           (setf to (median-heap-right heap)))
          ((> (heap-size (median-heap-right heap)) (heap-size (median-heap-left heap)))
           (setf from (median-heap-right heap))
           (setf to (median-heap-left heap))))
    (when (and from to)
      (heap-insert to (heap-topmost-extract from)))))

(defun median-heap-insert (heap key)
  (macrolet ((left (heap-sym)
               `(median-heap-left ,heap-sym))
             (right (heap-sym)
               `(median-heap-right ,heap-sym)))
    (when (median-heap-p heap)
      (cond ((median-heap-empty-p heap)
             (heap-insert (left heap) key))
            ((and (plusp (heap-size (left heap)))
                  (plusp (heap-size (right heap))))
             (if (> key (heap-topmost-peek (left heap)))
                 (heap-insert (right heap) key)
                 (heap-insert (left heap) key)))
            (t
             (if (null (heap-topmost-peek (left heap)))
                 (heap-insert (left heap) key)
                 (heap-insert (right heap) key))
             (when (and (> (heap-topmost-peek (left heap))
                           (heap-topmost-peek (right heap))))
               (let (swap)
                 (setf swap (heap-topmost-extract (left heap)))
                 (heap-insert (left heap) (heap-topmost-extract (right heap)))
                 (heap-insert (right heap) swap)))))
      (median-heap-rebalance heap)
      heap)))

(defun median-heap-topmost-extract (heap)
  (when (and (median-heap-p heap) (not (median-heap-empty-p heap)))
    (let ((result (if (= (heap-size (median-heap-left heap)) (heap-size (median-heap-right heap)))
                      (float (/ (+ (heap-topmost-extract (median-heap-left heap)) (heap-topmost-extract (median-heap-right heap))) 2))
                      (heap-topmost-extract (median-heap-left heap)))))
      (median-heap-rebalance heap)
      result)))

(defun median-heap-topmost-peek (heap)
  (when (and (median-heap-p heap) (not (median-heap-empty-p heap)))
    (if (= (heap-size (median-heap-left heap)) (heap-size (median-heap-right heap)))
        (float (/ (+ (heap-topmost-peek (median-heap-left heap)) (heap-topmost-peek (median-heap-right heap))) 2))
        (heap-topmost-peek (median-heap-left heap)))))
