;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic programming solving the egg drop problem, defined as:
;;
;; You are given two eggs, and access to a 100-storey building. Both eggs
;; are identical. The aim is to find out the highest floor from which an
;; egg will not break when dropped out of a window from that floor. If an
;; egg is dropped and does not break, it is undamaged and can be dropped
;; again. However, once an egg is broken, that’s it for that egg.
;;
;; If an egg breaks when dropped from floor n, then it would also have
;; broken from any floor above that. If an egg survives a fall, then it
;; will survive any fall shorter than that.
;;
;; The question is: What strategy should you adopt to minimize the number
;; egg drops it takes to find the solution?. (And what is the worst case
;; for the number of drops it will take?)
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *egg-drop-table* (make-array '(5000 5000) :initial-element nil) "For the memoized version of the egg drop problem.")

(defun egg-drop-recursive (eggs floors)
  "Returns the worst case number of egg droppings we have to do to find the floor where eggs break in a building of n floors recursively."
  (if (or (= eggs 1) (<= floors 1))
      floors
      (1+ (loop
             for i from 1 to floors
             minimizing (max (egg-drop-recursive eggs (- floors i))
                             (egg-drop-recursive (1- eggs) (1- i)))))))

(defun egg-drop-memoized (eggs floors)
  "Returns the worst case number of egg droppings we have to do to find the floor where eggs break in a building of n floors recursively
   but caching results from previous computations."
  (let ((cached-result (aref *egg-drop-table* floors eggs)))
    (if cached-result
        cached-result
        (setf (aref *egg-drop-table* floors eggs)
              (if (or (= eggs 1) (<= floors 1))
                  floors
                  (1+ (loop
                         for i from 1 to floors
                         minimizing (max (egg-drop-memoized eggs (- floors i))
                                         (egg-drop-memoized (1- eggs) (1- i))))))))))

(defun egg-drop-dynamic-programming (eggs floors)
  "Returns the worst case number of egg droppings we have to do to find the floor where eggs break in a building of n floors
   with dynamic programming."
  (let ((egg-matrix (make-array (list (1+ floors) (1+ eggs)))))
    ;; When you have 0 floors you need zero drops, when you have one floor you need only one drop.
    (loop for i from 1 to eggs do
         (setf (aref egg-matrix 0 i) 0)
         (setf (aref egg-matrix 1 i) 1))
    ;; When you only one egg, you need as many drops as there are floors.
    (loop for i from 1 to floors do (setf (aref egg-matrix i 1) i))
    (loop
       for egg from 2 to eggs do
         (loop
            for floor from 2 to floors do
              (setf (aref egg-matrix floor egg)
                    (1+ (loop
                           for i from 1 to floor
                           minimizing (max (aref egg-matrix (- floor i)  egg)
                                           (aref egg-matrix (1- i) (1- egg))))))))
    (aref egg-matrix floors eggs)))
