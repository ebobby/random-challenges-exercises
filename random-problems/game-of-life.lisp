;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conway's Game Of Life
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cellboard (width height)
  "Constructs a cell board, all cells are dead initially."
  (make-array (list height width) :element-type 'symbol :initial-element :dead))

(defun make-randomized-cellboard (width height)
  "Makes a cellboard with randomized alive cells."
  (let* ((result (make-cellboard width height)))
    (dotimes (j height)
      (dotimes (i width)
        (when (> (random 1.0) 0.75)
          (cell-revive result i j))))
    result))

(defun cellboard-width (cellboard)
  "Returns of the width of the cellboard."
  (and (arrayp cellboard) (array-dimension cellboard 1)))

(defun cellboard-height (cellboard)
  "Returns of the height of the cellboard."
  (and (arrayp cellboard) (array-dimension cellboard 0)))

(defun cell-revive (cellboard x y)
  "Revive a cell."
  (and (arrayp cellboard) (setf (aref cellboard y x) :live)))

(defun cell-kill (cellboard x y)
  "Kill a cell."
  (and (arrayp cellboard) (setf (aref cellboard y x) :dead)))

(defun cell-dead-p (cellboard x y)
  "Is the cell dead?"
  (and (arrayp cellboard) (eq (aref cellboard y x) :dead)))

(defun cell-alive-p (cellboard x y)
  "Is the cell alive?"
  (and (arrayp cellboard) (eq (aref cellboard y x) :live)))

(defun cell-neighbors (cellboard x y)
  "Return the number of alive neighbors this cell has."
  (when (arrayp cellboard)
    (let ((width (cellboard-width cellboard))
          (height (cellboard-height cellboard)))
      (loop
         for j from (1- y) to (1+ y)
         sum (loop
                for i from (1- x) to (1+ x)
                unless (or (minusp i) (minusp j) (>= i width) (>= j height) (and (= i x) (= j y)))
                count (cell-alive-p cellboard i j))))))

(defun game-of-life (cellboard)
  "Receives a cellboard and returns a new cellboard after applying the game of life rules to it."
  (when (arrayp cellboard)
    (let* ((width (cellboard-width cellboard))
           (height (cellboard-height cellboard))
           (result (make-cellboard width height)))
      (dotimes (j height)
        (dotimes (i width)
          (let ((neighbors (cell-neighbors cellboard i j)))
            (if (cell-alive-p cellboard i j)
                (when (or (= neighbors 2) (= neighbors 3))
                  (cell-revive result i j))                 ; this cell gets to live for another generation.
                (when (= neighbors 3)
                  (cell-revive result i j))))))             ; this cell comes back to life.
      result)))

(defun cellboard-print (cellboard)
  "Print the cellboard."
  (when (arrayp cellboard)
    (dotimes (j (cellboard-height cellboard))
      (dotimes (i (cellboard-width cellboard))
        (format t "~C " (if (cell-alive-p cellboard i j) #\o #\Space)))
      (format t "~%"))))

(defun test-run ()
  "Does a randomized game of life forever."
  (labels ((clear-screen ()
             "Clears the screen and puts the cursor at the home position. HIGHLY NON-PORTABLE."
             (format t "~C[2J" #\Esc)
             (format t "~C[H" #\Esc)))
    (let ((cellboard (make-randomized-cellboard 50 50))
          (generation 0))
      (loop
         do
           (clear-screen)
           (format t "Generation: ~a~%" generation)
                     (cellboard-print cellboard)
           (setf cellboard (game-of-life cellboard))
           (incf generation)
           (sleep 1)))))

;; Uncomment this when running this as a script.
;(test-run)
