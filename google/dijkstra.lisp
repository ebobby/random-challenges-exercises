;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dijkstra's shortest path implementation
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "graph")

(defparameter *simple-graph* '((A ((B . 5) (C . 7) (D . 12)))
                               (B ((E . 7) (C . 9) (A . 5)))
                               (C ((A . 7) (B . 9) (D . 4) (E . 4) (F . 3)))
                               (D ((A . 12) (C . 4) (F . 7)))
                               (E ((B . 7) (C . 4) (F . 2) (G . 5)))
                               (F ((D . 7) (E . 2) (C . 3) (G . 2)))
                               (G ((E . 5) (F . 2)))))

(defun dijkstra (graph starting-node)
  "Returns the minimum spanning tree of the given weighted graph using Prim's algorithm.."
  (loop
     with tree = (make-graph)
     with nodes = (graph-get-nodes graph)
     with weights = (make-hash-table :test #'equal)
     with parents = (make-hash-table :test #'equal)
     with processed = (make-hash-table :test #'equal)
     with current-node = starting-node
     initially
       (setf (gethash current-node weights) 0)
     until (gethash current-node processed)
     do
       (setf (gethash current-node processed) t)
       (unless (graph-node-p tree current-node)
         (graph-add-node tree current-node))
     ;; Check if we have a shorter path in our edges for nodes we've seen and not yet added.
       (loop
          for edge in (graph-get-edges graph current-node)
          for edge-node = (car edge)
          for weight = (cdr edge)
          for current-weight = (gethash edge-node weights)
          for current-node-weight = (gethash current-node weights)
          when (and (or (null current-weight) (> current-weight (+ current-node-weight weight))))
          do
            (setf (gethash edge-node weights) (+ current-node-weight weight))
            (setf (gethash edge-node parents) current-node))
     ;; Pick up the next shorter path we have, add that node to the tree and process that node.
       (loop
          with w = nil
          with next-node = nil
          for i in nodes
          for d = (gethash i weights)
          when (and (not (null d)) (or (null w) (> w d)) (null (gethash i processed)))
          do
            (setf w d)
            (setf next-node i)
          finally
            (when next-node
              (graph-add-edge tree (gethash next-node parents) (cons next-node w))
              (graph-add-edge tree next-node (cons (gethash next-node parents) w))
              (setf current-node next-node)))
     finally (return tree)))
