;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphs implementation
;;
;; Practice for Google's interviews
;;
;; Francisco Soto <ebobby@ebobby.org>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Our graph representation is an adjacency list formed by a hash table and lists.
;;; Association lists make for easier to type graphs, so we provide a convertion function.
(defun assoc->graph (alist)
  "Association list to graph."
  (let ((graph (make-hash-table :test #'equal :size (length alist))))
    (mapcar (lambda (node-info)
              (let* ((node (first node-info))
                     (edge-list (second node-info)))
                (setf (gethash node graph) edge-list)))
            alist)
    graph))

(defun graph->assoc (graph)
  "Converts a graph to an association list."
  (let ((alist '()))
    (maphash (lambda (node edges)
               (setf alist (acons node edges alist)))
             graph)
    (reverse alist)))

(defun make-graph ()
  "Construct a new graph."
  (make-hash-table :test #'equal :size 32))

(defun graph-add-node (graph node &optional (edges '()))
  "Add a node to the graph with a given list of edges if given."
  (setf (gethash node graph) edges))

(defun graph-add-edge (graph node edge)
  "Adds an edge to the node."
  (let ((edges (gethash node graph)))
    (if edges
        (let ((existing-edge (assoc (first edge) edges)))
          (if existing-edge
              (rplacd existing-edge (cdr edge))
              (setf (gethash node graph) (cons edge edges))))
        (graph-add-node graph node (list edge)))))

(defun graph-get-nodes (graph)
  "Get a list of all the nodes in the graph."
  (loop for k being the hash-keys in graph collect k))

(defun graph-get-edges (graph node)
  "Gets the list of edges for node."
  (gethash node graph))

(defun graph-edge-p (graph n1 n2)
  "Is there an edge between n1 and n1?"
  (find n2 (gethash n1 graph)))

(defun graph-node-p (graph node)
  "Is there a node node in the graph?"
  (gethash graph node))

(defun graph-node-count (graph)
  "How many nodes are there in this graph?"
  (hash-table-count graph))

(defun graph-search (graph starting-node &key (process-node #'identity) (process-edge (lambda (&rest a) a)) (add-nodes #'append))
  "Does a graph search on the graph/starting node, calling the given functions.
   process-node gets called when a node is first seen.
   process-edge gets called when an edge is first seen, A->B is not equals B->A though.
   add-nodes (new total)should return a list with the given nodes added to it. nodes get processed left to right, this defines the search algorithm."
  (labels ((traverse-graph (nodes visited-nodes)
             (unless (null nodes)
               (let ((node (pop nodes)))
                 (unless (gethash node visited-nodes)
                   (funcall process-node node)
                   (mapcar (lambda (adjacent-node)
                             (funcall process-edge node adjacent-node))
                           (graph-get-edges graph node))
                   (setf nodes (funcall add-nodes (graph-get-edges graph node) nodes))
                   (setf (gethash node visited-nodes) T))
                 (traverse-graph nodes visited-nodes)))))
    (traverse-graph (list starting-node) (make-hash-table :test #'equal :size (graph-node-count graph)))))

(defun breadth-first-search (graph starting-node &key (process-node #'identity) (process-edge (lambda (&rest a) a)))
  "Traverse the graph on a breadth-first basis and calls the given functions."
  (graph-search graph
                starting-node
                :process-node process-node
                :process-edge process-edge
                ;; queue
                :add-nodes (lambda (new total) (append total new))))

(defun depth-first-search (graph starting-node &key (process-node #'identity) (process-edge (lambda (&rest a) a)))
  "Traverse the graph on a depth-first basis and calls the given functions."
  (graph-search graph
                starting-node
                :process-node process-node
                :process-edge process-edge
                ;; stack
                :add-nodes (lambda (new total) (append new total))))
