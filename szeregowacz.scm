(use-modules (srfi srfi-9))
(use-modules (ice-9 threads))

(define-record-type <node>
  (%make-node name mutex deps-count dependent-nodes commands) ;mutex guards deps-count
  node?
  (name    node-name)
  (mutex     node-mutex)
  (deps-count  node-deps-count set-node-deps-count!)
  (dependent-nodes node-dependent-nodes set-node-dependent-nodes!)
  (commands node-commands set-node-commands!))

(define (make-node name commands)
  (%make-node name (make-mutex) 0 '() commands))

(define (compute-node node)
  (display "Started processing node: ")
  (display (node-name node))
  (newline)
  ;;step I - exec all commands in parallel
  (for-each join-thread
            (map (lambda (command) (call-with-new-thread (lambda () (system command)))) (node-commands node)))
  (display "Finished processing node: ")
  (display (node-name node))
  (newline)
  ;;step II - let dependent nodes know that dependency is fullfiled
  (let* ((nodes-and-deps-count
          (map (lambda (node)
                 (with-mutex (node-mutex node)
                   (let ((new-count (set-node-deps-count! node (- (node-deps-count node) 1))))
                     (cons node new-count)))) (node-dependent-nodes node))) ; count is not cons cell and won't change after we exit mutex
         (nodes-without-unsatisfied-deps
          (map car (filter (lambda (c) (= 0 (cdr c))) nodes-and-deps-count))))
    (for-each join-thread
              (map (lambda (node) (call-with-new-thread (lambda () (compute-node node)))) nodes-without-unsatisfied-deps))))

(define (set-dependent! dependency-provider dependency-needer)
  (set-node-deps-count! dependency-needer (+ 1 (node-deps-count dependency-needer)))
  (set-node-dependent-nodes! dependency-provider (cons dependency-needer (node-dependent-nodes dependency-provider))))

(define (run nodes)
  (display "Starting work") (newline)
  (let ((root-nodes (filter (lambda (node) (= 0 (node-deps-count node))) nodes)))
    (for-each join-thread
              (map (lambda (node) (call-with-new-thread (lambda () (compute-node node)))) root-nodes)))
  (display "Trud skończony") (newline))


;; example:
;; TODO: provide API, more convinient declaration of dependencies etc. For now it's raw example

(define node1a (make-node "node1a" '("sleep 3")))
(define node1b (make-node "node1b" '("sleep 5")))
(define node2 (make-node "node2" '("sleep 1")))
(define node3a (make-node "node3a" '("sleep 2")))
(define node3b (make-node "node3b" '("sleep 3")))
(set-dependent! node1a node2)
(set-dependent! node1b node2)
(set-dependent! node2 node3a)
(set-dependent! node2 node3b)
(run (list node1a node1b node2 node3a node3b))
