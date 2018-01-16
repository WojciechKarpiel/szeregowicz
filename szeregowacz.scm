#!/usr/local/bin/guile \
-e main -s
!#
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (ice-9 threads))
(use-modules (ice-9 match))
(use-modules (ice-9 local-eval))

(define-record-type <node>
  (%make-node name mutex deps-count dependent-nodes commands) ;mutex guards deps-count
  node?
  (name node-name)
  (mutex node-mutex)
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

(define (main args)
  (let* ((file-descriptors (cdr args))
         (graph-definitions (if (null? file-descriptors)
                                (list (read))
                                (map (lambda (descriptor) (call-with-input-file descriptor read)) file-descriptors)))
         (node-list '())
         (node (lambda (name .  commands) (set! node-list (cons (make-node name commands) node-list))))
         (dep (lambda (parent-name child-name) (set-dependent! (find (lambda (node) (eq? (node-name node) parent-name)) node-list)
                                                          (find (lambda (node) (eq? (node-name node) child-name)) node-list))))
         (environment (the-environment)))
    (for-each (lambda (graph-definition)
                (for-each (lambda (def) (local-eval def environment)) graph-definition))
              graph-definitions)
    (run node-list)))
