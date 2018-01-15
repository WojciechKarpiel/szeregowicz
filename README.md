# Szeregowicz

Zapodaj graf zależności między zadaniami a SZEREGOWICZ wykona współbieżnie zadania które da się wykonać współbieżnie!

# Przykład

```
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
```

```
wojciech@lisp-machine ~/K/szeregowacz> guile -s szeregowacz.scm 
Starting work
Started processing node: node1a
Started processing node: node1b
Finished processing node: node1a
Finished processing node: node1b
Started processing node: node2
Finished processing node: node2
Started processing node: node3b
Started processing node: node3a
Finished processing node: node3a
Finished processing node: node3b
Trud skończony
```
