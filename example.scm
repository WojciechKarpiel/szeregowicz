;; ./szeregowacz.scm example.scm
((node 'a1  "sleep 2" "sleep 3" "echo hello a1")
 (node 'a2 "sleep 4")
 (node 'b "sleep 2" "echo hello b")
 (node 'c1 "sleep 2")
 (node 'c2 "sleep 1")
 (dep 'a1 'b)
 (dep 'a2 'b)
 (dep 'b 'c1)
 (dep 'b 'c2))
