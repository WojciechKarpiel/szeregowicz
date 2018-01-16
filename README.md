# Szeregowacz

Zapodaj graf zależności między zadaniami a SZEREGOWACZ wykona współbieżnie zadania które da się wykonać współbieżnie!

# Przykład

```
wojciech@lisp-machine ~/K/szeregowacz> ./szeregowacz.scm example.scm 
Starting work
Started processing node: a2
Started processing node: a1
hello a1
Finished processing node: a1
Finished processing node: a2
Started processing node: b
hello b
Finished processing node: b
Started processing node: c2
Started processing node: c1
Finished processing node: c2
Finished processing node: c1
Trud skończony
```

# Wywołanie

```
> ./szeregowacz # czyta definicję grafu z STDIN
> ./szeregowacz plik1 plik2 ... # czyta definicję grafu z plików
```

# Opis grafu

Podaje się listę deklaracji. Deklaruje się węzły przez `node` a powiązania między węzłami przez `dep`
```
(
 (node 'nazwa-węzła "komenda-współbieżna 1" "komenda-współbieżna 2" ...)
 (dep 'nazwa-dostarczyciela-zależności 'nazwa-potrzebującego)
)
```
