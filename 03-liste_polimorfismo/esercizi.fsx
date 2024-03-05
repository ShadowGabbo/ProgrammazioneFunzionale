(*
Definire la funzione ricorsiva (polimorfa)

     length : 'a list -> int

che calcola la lunghezza di una lista.
*)

let rec length lst = 
    match lst with 
    |[] -> 0
    |x::xs -> 1 + length xs

let l1 = length [1 .. 10]
let l2 = length ['a' .. 'z']
let l3 = length [(1, 3); (4, 7); (6, 1)]

(*
Definire la funzione ricorsiva (polimorfa)

   rev : 'a list -> 'a list

che inverte gli elementi di una lista (analoga a List.rev):

  rev [ x0 ; x1 ; .... ; x(n-1) ; xn ]  = [ xn ; x(n-1) ; ... ; x1 ; x0 ]  

===

Notare che il pattern matching permette di estrarre il primo elemento della lista
ma non l'ultimo.

La lista 
 
     [ xn ; x(n-1) ; ... ; x1 ; x0 ]  

puo' pero' essere vista come la concatenazione delle due liste

   [ xn ; x(n-1) ; ... ; x1 ]      [x0]

Inoltre [ xn ; x(n-1) ; ... ; x1 ] puo' essere costruita usando una chiamata ricorsiva.


rev [1 .. 10]         // [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]

*)
let rec rev lst = 
    match lst with 
    |[] -> []
    |[x] -> [x]
    |x::xs -> rev xs @ [x]

let r1 = rev [1..10]
let r2 = rev ['a'..'z']