(* 
Usando la funzione List.map:

1) Costruire la lista sq10 dei quadrati dei numeri da 1 a 10:

 sq10 =  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]

2) Definire la funzione
  
  sqList : int -> int list

che, dato n >=1, costruisce la lista dei quadrati da 1 a n.

Ad esempio:

  sqList 4 =  [1; 4; 9; 16]

*)  
let sq10 = List.map (fun x -> x*x) [1..10]
let sqList n = List.map (fun x -> x*x) [1..n]
let s1 = sqList 4

(*
Usando List.filter

1) Definire la funzione

   pari : int -> int list

che, dato n >=0, costruisce la lista dei numeri pari compresi fra 0 e n.

Ad esempio:

   pari 10 = [0; 2; 4; 6; 8; 10]
   pari 15 = [0; 2; 4; 6; 8; 10; 12; 14]

2) Definire la funzione

 mult : int -> int -> int list

che, dati due interi k e m, tali che 0 < k <= m,
costruisce la lista dei multipli di k compresi tra k e m.

Ad esempio:

  mult 3 15 = [3;  6;  9; 12; 15]
  mult 5 27 = [5; 10; 15; 20; 25]

*)

let pari n = List.filter (fun x -> x%2=0) [1..n]
let p1 = pari 10
let p2 = pari 15

let mult k m = List.filter (fun x -> x%k=0) [k..m]
let m1 = mult 3 15
let m2 = mult 5 27

(*
1) Usando List.exists, definire la funzione

    contains : x:'a -> xs:'a list -> bool when 'a : equality

che controlla se un elemento x appartiene a una lista xs.
 
2)  Usando List.exists, definire la funzione

  isSquare : int -> bool

che, dato un intero n >= 0, determina se n e' un quadrato perfetto.

Verificare che

  List.filter isSquare [0 .. 200]

produce la lista

[0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196]
 

Suggerimento
^^^^^^^^^^^^
Osservare che  n e' un quadrato perfetto SE E SOLO SE
 esiste  k tale che 0 <= k <= (n/2 + 1)  e n = k * k     
*)   

let contains item list = List.exists (fun x -> x=item) list
let c1 = contains 5 [1..7]
let c2 = contains 3 [4..10]

let isSquare n = List.exists (fun x -> x*x=n) [1..n/2+1]
let i1 = List.filter isSquare [0 .. 200]
