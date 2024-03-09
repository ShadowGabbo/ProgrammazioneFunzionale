(*
Definire la funzione ricorsiva

    rmEven : int list -> int list

che cancella da una lista di interi tutti i numeri pari.
Non vanno usate le funzioni su liste definite in F#. 
*)

let rec rmEven lst = 
    match lst with
    |[]->[]
    |x::xs -> if x%2=0 then rmEven xs else x :: rmEven xs

let rm1 = rmEven [-10 .. 10 ] 
// val rm1 : int list = [-9; -7; -5; -3; -1; 1; 3; 5; 7; 9]
let rm2 = rmEven [2; 5; 5; 6; 6; 87; 6; 100; 2]  
// val rm2 : int list =  [5; 5; 87]

(*
Definire la funzione ricorsiva

    rmOddPos :  a' list -> 'a list

che cancella tutti gli elementi di una lista in posizione dispari;
il primo elemento della lista ha posizione 0.
Non vanno usate le funzioni su liste definite in F#. 
*)

let rec rmOddPos lst = 
    match lst with 
    |[]->[]
    |[x] -> [x]
    |x::y::xs -> x :: rmOddPos xs

let rmp1 = rmOddPos ['a'  .. 'z'] 
// val rmp1 : char list = ['a'; 'c'; 'e'; 'g'; 'i'; 'k'; 'm'; 'o'; 'q'; 's'; 'u'; 'w'; 'y']

let rmp2 = rmOddPos ["zero" ;  "uno" ; "due" ; "tre" ; "quattro"]  
// val rmp2 : string list = ["zero"; "due" ; "quattro"]

(*
Definire la funzione 

   split : 'a list -> 'a list * 'a list

che, data una lista, costruisce la coppia di liste 
degli elementi in posizione pari e in posizione dispari.
Non vanno usate le funzioni su liste definite in F#. 
*)

let rec split lst = 
    match lst with
    |[]->([],[])
    |[x]->([x], [])
    |x::y::xs -> 
        let (z, w) = split xs
        (x::z, y::w)

let s1 = split [0 .. 9]
// val s1 : int list * int list = ([0; 2; 4; 6; 8], [1; 3; 5; 7; 9])
let s2 = split ["ciao"] 
// val s2 : string list * string list = (["ciao"], [])
let s3 = split ["ciao" ; "ciao!!!" ]  
//val s3 : string list * string list = (["ciao"], ["ciao!!!"])
let s4 = split [ 'a' .. 'k'] 
// val s4 : char list * char list = (['a'; 'c'; 'e'; 'g'; 'i'; 'k'], ['b'; 'd'; 'f'; 'h'; 'j'])

(*
Definire la funzione ricorsiva

  val swap: 'a list -> 'a list

che scambia gli elementi di una lista a due a due; se la lista ha lunghezza dispari, l'ultimo elemento non cambia.
Non vanno usate le funzioni su liste definite in F#.
*)

let rec swap lst = 
    match lst with
    |[]->[]
    |[x]->[x]
    |x::y::xs -> y::x:: swap xs

let sw1 = swap [1 .. 6]
// val sw1: int list = [2; 1; 4; 3; 6; 5]
let sw2 = swap [1 .. 7]
// val sw2: int list = [2; 1; 4; 3; 6; 5; 7]
let sw3 = swap ['a'; 'b'; 'c'];;         
//val sw3: char list = ['b'; 'a'; 'c']

(*
Definire la funzione ricorsiva 

  cmpLength : cmpLength : 'a list -> 'b list -> int

che, date due liste xs e ys, confronta le lunghezza
(length) delle liste e restituisce:

   -1    se  length(xs) < length(ys)
    0    se  length(xs) = length(ys)
    1    se  length(xs) > length(ys)

Non vanno usate le funzioni su liste definite in F#
*)

let rec cmpLength lst0 lst1 = 
    match (lst0, lst1) with
    |([], []) -> 0
    |([x], []) -> 1
    |(x::xs, []) -> 1
    |([], [x]) -> -1
    |([], x::xs) -> -1
    |(x::xs, y::ys) -> cmpLength xs ys


let c1 = cmpLength  [1 .. 10]  ['a' .. 'z']   // -1
let c2 = cmpLength  [1 .. 26]  ['a' .. 'z']   // 0
let c3 = cmpLength  ['a'; 'b';'c']  ["e" ; "f"]  // 1

(*
Definire la funzione ricorsiva

  remove :  'a -> 'a list -> 'a list when 'a : equality

che dato un elemento x e una lista ys, restituisce la 
lista ottenuta da ys eliminando tutte le occorrenze di x.
Non vanno usate le funzioni su liste definite in F#. 
*)
let rec remove item lst = 
    match lst with
    |[]-> []
    |x::xs -> if x=item then remove item xs else x :: remove item xs

let x1s = remove  2  [0 ..10] 
//val x1s : int list = [0; 1; 3; 4; 5; 6; 7; 8; 9; 10]
let x2s = remove   "uva"  [ "mele" ; "uva" ; "pere" ; "uva" ; "banane" ; "uva" ] 
// val x2s : string list = ["mele"; "pere"; "banane"]
let  x3s = remove  11  [0 .. 10] 
// val x3s : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

(*
Usando remove (e non le funzioni F#), definire la funzione ricorsiva

    removeDup : 'a list -> 'a list when 'a : equality 

che rimuove tutti i duplicati in una lista.
Piu' precisamente, se un elemento x compare piu' volte,
viene mantenuta solo la prima occorrenza.
*)

let rec removeDup lst = 
    match lst with 
    |[]->[]
    |x::xs->x :: removeDup (remove x xs)

let x4s = removeDup [1; 2; 1; 2; 3] 
// val x4s : int list = [1; 2; 3]
let x5s = removeDup  [ "mele" ; "uva" ; "mele" ; "pere" ; "uva" ; "banane" ; "uva" ; "pere" ; "pere" ; "banane"] 
// val x5s : string list = ["mele"; "uva"; "pere"; "banane"]

(*
Definire le funzioni  ricorsive

  downTo : int -> int list      
    upTo : int -> int list

tali che
 
    upTo n  = lista degli interi da 0 a n

In entrambe le funzioni si *assume* n >= 0 
(attenzione a non scrivere  `downto', che e' una keyword di F#)

Non vanno usate le funzioni su liste definite in F#.
*)

let rec downTo num = 
    match num with 
    |0 -> [0]
    |_ -> num :: downTo (num-1)

let downTo10  =  downTo 10 
// val downTo10 : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]

// si potrebbe fare meglio
let upTo num =
    let rec inner item = 
        match item<num with
        |false -> [num]
        |true -> item :: inner (item+1)
    inner 0

let upTo10  =  upTo 10 
// val upto10 : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]