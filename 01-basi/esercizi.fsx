(*
    1) Definire la funzione

    orb : bool ->  bool -> bool

    che calcola or di due valori booleani.
*)

let orb a b = a || b 

let a1 = orb true true 
let a2 = orb true false 
let a3 = orb true false 
let a4 = orb false false 

(*
2) Definire la funzione

  isPariString : int -> string

che, applicata a un intero n, restituisce la stringa  "pari" se n e' pari,
"dispari"  se n e' dispari.

Definire quindi la funzione

 isPariString1 : int -> string

che, applicata a un intero n, restituisce una stringa che descrive se n e' pari o dispari

Esempio:

isPariString1 4  // "4 e' un numero pari"
isPariString1 5  // "5 e' un numero dispari"
*)
let isPariString1 num = 
    match num%2=0 with 
    |false -> string num + " e' un numero dispari"
    |true -> string num + " e' un numero pari"

let b1 = isPariString1 4
let b2 = isPariString1 5