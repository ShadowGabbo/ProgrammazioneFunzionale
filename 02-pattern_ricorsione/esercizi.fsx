(*

Esercizio 1: esponenziale
^^^^^^^^^^^^^^^^^^^^^^^

Definire una funzione ricorsiva

   exp : float -> int -> float

che calcola l'esponenziale:

  exp b n = b ^n   

Si *assume* n>= 0. 
Usiamo la seguente definizione ricorsiva (induzione su n)

  b^n   =  1                se n = 0    [CASO BASE}

  b^n  =   b * b^(n-1)      se n > 0    [PASSO INDUTTIVO] 

*)

let rec exp (b:float) n  = 
    match n with 
    |0 -> 1.0
    |_ -> b * exp b (n-1)

let e1 = exp 3 3
let e2 = exp 4 2
let e3 = exp 2 5

(*

Esercizio 2
^^^^^^^^^^^

i) Definire una funzione ricorsiva

make_str : int -> string

che, dato un intero n>=0, costruisce la stringa "0 1 2 ... n"

Esempio:

make_str 20 

deve costruire la stinga

 "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20"

Per concatenare stringhe, usare operatore +.
Per convertire da intero a stringa, usare la funzione

   string : int ->  string
*)

let rec make_str num = 
    match num with
    |0 -> "0"
    |_ -> make_str (num-1) + " " + string num 

let m1 = make_str 20 
let m2 = make_str 25
let m3 = make_str 10

(*
ii) Definire la funzione ricorsiva

  make_sum_str  int -> int * string

tale che 

  make_sum_str n

calcola la coppia (sum,str) dove:

- sum e' l'intero corrispondente alla somma  0 + 1 + 2 + ... + n
- str e' stringa "0 1 2 ... n"  (come nel punto precedente)

Va fatta una *unica* chiamata ricorsiva  

Esempio:

mk_sum_str 5 
 val it : int * string = (15, "0 1 2 3 4 5")
*)

let mk_sum_str num = 
    let rec calc_sum num = 
        match num with
        | 0 -> 0 
        |_ -> calc_sum (num-1) + num
    (calc_sum num, make_str num)

let n1 = mk_sum_str 5
let n2 = mk_sum_str 10
let n3 = mk_sum_str 15

(*
iii) Definire  una funzione

  somma_n : int -> string

tale che

   somma_n n

stampa una stringa della forma

  "0 + 1 + ... + n = k"

con k il valore della somma 0+1+ ... +k


Esempi:

somma_n 5 ;;
// val it : string = "0 + 1 + 2 + 3 + 4 + 5 = 15"

somma_n 10 ;;
// val it : string = "0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 = 55"

Suggerimento
^^^^^^^^^^^^

Definire una funzione ausiliaria

 make_sum_str1  int -> int * string

analoga alla funzione make_sum_str del punto ii) in cui pero' la stringa ha 
il formato

 "0 + 1 + ... + n"

La funzione ausiliaria puo' anche essere definita internamente alla funzione somma_n
*)

let somma_n n = 
    let rec make_sum_str1 n =
        match n with
        |0 -> "0"
        |_ -> make_sum_str1 (n-1) + " + " + string n
    let rec calc_sum num = 
        match num with
        | 0 -> 0 
        |_ -> calc_sum (num-1) + num
    make_sum_str1 n + " = " + string (calc_sum n)

let str1 = somma_n 5
let str2 = somma_n 10
    