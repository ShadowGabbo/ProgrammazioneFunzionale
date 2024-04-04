(*
Esercizio 5

i) Definire la funzione ricorsiva higher-order

    map : ('a -> 'b) -> seq<'a> -> seq<'b>

analoga alla funzione  map su liste in cui si assume che la sequenza sia infinita.

Piu' precisamente, data una sequenza infinita

   sq = seq [ e0 ; e1 ; e2 ; .... ]   : seq<'a>

e una funzione f : 'a -> 'b, vale:

    map f sq  =  seq [ f e0  ; f e1  ; f e2  ; .... ]   : seq<'b>

Notare che si *assume* che sq sia infinita. 
Questo implica che sq contenga almeno un elemento, quindi
la testa e la coda di sq sono *sempre* definite.
Inoltre, la coda di sq e' a sua volta una sequenza infinita.
*)
let nat =  Seq.initInfinite id

let rec map f sq = 
    seq{
        let first_item = Seq.item 0 sq
        yield f(first_item)
        yield! map f (Seq.skip 1 sq) 
    }

(*
ii)  Applicare map alla sequenza infinita nat dei naturali 
per generare la sequenza infinita squares  dei quadrati dei naturali.
  
Verificare che la lista dei primi 15 elementi di squares e':

[0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196; 225; 256; 289; 324; 361]


*)
//  map : f:('a -> 'b) -> sq:seq<'a> -> seq<'b>
// Si *assume* che la sequenza sq sia infinita 
let squares = map (fun x -> x * x) nat 
// seq [0; 1; 4; 9; ...] (sequenza dei quadrati dei  numeri naturali)

// lista dei primi 15 quadrati
squares |> Seq.take 15 |> Seq.toList 
// [0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196]

(*
Esercizio 6   

i) Definire la funzione ricorsiva  higher-order

 filter : ('a -> bool) -> seq<'a> -> seq<'a>

che, dato un predicato pred : 'a -> bool e una sequenza *infinita* sq,
genera la sequenza degli elementi di sq che verificano sq.

ii) Applicare filter alla sequenza infinita nat dei naturali
per generare la sequenza infinita dei multipli di 3 (0, 3, 6, ...)

Verificare che la lista dei primi 20 elementi della sequenza generata e'

 [0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48; 51; 54; 57]

*)

// filter : pred:('a -> bool) -> sq:seq<'a> -> seq<'a>
// Si *assume* che la sequenza sq sia infinita
let rec filter pred sq =
    seq{
        let first = Seq.item 0 sq
        if pred first then yield first
        yield! filter pred (Seq.skip 1 sq)
    }

// primi 20 multipli di 3
filter (fun x -> x%3 = 0 ) nat |> Seq.take 20 |> Seq.toList  

(*
Esercizio 7   

i) Definire la funzione
   
   sumSeq : seq<int> -> seq<int>

che, data una sequenza infinita sq di interi

  n0, n1, n2, n3, .....

costruisce la sequenza infinita ssq delle somme di sq,
cioe' la sequenza i cui elementi sono 

 n0, n0 + n1, n0 + n1 + n2, n0 + n1 + n2 + n3, ....

Suggerimento
^^^^^^^^^^^^

Consideriamo la sottosequenza di ssq che parte da n0 + n1:
   
  n0 + n1, n0 + n1 + n2, n0 + n1 + n2 + n3 ...
  
Tale sequenza puo' essere ottenuta applicando ricorsivamente sumSeq 
alla sequenza infinita sq1 i cui elementi sono:

  n0 + n1, n2,  n3, n4 ...

La sequenza sq1 e' facilmente costruibile a partire da sq.


ii) Verificare che la lista dei primi 15 elementi della sequenza

    sumSeq nat 

e'

 [0; 1; 3; 6; 10; 15; 21; 28; 36; 45; 55; 66; 78; 91; 105]

*)

// sumSeq : sq:seq<int> -> seq<int>
// Si *assume* che sq sia infinita
let sumSeq sq =
    let rec helper sq2 sum =
        seq{
            let head = Seq.head sq2
            let tail = Seq.tail sq2
            yield (head + sum)
            yield! helper tail (sum+head)
        } 
    helper sq 0


// lista dei primi 15 elementi di (sumSeq nat) 
let s1 = sumSeq nat  |>  Seq.take 15 |> Seq.toList 
// [0; 1; 3; 6; 10; 15; 21; 28; 36; 45; 55; 66; 78; 91; 105]