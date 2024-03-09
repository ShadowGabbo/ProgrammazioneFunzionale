type figura = 
   | Rettangolo of  float * float      // (base, altezza)
   | Quadrato   of  float              // lato
   | Triangolo  of  float * float      // (base, altezza)

//  area : figura -> float 
let area fig =
   match fig with
   | Rettangolo(b,h) ->   b * h     
   | Quadrato l      ->   l * l  
   | Triangolo(b,h)  ->  ( b * h )  / 2.0 

// well_formed: figura -> bool
let well_formed fig =
   match fig with
   | Rettangolo(b,h) | Triangolo(b,h) ->   b >= 0 && h >= 0     
   | Quadrato l      ->   l >= 0  


(*
Definire la funzione areaOpt che calcola l'area di una figura fig, se fig e' ben definita. 
La funzione areaOpt ha tipo

  areaOpt : figura -> float option

e restituisce:

-   None       se fig non e' ben definita 
-   Some a     se fig e' ben definita e l'area di fig e' a (un float).
*)

let areaOpt fig = if well_formed fig then Some(area fig) else None

let a1 = areaOpt ( Rettangolo(2.0,3.0) ) 
// val a1 : float option = Some 6.0
let a2 = areaOpt ( Rettangolo(2.0, -3.0) ) 
// val a2 : float option = None
let a3 =  areaOpt ( Triangolo(2.5, 3.6) )
// val a3 : float option = Some 4.5
let a4 =  areaOpt ( Triangolo(-2.5, 3.6) )
// val a4 : float option = None

(*
Definire la funzione

    printArea : figura -> string

che  calcola l'area di una figura  e restituisce una stringa col risultato.
Per la conversione da float a string,  usare la funzione string;
se l'area non e' definita, va restituita un opportuno messaggio
*)

let printArea fig = 
    match areaOpt fig with
    |Some a -> "area: " + string a
    |None -> "la figura non e' ben definita"

let as1 = printArea ( Quadrato 10.0 ) 
//val as1 : string = "area: 100"
let as2 = printArea ( Quadrato -10.0 ) 
//val as2 : string = "la figura non e' ben definita"


(*
Definire la funzione

    sommaAreaList : figura list -> float

che, data una lista figs di figure, calcola la somma delle aree delle
figure ben definite contenute in figs.
Se la lista figs non contiene figure ben definite, la somma vale zero.
*)

let rec sommaAreaList figs = 
    match figs with 
    |[] -> 0.0
    |x::xs -> 
        let area = areaOpt x
        match area with 
        |Some a -> a + sommaAreaList xs 
        |None -> sommaAreaList xs 

let ret = Rettangolo (5.0, 6.0)
let quad =  Quadrato 2.0
let tr = Triangolo (2.0, -2.0)

let figs1 = [tr]   // la lista non contiene figure ben definite
let s1 = sommaAreaList figs1
// val s1 : float = 0.0
let figs2 = [ ret ; tr ; quad ; tr ; ret ; quad] 
let s2 = sommaAreaList figs2
// s2 : float = 68.0