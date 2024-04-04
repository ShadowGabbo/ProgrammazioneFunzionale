// f : float -> int -> float
// f x k = t(x,k) | t(x,k) :=  x^k / k!
let rec f x k =
    if k = 0 then
        1.0
    else
        (x / (float k)) * f x (k - 1)

// sumSeq : sq:seq<int> -> seq<int>
// Si *assume* che sq sia infinita
// sequenza delle somme di una sequenza
let sumSeq sq =
    let rec helper sq2 sum =
        seq{
            let head = Seq.head sq2
            let tail = Seq.tail sq2
            yield float(head + sum)
            yield! helper tail (sum+head)
        } 
    helper sq 0.0

// apprTaylor : x -> seq<float>
// costruisce la sequenza infinita t(x,0) , t(x,0) + t(x,1) ,  t(x,0) + t(x,1) + t(x,2) , ....     
// corrisponente alla sequenza infinita delle approssimazioni di exp(x)
let apprTaylor x = 
    let rec helper x k = 
        seq{
            yield f x k 
            yield! helper x (k+1)
        }
    sumSeq (helper x 0)

let a1 = apprTaylor 1.0 |> Seq.take 10 |> Seq.toList

// apprExp : float -> float -> float
// dato x:float e delta:float, calcola e^x con approssimazione delta
// va costruita la sequenza infinita  'apprTaylor x' delle approssimazioni di exp(x)
let apprExp x delta = 
    let appross_taylor = apprTaylor x 
    let rec helper sq =
        let primo = Seq.head sq
        let secondo = Seq.head (Seq.tail sq) 

        if abs (secondo - primo) >= delta then
            helper (Seq.tail sq) 
        else
            primo
    helper appross_taylor

let ae1 = apprExp 1.0 0.01  
// val it : float = 2.708333333

let ae2 = apprExp 1.0 0.0001 
// val it : float = 2.718253968

let ae3 = apprExp 1.0 0.0000001 
// val it : float = 2.718281801

let ae4 = apprExp 2.5 0.0000001 
// val it : float = 12.18249394