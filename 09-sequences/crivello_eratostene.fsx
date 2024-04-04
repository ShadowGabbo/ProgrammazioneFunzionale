// Funzione che dato un numero n e una sequenza sq, cotruisce la sequenza 
// infinita di interi eliminando da sq tutti i multipli di n
let rec sift n sq = 
    seq{
        let head = Seq.head sq 
        let tail = Seq.tail sq
        if head % n <> 0 then yield head 
        yield! sift n tail
    }

let nat = Seq.initInfinite id
let s1 = sift 2 nat |> Seq.take 10 |> Seq.toList
let s2 = sift 3 nat |> Seq.take 15 |> Seq.toList

// Funzione che esegue il crivello di eratostene
let rec sieve sq = 
    seq{
        let x0 = Seq.head sq 
        let sq1 = Seq.tail sq 
        yield x0 
        yield! sieve (sift x0 sq1)
    }

let nat2 = Seq.initInfinite (fun x -> x+2)
let first10primes = sieve nat2 |> Seq.take 10 |> Seq.toList
// let Item1000 = sieve nat2 |> Seq.item 1000

// Poco efficiente, vediamo la versione cached
let siftC n sq = Seq.cache  (sift n sq) 
let rec sieveC sq = 
    seq{
        let x0 = Seq.head sq 
        let sq1 = Seq.tail sq 
        yield x0 
        yield! sieveC (siftC x0 sq1)
    }
let primesC = Seq.cache (sieveC nat2)
let prime = primesC |> Seq.item 1000