(*
    Definire la somma di una lista usando la funzione universale "fold"
*)
let sumList xs = List.foldBack (fun x acc -> acc + x) xs 0
let s1 = sumList [1..10]
let s2 = sumList [1..5]
let s3 = sumList [1..3]

(*
    Definire il prodotto degli elementi di una lista usando la funzione universale "fold"
*)
let prodList xs = List.foldBack (fun x acc -> acc * x) xs 1
let p1 = prodList [1..10]
let p2 = prodList [1..5]
let p3 = prodList [1..3]

(*
    Definire la lunghezza di una lista usando la funzione universale "fold"
*)
let lenList xs = List.foldBack (fun _ acc -> acc + 1) xs 0
let l1 = lenList [1..10]
let l2 = lenList [1..5]
let l3 = lenList [1..3]

(*
    Definire la funzione append usando la funzione universale "fold"
*)
let appendList xs ys = List.foldBack (fun x acc -> x :: acc) ys xs
let a1 = appendList [1..3] [5..9]