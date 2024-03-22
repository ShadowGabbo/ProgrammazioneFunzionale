(*
Exercises on PBT

Remember your functions:

1.1 remove even numbers from int list
    rmEven : int list -> int list
    
1.2 remove all elements in odd **position** from a list 
    considering the first element an even position.
    rmOdd : rmOdd : 'a list -> 'a list
    
1.3 split a list into two pair of lists consisting of the even and odd positions
    split : 'a list -> 'a list * 'a list

Validate them with FsCheck writing the following properties

  + if I remove even numbers from a list, what's left are odds

  + if I remove the odd positions, the length of the resulting list is
  more or less halved

  + in cases 1.2 and 1.3, the functions do not add "new" elements,
  that is the underlying resulting set is a subset of the starting one
    - Hint for 1.3: define the inverse function of split, say merge and
    show merge (split xs) = xs)

  + check that your def of downto0 corresponds to [n .. -1 .. 0]
     (Hint: exclude the case for n negative)

You can use the function in the List library (example List.length) and
the Set library for example Set.isSubset and Set.ofList;;
*)

#r "nuget:FsCheck"
open FsCheck 

// 1) if I remove even numbers from a list, what's left are odds
let rec rmEven lst = 
    match lst with
    |[]->[]
    |x::xs -> if x%2=0 then rmEven xs else x :: rmEven xs

let ``if remove all even numbers all elements are odd`` xs = 
  rmEven xs = List.filter (fun x -> abs x%2 =1) xs

do Check.Quick ``if remove all even numbers all elements are odd``

// 2) if I remove the odd positions, the length of the resulting list is more or less halved

let rec rmOddPos lst = 
    match lst with 
    |[]->[]
    |[x] -> [x]
    |x::y::xs -> x :: rmOddPos xs

let ``if I remove the odd positions length of the resulting list is more or less halved`` (xs:int list) =
  List.length (rmOddPos xs) - (List.length xs)/2 <= 1

do Check.Quick ``if I remove the odd positions length of the resulting list is more or less halved``

// 3) show -> merge (split xs) = xs
let rec split lst = 
    match lst with
    |[]->([],[])
    |[x]->([x], [])
    |x::y::xs -> 
        let (z, w) = split xs
        (x::z, y::w)

let rec merge lst1 lst2 = 
  match (lst1, lst2) with
  |([],[]) -> []
  |([x],[]) -> [x]
  |([],[x]) -> [x]
  |(x::xs,[]) -> x :: (merge xs [])
  |([],x::xs) -> x :: (merge [] xs)
  |(x::xs,y::ys) -> x :: y :: (merge xs ys)

let ``show that merge split xs is equal to xs`` xs =
  let (a, b) = split xs
  merge a b = xs

do Check.Quick ``show that merge split xs is equal to xs``

// 4) check that your def of downto0 corresponds to [n .. -1 .. 0]
#r "nuget:FsCheck"
open FsCheck 

let rec downTo (num:int) = 
    match num with 
    |0 -> [0]
    |_ -> num :: downTo (num-1)

let ``check that downto is correct`` (num:int) = 
  (num > 0) ==> lazy(downTo num = [num .. -1 .. 0])

do Check.Quick ``check that downto is correct``