module Assignment5.program

(*
Exercise 5.1 The purpose of this exercise is to contrast the F# and Java programming
styles, especially as concerns the handling of lists of elements. The exercise
asks you to write functions that merge two sorted lists of integers, creating a new
sorted list that contains all the elements of the given lists.
(A) Implement an F# function
merge : int list * int list -> int list
that takes two sorted lists of integers and merges them into a sorted list of integers.
For instance, merge ([3;5;12], [2;3;4;7]) should give [2;3;3;4;5;7;12].
*)

let rec merge listA listB acc =
    match listA, listB with
    | [], [] -> acc
    | [], _ -> acc @ listB
    | _, [] -> acc @ listA
    | headA::tailA, headB::tailB ->
        if headA < headB then merge tailA listB (acc @ [headA])
        else merge listA tailB (acc @ [headB])

let mergeResult = merge [3;5;12] [2;3;4;7] []
printfn "mergeResult: %A" mergeResult


// Give me a function that has the following signature:
//(’a -> ’b) -> (’b -> ’c) -> (’a -> ’c)

let compose f g = fun x -> g (f x)

type A = A
type B = B
type C = C
let atob (a:A) = B
let btoc (b:B) = C


let f ac = let abbc ab = let ac = ab ac in ab ac in abbc

