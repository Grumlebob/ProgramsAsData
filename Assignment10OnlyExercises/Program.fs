
//1.1
let rec len xs =
    match xs with
    | [] -> 0
    | x :: xr -> 1 + len xr

//1.1(i)
let rec lenc xs c =
        match xs with
        | [] -> c 0
        | _ :: tail -> lenc tail (fun value -> c (1 + value))

let result1_1_i = lenc [ 2; 5; 7 ] (fun x -> x)
printf $"1.1(i) answer is %d{result1_1_i}\n"

//1.1(ii): What happens if you call it as lenc xs (fun v -> 2*v) instead?
let result1_1_ii = lenc [ 2; 5; 7 ] (fun x -> x * 2)
printf $"1.1(ii) answer is %d{result1_1_ii} \n"

//1.1(iii)
let rec leni xs acc =
        match xs with
        | [] -> acc
        | _ :: xr -> leni xr (acc+1)
let result1_1_iii = leni [ 2; 5; 7 ] 0
printf $"1.1(iii) answer is %d{result1_1_iii} \n"

//1.2
(*
First iteration:
[1,2,3]
[2,3]@[1]
*)
let rec rev xs =
    match xs with
    | [] -> []
    | x :: xr -> rev xr @ [ x ]


//1.2(i)
let rec revc xs c =
        match xs with
        | [] -> c []
        | x :: xr ->
            revc xr (fun value -> c (value @ [ x ]))

let result1_2_i = revc [1;2;3] id
printf $"1.2(i) answer is %A{result1_2_i} \n"

//1.2(ii): What happens if you call it as revc xs (fun v -> v @ v) instead?

let result1_2_ii = revc [1;2;3] (fun v -> v@v)
printf $"1.2(ii) answer is %A{result1_2_ii} \n"

//1.2(iii)
let rec revi xs acc =
        match xs with
        | [] -> acc
        | x :: xr -> revi xr ( x ::acc)

let result1_2_iii = revi [1;2;3] []
printf $"1.2(iii) answer is %A{result1_2_iii} \n"


//1.3
let rec prod xs =
    match xs with
    | [] -> 1
    | x :: xr -> x * prod xr
//1.3(i) (se slides)
let rec prodc xs c =
        match xs with
        | [] -> c 1
        | x :: xr -> prodc xr (fun value -> c (x*value))

let result1_3_i = prodc [1;2;3] id
printf $"1.3(i) answer is %d{result1_3_i} \n"


//1.4
let rec prodi xs c =
        match xs with
        | [] -> c 1
        | x :: xr ->
                if x = 0 then 0
                else prodi xr (fun value -> c (x*value))

let result1_4_NoZero = prodi [1;2;3] id
printf $"1.4 answer with no zero is %d{result1_4_NoZero} \n"
let result1_4_WithZero = prodi [1;2;3;0;5] id
printf $"1.4 answer with zero is %d{result1_4_WithZero} \n"


//1.8
open Icon
run (Every(Write(Prim("*", CstI 2, FromTo(1, 4)))));;
printf "\n"
//1.8(i)
//Write an expression that produces and prints the values 3 5 7 9.
printf "1.8.i_1: \n"
run (Every(Write(Prim("+",Prim("*", CstI 2, FromTo(1, 4)),CstI 1))));;
printf "\n1.8.i_2: \n"
//Write an expression that produces and prints the values 21 22 31 32 41 42.
//Inner function: 20,30,40
//Outer function: 20+1 20+2 30+1 30+2 40+1 40+2
run (Every(Write(Prim("+",Prim("*", CstI 10, FromTo(2, 4)),FromTo(1,2)))));;
//1.8(ii): write an expression that prints the least multiple of 7 that is greater than 50.
//print 56, 63, 70, 77, 84, 91, 98, 105, 112, 119, 126, ...
printf "\n1.8.ii: \n"
run (Write(Prim("<",CstI 50,Prim("*", CstI 7, FromTo(1, 50)))));;

//1.8(iii)
(*
(a) define a primitive sqr that computes the square x · x of its argument x; 

(b) define a primitive even that fails if its argument is odd, and succeeds if it is
even (producing the argument as result). For instance, square(3 to 6) should
succeed four times, with the results 9, 16, 25, 36, and even(1 to 7) should succeed
three times with the results 2, 4, 6.
*)
printf "\n1.8.iii_a: \n"
run (Every(Write(Prim1("square", FromTo(3, 6)))));;

printf "\n1.8.iii_b: \n"
run (Every(Write(Prim1("even", FromTo(1, 7) ))));;


//1.8(iv):
//Define a unary primitive multiples that succeeds infinitely many times, producing
//all multiples of its argument. For instance, multiples(3) should produce 3, 6, 9

//We chose to set an arbitrary limit of 100, to let the function terminate neatly and show results:
printf "\n1.8.iv: \n"
run (Every(Write(Prim1("multiples", CstI 3))));;
