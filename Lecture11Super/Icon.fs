(* File Cont/Icon.fs 

   Abstract syntax and interpreter for micro-Icon, a language where an 
   expression can produce more than one result.  

   sestoft@itu.dk * 2010-05-18

   ---

   For a description of micro-Icon, see Chapter 11: Continuations, in
   Programming Language Concepts for Software Developers.

   As described there, the interpreter (eval e cont econt) has two
   continuations:

      * a success continuation cont, that is called on the result v of
        the expression e, in case it has one;

      * a failure continuation econt, that is called on () in case the
        expression e has no result.
 *)

module Icon

(* Micro-Icon abstract syntax *)

type expr = 
  | CstI of int
  | CstS of string
  | FromTo of int * int
  | Write of expr
  | If of expr * expr * expr
  | Prim of string * expr * expr 
  | And of expr * expr
  | Or  of expr * expr
  | Seq of expr * expr
  | Every of expr
  | Bang of string //2018 exam
  | BangN of string * int //2018 exam
  | Find of string * string //2019 exam
  | Random of int * int * int //2022 exam Random(min,max,num) inclusive
  | FromToBy of int * int * int //2017 exam (start, end, increment)
  | RandomFromList of int * int list //2024 exam (numbersToGenerator, listOfNumbersToGeneratorFrom)
  | Fail;;

(* Runtime values and runtime continuations *)

type value = 
  | Int of int
  | Str of string;;

type econt = unit -> value;;

type cont = value -> econt -> value;;

(* Print to console *)

let write v =
    match v with 
    | Int i -> printf "%d " i
    | Str s -> printf "%s " s;;

(* Expression evaluation with backtracking *)

let rec eval (e : expr) (cont : cont) (econt : econt) = 
    match e with
    | CstI i -> cont (Int i) econt
    | CstS s  -> cont (Str s) econt
    | FromTo(i1, i2) -> 
      let rec loop i = 
          if i <= i2 then 
              cont (Int i) (fun () -> loop (i+1))
          else 
              econt ()
      loop i1
    | Write e -> 
      eval e (fun v -> fun econt1 -> (write v; cont v econt1)) econt
    | If(e1, e2, e3) -> 
      eval e1 (fun _ -> fun _ -> eval e2 cont econt)
              (fun () -> eval e3 cont econt)
    | Prim(ope, e1, e2) -> 
      eval e1 (fun v1 -> fun econt1 ->
          eval e2 (fun v2 -> fun econt2 -> 
              match (ope, v1, v2) with
              | ("+", Int i1, Int i2) -> 
                  cont (Int(i1+i2)) econt2 
              | ("*", Int i1, Int i2) -> 
                  cont (Int(i1*i2)) econt2
              | ("<", Int i1, Int i2) -> 
                  if i1<i2 then 
                      cont (Int i2) econt2
                  else
                      econt2 ()
              | _ -> Str "unknown prim2")
              econt1)
          econt
    | And(e1, e2) -> 
      eval e1 (fun _ -> fun econt1 -> eval e2 cont econt1) econt
    | Or(e1, e2) -> 
      eval e1 cont (fun () -> eval e2 cont econt)
    | Seq(e1, e2) -> 
      eval e1 (fun _ -> fun econt1 -> eval e2 cont econt)
              (fun () -> eval e2 cont econt)
    | Every e -> 
      eval e (fun _ -> fun econt1 -> econt1 ())
             (fun () -> cont (Int 0) econt)
    | Fail -> econt ()
    | Find (patternToMatch, inputString )->
        (*let str = "Hi there - if there are anyone"
        > val str : string = "Hi there - if there are anyone"
        run (Every(Write(Find("there",str))))
        > 3 14 val it : value = Int 0*)
        let rec loop (currentIndex:int) =
            let foundIndex = inputString.IndexOf(patternToMatch,currentIndex)
            if foundIndex = -1 then
                econt ()
            else
                cont (Int foundIndex)  (fun () -> loop (foundIndex+1))
        loop 0
    | Bang(str) -> //2018 exam
        //explode string to add space between each char
        let rec loop (inputString:string) =
            if inputString.Length = 0 then
                econt ()
            else
                let c = inputString.[0]
                let rest = inputString.Substring(1)
                cont (Str (string c))  (fun () -> loop (rest))
        loop str     
    | BangN(str:string,times:int) -> //2018 exam
        //do Bang n times
        let rec outerLoop (inputString:string) (times:int) =
            
            if times = 0 then
                econt ()
            else
                let rec loop (inputString:string) =
                    if inputString.Length = 0 then
                        outerLoop str (times-1)
                    else
                        let c = inputString.[0]
                        let rest = inputString.Substring(1)
                        cont (Str (string c))  (fun () -> loop (rest))
                loop str
        outerLoop str times
    | Random(min,max,num) -> //2022 exam
        let random = new System.Random();
        let randomNext(min, max) = random.Next(min,max+1) // max is exclusive in Next.
        let rec loop (num:int) =
            if num = 0 then
                econt ()
            else
                let randomInt = randomNext(min,max)
                cont (Int randomInt)  (fun () -> loop (num-1))
        loop num
    | FromToBy(startnum, endnum, increment) -> //2017 exam
        let rec loop (currentnum:int) =
            if currentnum > endnum then
                econt ()
            else
                cont (Int currentnum)  (fun () -> loop (currentnum+increment))
        loop startnum
    | RandomFromList(numbersToGenerator, listOfNumbersToGeneratorFrom) -> //2024 exam
        //Fail if N is less than 0 or list is empty
        if numbersToGenerator < 0 || listOfNumbersToGeneratorFrom = [] then
            econt ()
        else
            let random = new System.Random()
            //Max is exclusive, and we want inclusive
            let randomNext(min, max) = random.Next(min,max+1)
            
            let rec loop (num:int) =
                if num = 0 then
                    econt () //No more results left
                else
                    //Get random index from list.
                    let randomIndex = randomNext(0,listOfNumbersToGeneratorFrom.Length-1)
                    let randomIntFromList = listOfNumbersToGeneratorFrom.[randomIndex]
                    //Accept random number, and repeat loop with one less num
                    cont (Int randomIntFromList)  (fun () -> loop (num-1))
            loop numbersToGenerator //Start loop with N, decrement each loop and repeat until N is 0
        

let run e = eval e (fun v -> fun _ -> v) (fun () -> (printfn "Failed"; Int 0));


(* Examples in abstract syntax *)

// (write(1 to 3)) ; fail
let ex1 = Seq(Write (FromTo(1, 3)), Fail);

// (write(1 to 3)) & fail
let ex2 = And(Write (FromTo(1, 3)), Fail);

// (write((1 to 3) & (4 to 6))) & fail
let ex3and = And(Write(And(FromTo(1, 3), FromTo(4, 6))), Fail);

// (write((1 to 3) | (4 to 6))) & fail
let ex3or  = And(Write(Or(FromTo(1, 3), FromTo(4, 6))), Fail);

// (write((1 to 3) ; (4 to 6))) & fail
let ex3seq = And(Write(Seq(FromTo(1, 3), FromTo(4, 6))), Fail);

// write((1 to 3) & ((4 to 6) & "found"))
let ex4 = Write(And(FromTo(1, 3), And(FromTo(4, 6), CstS "found")));

// every(write(1 to 3))
let ex5 = Every (Write (FromTo(1, 3)));

// (every(write(1 to 3)) & (4 to 6))
let ex6 = And(Every (Write (FromTo(1, 3))), FromTo(4, 6));

// every(write((1 to 3) + (4 to 6)))
let ex7 = Every(Write(Prim("+", FromTo(1,3), FromTo(4, 6))));

// write(4 < (1 to 10))
let ex8 = Write(Prim("<", CstI 4, FromTo(1, 10)));

// every(write(4 < (1 to 10)))
let ex9 = Every(Write(Prim("<", CstI 4, FromTo(1, 10))))


//Exam 2018Jan
//opg 1
let iconEx1_2018 = Every(Write(Or(FromTo(1, 2), FromTo(3,4))))
// rewrite så værdierne 3 4 3 4 udskrives
let iconEx1Rewritten_2018 = Every(Write(And(FromTo(1, 2), FromTo(3,4))))

//opg 2 - Write "I C O N"
let iconEx2_2018 = And(Seq(Write(CstS "I"), Seq(Write(CstS "C"), Seq(Write(CstS "O"), Write(CstS "N")))),CstI 0)
//I C O N val it: value = Int 0

//opg 3 - implement Bang(string)
(*
dotnet fsi Icon.fs
open Icon;;
*)
let iconEx3_2018 = Every(Write(Bang "Icon"));;
//run iconEx3;;
//opg 4 - implement BangN(string, int)
let iconEx4_2018 = Every(Write(BangN("Icon", 2)));;
//run iconEx4;;

(*
(1,2,3,4) & (1,2,3,4)
1: 1
1: 2
1: 3
1: 4
2: 1
...
4: 4

(1,2,3,4) * (1,2,3,4)
1: 1*1
1: 1*2
1: 1*3
1: 1*4
2: 2*1
...
4: 4*4

*)

//Exam 2019 opg 1:
let iconEx1_2019 = Write(Prim("<",CstI 7,FromTo(1,10)))
//Rewritten to write 8 9 10
let iconEx1Rewritten_2019 = Every(Write(Prim("<",CstI 7,FromTo(1,10))))

//Exam 2019 opg 2:
let iconEx2_2019 = Every(Write(And(FromTo(1,4), And(Write (CstS "\n"),FromTo(1,4)))))
//rewritten to write 1,2,3,4 .....4 8 12 16
let iconEx2Rewritten_2019 = Every(Write(Prim("*", FromTo(1,4), And(Write (CstS "\n"),FromTo(1,4)))))

//Exam 2022 Opg 1: 1 2 3 4 5 6 7 8 9 10
let iconEx1_2022 = Every(Write(FromTo(1,10)))

//Exam 2022 Opg 2: 10 tabellen
let iconEx2_2022 = Every(Write(Prim("*",FromTo(1,10),FromTo(1,10))))

//Exam 2022 opg3: 10 tabellen på ny linje:
let iconEx3_2022 = Every(Write(Prim("*", FromTo(1,10), And(Write (CstS "\n"),FromTo(1,10)))))

//Exam 2022 opg4: Random
let iconEx4_2022 = (Every(Write(Random(1,10,3))));;


//Write an expression that produces and prints the values 21 22 31 32 41 42.
//Inner function: 20,30,40
//Outer function: 20+1 20+2 30+1 30+2 40+1 40+2
let iconEx0_2017 = Every(Write(Prim("+",Prim("*", CstI 10, FromTo(2, 4)),FromTo(1,2))));;

//exam 2017 write expression to get 33 34 43 44 53 54 63 64
//Inner function: (3*10,4*10,5*10,6*10): 30,40,50,60
//Outer function: 30+3, 30+4, 40+3, 40+4, 50+3, 50+4, 60+3, 60+4
let iconEx1_2017 = Every(Write(Prim("+",Prim("*", CstI 10, FromTo(3, 6)),FromTo(3,4))));;

//Exam 2017 FromToBy(s,e,i)
let iconEx2_2017 = Every(Write(FromToBy(1,10,3)));;

//Exam 2017 FromToBy(s,e,i) getting values 33 34 43 44 53 54 63 64
let iconEx3_2017 = Every(Write(Prim("+",FromToBy(33,70,10),FromTo(0,1))));;

//Exam 2017 FromToBy uendeligt
let iconEx4_2017 = Every(Write(FromToBy(10, 11, 0)));;

//Assignment 10  write an expression that prints the least multiple of 7 that is greater than 50.
//print 56, 63, 70, 77, 84, 91, 98, 105, 112, 119, 126, ...
let iconEx10 = Write(Prim("<",CstI 50,Prim("*", CstI 7, FromTo(1, 50))));;


//Exam 2024
//opg 1 1. Skriv et Icon udtryk, som udskriver værdierne 5 6 7 8 9 10 11 12 på skærmen:
(*
dotnet fsi Icon.fs
open Icon;;
*)
let iconEx1_2024 = Every(Write(FromTo(5,12)));;

//påg 1 2: Skriv et Icon udtryk, som udskriver alle tal n mellem 3 og 60, hvor 3 går op i tallet n:
let iconEx2_2024 = Every(Write(Prim("*", CstI 3,FromTo(1,20))));;

//opg 1 3: 3. Skriv et Icon udtryk, som udskriver alle tal n mellem 4 og 61, hvor 3 går op i n − 1:
let iconEx3_2024 = Every(Write(Prim("+", CstI 1 ,Prim("*", CstI 3,FromTo(1,20)))));;