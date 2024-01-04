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
  | Bang of string
  | BangN of string * int
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
    | Bang(str) ->
        //explode string to add space between each char
        let rec loop (inputString:string) =
            if inputString.Length = 0 then
                econt ()
            else
                let c = inputString.[0]
                let rest = inputString.Substring(1)
                cont (Str (string c))  (fun () -> loop (rest))
        loop str     
    | BangN(str:string,times:int) ->
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
        
        
    | Fail -> econt ()

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
let ex9 = Every(Write(Prim("<", CstI 4, FromTo(1, 10))));


//Exam 2018Jan
//opg 1
let iconEx1 = Every(Write(Or(FromTo(1, 2), FromTo(3,4))))
// rewrite så værdierne 3 4 3 4 udskrives
let iconEx1Rewritten = Every(Write(And(FromTo(1, 2), FromTo(3,4))))

//opg 2 - Write "I C O N"
let iconEx2 = And(Seq(Write(CstS "I"), Seq(Write(CstS "C"), Seq(Write(CstS "O"), Write(CstS "N")))),CstI 0)
//I C O N val it: value = Int 0

//opg 3 - implement Bang(string)
(*
dotnet fsi Icon.fs
open Icon;;
*)
let iconEx3 = Every(Write(Bang "Icon"));;
//run iconEx3;;

//opg 4 - implement BangN(string, int)
let iconEx4 = Every(Write(BangN("Icon", 2)));;
//run iconEx4;;