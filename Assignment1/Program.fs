
//1.1,
(*
Exercise 1.1 (i) File Intro2.fs contains a definition of the expr expression
language and an evaluation function eval. Extend the eval function to handle
three additional operators: "max", "min", and "==". Like the existing operators,
they take two argument expressions. The equals operator should return 1 when true
and 0 when false.
(ii) Write some example expressions in this extended expression language, using
abstract syntax, and evaluate them using your new eval function.
(iii) Rewrite one of the eval functions to evaluate the arguments of a primitive
before branching out on the operator, in this style:
| ...
(iv) Extend the expression language with conditional expressions If(e1, e2,
e3) corresponding to Java’s expression e1 ? e2 : e3 or F#’s conditional
expression if e1 then e2 else e3. You need to extend the expr datatype with a new constructor If that takes three
expr arguments.
(v) Extend the interpreter function eval correspondingly. It should evaluate e1, and
if e1 is non-zero, then evaluate e2, else evaluate e3. You should be able to evaluate
the expression If(Var "a", CstI 11, CstI 22) in an environment that
binds variable a.
Note that various strange and non-standard interpretations of the conditional
expression are possible. For instance, the interpreter might start by testing whether
expressions e2 and e3 are syntactically identical, in which case there is no need to
evaluate e1, only e2 (or e3). Although possible, this shortcut is rarely useful.
*)
let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "max" -> max i1 i2
        | "min" -> min i1 i2
        | "==" -> if i1 = i2 then 1 else 0
    | If(e1, e2, e3)       ->
        let boole1 = eval e1 env
        if boole1 > 0 then eval e2 env else eval e3 env



let example1 = eval (Prim("max", CstI 11, CstI 10)) []
let example2 = eval (Prim("min", CstI 11, CstI 10)) []
let example3 = eval (Prim("==", CstI 11, CstI 10)) []
let example4 = eval (If(Var "a", CstI 11, CstI 22)) [("a", 1)]

//1.2,
(*
Exercise 1.2 (i) Declare an alternative datatype aexpr for a representation of arithmetic
expressions without let-bindings. The datatype should have constructorsCstI,
Var, Add, Mul, Sub, for constants, variables, addition, multiplication, and subtraction.
Then x ∗ (y + 3) is represented as Mul(Var "x", Add(Var "y", CstI
3)), not as Prim("*", Var "x", Prim("+", Var "y", CstI 3)).
(ii) Write the representation of the expressions v − (w + z) and 2 ∗ (v − (w + z))
and x + y + z + v.
(iii) Write an F# function fmt : aexpr -> string to format expressions
as strings. For instance, it may format Sub(Var "x", CstI 34) as the string
"(x - 34)". It has very much the same structure as an eval function, but takes no
environment argument (because the name of a variable is independent of its value).
(iv) Write an F# function simplify : aexpr -> aexpr to perform expression
simplification. For instance, it should simplify (x + 0) to x, and simplify (1 + 0)
to 1. The more ambitious student may want to simplify (1 + 0) ∗ (x + 0) to x. Hint:
Pattern matching is your friend.
(v) Write an F# function to perform symbolic differentiation of simple arithmetic
expressions (such as aexpr) with respect to a single variable.
*)


type aexpr = 
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr
  
// v − (w + z)
let firstaxpr = Sub(Var "v", Add(Var "w", Var "z"))

// 2 ∗ (v − (w + z))
let secondaxpr = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))

// x + y + z + v.

let thirdaxpr = Add(Add(Add(Var "x", Var "y"), Var "z"), Var "v")


let rec fmt (exp : aexpr) : string =
        match exp with 
            | CstI i -> string i
            | Var x -> x
            | Add (e1, e2) -> "(" + fmt e1 + "+" + fmt e2 + ")"
            | Sub (e1, e2) -> "(" + fmt e1 + "-" + fmt e2 + ")"
            | Mul (e1, e2) -> "(" + fmt e1 + "*" + fmt e2 + ")"
            
let fmtexample = fmt thirdaxpr


let rec simplify (aexpr: aexpr) : aexpr =
    match aexpr with
    | CstI i -> CstI i
    | Var x -> Var x
    | Mul(e1, e2) ->
        let s1 = simplify e1
        let s2 = simplify e2
        match s1, s2 with
        | CstI 0, _ -> CstI 0
        | _, CstI 0-> CstI 0
        | CstI 1, _ -> s2
        | _, CstI 1 -> s1
        | _ -> Mul(s1, s2)
    | Add(e1, e2) ->
        let s1 = simplify e1
        let s2 = simplify e2
        match s1, s2 with
        | CstI 0, _ -> s2
        | _, CstI 0 -> s1
        | _ -> Add(s1, s2)
    | Sub(e1, e2) ->
        let s1 = simplify e1
        let s2 = simplify e2
        if (s1 = s2) then CstI 0
        else 
            match s1, s2 with
            | _, CstI 0 -> s1
            | _ -> Sub(s1, s2)
        
let example = simplify (Mul(Add(CstI 0, Var "x"), Sub(CstI 1, Sub(CstI 3, CstI 3))))

//Write an F# function to perform symbolic differentiation of simple arithmetic expressions (such as aexpr) with respect to a single variable.

let rec diff (aexpr: aexpr) : aexpr =
    match aexpr with
    | CstI _ -> CstI 0
    | Var x -> if x = "x" then CstI 1 else CstI 0
    | Mul(e1, e2) ->
        Add(Mul(diff e1, e2), Mul(e1, diff e2))
    | Add(e1, e2) ->
        Add(diff e1, diff e2)
    | Sub(e1, e2) ->
        Sub(diff e1, diff e2)

let exampleDiff = fmt (diff(Add(Mul(Var "x", Var "y"), Sub(Var "x", CstI 4))))
