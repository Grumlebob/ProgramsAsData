
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
let rec eval e (env : (string * int) list) : int =
match e with
| ...
| Prim(ope, e1, e2) ->
let i1 = ...
let i2 = ...
match ope with
| "+" -> i1 + i2
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
    | Prim _            -> failwith "unknown primitive";;


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
Pattern matching is your friend. Hint: Don’t forget the case where you cannot simplify
anything.
You might consider the following simplifications, plus any others you find useful
and correct:
0 + e −→ e
e + 0 −→ e
e − 0 −→ e
1 ∗ e −→ e
e ∗ 1 −→ e
0 ∗ e −→ 0
e ∗ 0 −→ 0
e − e −→ 0
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
//1.4,

(*
Exercise 1.4 This chapter has shown how to represent abstract syntax in functional
languages such as F# (using algebraic datatypes) and in object-oriented languages
such as Java or C# (using a class hierarchy and composites).
(i) Use Java or C# classes and methods to do what we have done using the F# datatype
aexpr in the preceding exercises. Design a class hierarchy to represent arithmetic
expressions: it could have an abstract class Expr with subclasses CstI, Var, and
Binop, where the latter is itself abstract and has concrete subclasses Add, Mul
and Sub. All classes should implement the toString() method to format an
expression as a String.
The classes may be used to build an expression in abstract syntax, and then print
it, as follows:
Expr e = new Add(new CstI(17), new Var("z"));
System.out.println(e.toString());
(ii) Create three more expressions in abstract syntax and print them.
(iii) Extend your classes with facilities to evaluate the arithmetic expressions, that
is, add a method int eval(env).
(iv) Add a method Expr simplify() that returns a new expression where algebraic
simplifications have been performed, as in part (iv) of Exercise 1.2.
*)




//2.1,

(*

Exercise 2.1 Extend the expression language expr from Intcomp1.fs with
multiple sequential let-bindings, such as this (in concrete syntax):
let x1 = 5+7 x2 = x1*2 in x1+x2 end
To evaluate this, the right-hand side expression 5+7 must be evaluated and bound
to x1, and then x1*2 must be evaluated and bound to x2, after which the let-body
x1+x2 is evaluated.
The new abstract syntax for expr might be
type expr =
| CstI of int
| Var of string
| Let of (string * expr) list * expr (* CHANGED *)
| Prim of string * expr * expr

so that the Let constructor takes a list of bindings, where a binding is a pair of a
variable name and an expression. The example above would be represented as:
Let ([("x1", ...); ("x2", ...)], Prim("+", Var "x1", Var "x2"))
Revise the eval interpreter from Intcomp1.fs to work for the expr language
extended with multiple sequential let-bindings.
*)


//2.2,

(*
Exercise 2.2 Revise the function freevars : expr -> string list to
work for the language as extended in Exercise 2.1. Note that the example expression
in the beginning of Exercise 2.1 has no free variables, but let x1 = x1+7 in
x1+8 end has the free variable x1, because the variable x1 is bound only in the
body (x1+8), not in the right-hand side (x1+7), of its own binding. There are
programming languages where a variable can be used in the right-hand side of its
own binding, but ours is not such a language.

*)


//2.3

(*
Exercise 2.3 Revise the expr-to-texpr compiler tcomp : expr -> texpr
from Intcomp1.fs to work for the extended expr language. There is no need
to modify the texpr language or the teval interpreter to accommodate multiple
sequential let-bindings.
*)


//(optionally also 2.6).

(*
Exercise 2.6 Now modify the interpretation of the language from Exercise 2.1 so
that multiple let-bindings are simultaneous rather than sequential. For instance,
let x1 = 5+7 x2 = x1*2 in x1+x2 end
should still have the abstract syntax
Let ([("x1", ...); ("x2", ...)], Prim("+", Var "x1", Var "x2"))
but now the interpretation is that all right-hand sides must be evaluated before any
left-hand side variable gets bound to its right-hand side value. That is, in the above
expression, the occurrence of x1 in the right-hand side of x2 has nothing to do with
the x1 of the first binding; it is a free variable.
Revise the eval interpreter to work for this version of the expr language. The
idea is that all the right-hand side expressions should be evaluated, after which all
the variables are bound to those values simultaneously. Hence
let x = 11 in let x = 22 y = x+1 in x+y end end
should compute 12 + 22 because x in x+1 is the outer x (and hence is 11), and x
in x+y is the inner x (and hence is 22). In other words, in the let-binding
let x1 = e1 ... xn = en in e end
the scope of the variables x1 . . . xn should be e, not e1 . . . en.
Exercise 2.7 Define a version of the (naive, exponential-time) Fibonacci
*)
let test = Assignment1.Two.teval (Assignment1.Two.tcomp (Assignment1.Two.Let([("x", Assignment1.Two.CstI 1); ("y", Assignment1.Two.Var "x")],Assignment1.Two.Prim("+", Assignment1.Two.Var "y", Assignment1.Two.Var "x"))) []) [];;

//print test
printfn "%A" test