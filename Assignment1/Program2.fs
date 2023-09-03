module Assignment1.Two 

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
type expr =
    | CstI of int
    | Var of string
    | Let of (string * expr) list * expr // (string * exp) list of x = 1, y = 2, z = 3. expr is the body, such as x + y + z
    | Prim of string * expr * expr

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r -> if x = y then v else lookup r x

let rec eval e (env: (string * int) list) : int =
    match e with
    | CstI i -> i
    | Var x -> lookup env x
    | Let(assigns, ebody) -> //ebody, evaluate last. assigns, evaluate first
        let env1 = List.fold (fun acc (x, erh) -> (x, eval erh env) :: acc) env assigns
        eval ebody env1
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim _ -> failwith "unknown primitive"

let rec mem x vs = //is x in vs
    match vs with
    | []      -> false
    | v :: vr -> x=v || mem x vr;;

let rec union (xs, ys) = 
    match xs with 
    | []    -> ys
    | x::xr -> if mem x ys then union(xr, ys)
               else x :: union(xr, ys);;
let rec minus (xs, ys) = 
    match xs with 
    | []    -> []
    | x::xr -> if mem x ys then minus(xr, ys)
               else x :: minus (xr, ys);;
               
let rec freevars e : string list =
    match e with
    | CstI _ -> []
    | Var x  -> [x]
    | Let(assigns, ebody) ->
          let freeInAssigns = List.fold (fun acc (_, erh) -> union (freevars erh, acc)) [] assigns
          let assigned = List.fold (fun acc (x, _) -> x :: acc) [] assigns
          union (freeInAssigns, minus (freevars ebody, assigned))
    | Prim(_, e1, e2) -> union (freevars e1, freevars e2);;
    
    
type texpr =                            (* target expressions *)
  | TCstI of int
  | TVar of int                         (* ind
  ex into runtime environment *)
  | TLet of texpr * texpr               (* erhs //variables assigned to expressions and ebody //evaluate body, with new environment, that we now know from after erhs compiled.
                                        TLet(tcomp erhs cenv, tcomp ebody cenv1) Left side: erhs: x = 1. Right side: ebody: y+x              *)
  | TPrim of string * texpr * texpr;;


(* Map variable name to variable index at compile-time *)

let rec getindex vs x = 
    match vs with 
    | []    -> failwith ("Variable not found " + x)
    | y::yr -> if x=y then 0 else 1 + getindex yr x;;

(* Compiling from expr to texpr *)


let rec tcomp (e : expr) (cenv : string list) : texpr = //tcomp 
    match e with
    | CstI i -> TCstI i
    | Var x  -> TVar (getindex cenv x)
    | Let(assigns, ebody) ->
            (*
               assigns: x = 1, y = x, ebody: y+x
                                                          tcomp (expr: y+x) (cenv: [y, x])  1. 
                                    TLet(tcomp y = x [y], tcomp y+x [y, x])                 2. 
               TLet(tcomp x = 1 [], TLet(tcomp y = x [y], tcomp y+x [y, x])                 3. 
               
               Without Tlet:
               
                                (y + x) [y, x]
                            (y = x (x + y) [x]
                    (x = 1 (y = x (x + y)) [ ]


For example, given 'assigns' as 'x = 1' and 'y = x', and 'ebody' as 'y + x':

Given assignments: x = 1, y = x
Given expression: y + x

Initially, the context 'cenv' is [y, x].

We start building the target expression from the innermost assignment:

1. Translate 'y + x' with 'cenv' as [y, x]:
   Result: TLet(Tcomp y = x [y], tcomp y + x [y, x])

2. Continue with the next assignment 'x = 1':
   Result: TLet(Tcomp x = 1 [], TLet(Tcomp y = x [y], tcomp y + x [y, x]))

The final translated expression preserves the variable bindings:

TLet(Tcomp x = 1 [], TLet(Tcomp y = x [y], tcomp y + x [y, x]))


        Discard because:
        In the target language, the TLet construct is used to represent variable assignments, 
        but once the assignments have been translated and applied, there is no need to keep the outermost TLet in the expression.

         This translation strategy preserves the lexical scoping of variables.


For each assignment (x, erh) in assigns, you do the following:
Tcomp erh env.Tail translates the right-hand side of the assignment (erh) with the current environment, excluding the variable x introduced by the assignment. This ensures that the assignment's scope is correctly maintained.
TLet(tcomp erh env.Tail, exp) wraps the translation of the right-hand side in a TLet with the current environment and combines it with the accumulated expression exp. This step preserves the scoping of the assignment and the subsequent expressions.
(env.Tail, TLet(..., exp)) updates the environment by removing the variable x from it.
         
      *)
            *)
            
            let cenv1 = List.fold (fun acc (x, _) -> x :: acc) cenv assigns //add all vars to env
            List.fold (fun (env : string list, exp) (_, erh) -> (env.Tail, TLet(tcomp erh env.Tail, exp))) (cenv1, tcomp ebody cenv1) (List.rev assigns) //start from the inner most TLet
            |> snd
    | Prim(ope, e1, e2) -> TPrim(ope, tcomp e1 cenv, tcomp e2 cenv)
    
    
let rec teval (e : texpr) (renv : int list) : int =
    match e with
    | TCstI i -> i
    | TVar n  -> List.item n renv
    | TLet(erhs, ebody) -> 
      let xval = teval erhs renv
      let renv1 = xval :: renv 
      teval ebody renv1 
    | TPrim("+", e1, e2) -> teval e1 renv + teval e2 renv
    | TPrim("*", e1, e2) -> teval e1 renv * teval e2 renv
    | TPrim("-", e1, e2) -> teval e1 renv - teval e2 renv
    | TPrim _            -> failwith "unknown primitive";;
    
    
    
let test = teval (tcomp (Let([("x", CstI 1); ("y", Var "x")], Prim("+", Var "y", Var "x"))) []) [];;

printfn "%A" test
