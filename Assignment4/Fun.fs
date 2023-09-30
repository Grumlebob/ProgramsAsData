(* File Fun/Fun.fs
   A strict functional language with integers and first-order 
   one-argument functions * sestoft@itu.dk

   Does not support mutually recursive function bindings.

   Performs tail recursion in constant space (because F# does).
*)

module Fun

open Absyn

(* Environment operations *)

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: r -> if x = y then v else lookup r x

(* A runtime value is an integer or a function closure *)

type value =
    | Int of int
    | Closure of string * string list * expr * value env (* (f, x, fBody, fDeclEnv) *)

let rec eval (e: expr) (env: value env) : int =
    match e with
    | CstI i -> i
    | CstB b -> if b then 1 else 0
    | Var x ->
        match lookup env x with
        | Int i -> i
        | _ -> failwith "eval Var"
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env

        match ope with
        | "*" -> i1 * i2
        | "+" -> i1 + i2
        | "-" -> i1 - i2
        | "=" -> if i1 = i2 then 1 else 0
        | "<" -> if i1 < i2 then 1 else 0
        | _ -> failwith ("unknown primitive " + ope)
    | Let(x, eRhs, letBody) ->
        let xVal = Int(eval eRhs env)
        let bodyEnv = (x, xVal) :: env
        eval letBody bodyEnv
    | If(e1, e2, e3) ->
        let b = eval e1 env
        if b <> 0 then eval e2 env else eval e3 env
    | Letfun(f, x, fBody, letBody) ->
        let bodyEnv = (f, Closure(f, x, fBody, env)) :: env
        eval letBody bodyEnv
    | Call(Var f, eArg) ->
        let fClosure = lookup env f

        match fClosure with
        | Closure(f, x, fBody, fDeclEnv) ->
            let keyVal = List.map (fun name -> Int(eval name env)) eArg |> List.zip x
            List.fold (fun bodyenv x -> x::bodyenv) ((f, fClosure) :: fDeclEnv) keyVal
            |> eval fBody
        | _ -> failwith "eval Call: not a function"
    | Call _ -> failwith "eval Call: not first-order function"

(* Evaluate in empty environment: program must have no free variables: *)

let run e = eval e []

(* Examples in abstract syntax *)

let ex1 = Letfun("f1",  ["x"; "b"], Prim("+", Var "x", CstI 1), Call(Var "f1", [CstI 12]))

(* Example: factorial *)

let ex2 =
    Letfun(
        "fac",
        ["x"; "b"],
        If(Prim("=", Var "x", CstI 0), CstI 1, Prim("*", Var "x", Call(Var "fac", [Prim("-", Var "x", CstI 1)]))),
        Call(Var "fac", [Var "n"])
    )

(* let fac10 = eval ex2 [("n", Int 10)];; *)

(* Example: deep recursion to check for constant-space tail recursion *)