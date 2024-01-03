(* Fun/Absyn.fs * Abstract syntax for micro-ML, a functional language *)

module Absyn

type expr = 
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr         (* | Let(x, eRhs, letBody) ->  *)
  | Prim of string * expr * expr           (* (op, e1, e2) *)
  | If of expr * expr * expr              (* (test, then, else) *)
  | Letfun of string * string * expr * expr    (* Create a function closure and bind f to it (f, x, fBody, letBody) - let f x= x + 7 in f 2 end = Letfun("f", "x", Prim("+", Var "x", CstI 7), Call (Var "f", CstI 2)) *)
  | Call of expr * expr                   (* Lookup closure f, evaluate args, create env and evaluate fBody (f, arg) *)
  //other assignments
  | LetfunList of string * string list * expr * expr    (* Assignment 4 (f, x list, fBody, letBody) *)
  | CallList of expr *  expr list                       (* Assignment 4 (var functionname, expr arguments)*)
  | Fun of string * expr                                (* Assignment 5 - fun x -> x+2 (x, body) *)
  | Print of expr                                       (* 2017Jan - print (expr) *)
  | Set of expr list                                    (* 2022Jan *)
  | InCheck of expr * expr * expr                       (* 2022Jan *)
  | Field of expr * string                    (* Jan2019 (e record, s fieldvariable) *)
  | Record of (string * expr) list          (*  Jan2019 (s fieldvariable, e record) list*)
  | Enum of string * string list * expr             (* 2018Jan - Enum ("Weekend",["Sat"; "Sun"],EnumVal ("Weekend","Sat")) *)
  | EnumVal of string * string                        (* 2018Jan - se ovenstående, enum n, værdien for n *)