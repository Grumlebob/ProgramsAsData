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
