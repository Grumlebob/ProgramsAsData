(* Fun/Absyn.fs * Abstract syntax for micro-ML, a functional language *)

module Absyn

type expr = 
  | CstI of int
  | CstB of bool
  | CstD of double
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | Prim1 of string * expr //Exam Prim1("toInt", Var "x")
  | If of expr * expr * expr
  | Letfun of string * string * expr * expr    (* (f, x, fBody, letBody) *)
  | Call of expr * expr
