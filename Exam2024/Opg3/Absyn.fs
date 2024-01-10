(* Fun/Absyn.fs * Abstract syntax for micro-ML, a functional language *)

module Absyn

type expr = 
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Letfun of string * string * expr * expr    (* (f, x, fBody, letBody) *)
  | Call of expr * expr
  | Queue of expr list   (* exam 2024.  q1 = [1 -> 2 -> 3] *)
  | Prim1 of string * expr (* exam 2024. support af   <<- [1 -> 5] til 5 = 5 *)
