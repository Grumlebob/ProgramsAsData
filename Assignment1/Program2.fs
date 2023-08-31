module Assignment1.Two 

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
            *)
            
            
            let cenv1 = List.fold (fun acc (x, _) -> x :: acc) cenv assigns
            List.fold (fun (env : string list, exp) (_, erh) -> (env.Tail, TLet(tcomp erh env.Tail, exp))) (cenv1, tcomp ebody cenv1) (List.rev assigns)
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

//print test
printfn "%A" test