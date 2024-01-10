// Implementation file for parser generated by fsyacc
module FunPar
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "FunPar.fsy"

 (* File Fun/FunPar.fsy 
    Parser for micro-ML, a small functional language; one-argument functions.
    sestoft@itu.dk * 2009-10-19
  *)

 open Absyn;

# 15 "FunPar.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | UNION
  | RETURNHEAD
  | INSERTTAIL
  | LBRACKET
  | RBRACKET
  | RARROW
  | EOF
  | LPAR
  | RPAR
  | EQ
  | NE
  | GT
  | LT
  | GE
  | LE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | ELSE
  | END
  | FALSE
  | IF
  | IN
  | LET
  | NOT
  | THEN
  | TRUE
  | CSTBOOL of (bool)
  | NAME of (string)
  | CSTINT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_UNION
    | TOKEN_RETURNHEAD
    | TOKEN_INSERTTAIL
    | TOKEN_LBRACKET
    | TOKEN_RBRACKET
    | TOKEN_RARROW
    | TOKEN_EOF
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EQ
    | TOKEN_NE
    | TOKEN_GT
    | TOKEN_LT
    | TOKEN_GE
    | TOKEN_LE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_ELSE
    | TOKEN_END
    | TOKEN_FALSE
    | TOKEN_IF
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_NOT
    | TOKEN_THEN
    | TOKEN_TRUE
    | TOKEN_CSTBOOL
    | TOKEN_NAME
    | TOKEN_CSTINT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_QueueList
    | NONTERM_Main
    | NONTERM_Expr
    | NONTERM_AtExpr
    | NONTERM_AppExpr
    | NONTERM_Const

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | UNION  -> 0 
  | RETURNHEAD  -> 1 
  | INSERTTAIL  -> 2 
  | LBRACKET  -> 3 
  | RBRACKET  -> 4 
  | RARROW  -> 5 
  | EOF  -> 6 
  | LPAR  -> 7 
  | RPAR  -> 8 
  | EQ  -> 9 
  | NE  -> 10 
  | GT  -> 11 
  | LT  -> 12 
  | GE  -> 13 
  | LE  -> 14 
  | PLUS  -> 15 
  | MINUS  -> 16 
  | TIMES  -> 17 
  | DIV  -> 18 
  | MOD  -> 19 
  | ELSE  -> 20 
  | END  -> 21 
  | FALSE  -> 22 
  | IF  -> 23 
  | IN  -> 24 
  | LET  -> 25 
  | NOT  -> 26 
  | THEN  -> 27 
  | TRUE  -> 28 
  | CSTBOOL _ -> 29 
  | NAME _ -> 30 
  | CSTINT _ -> 31 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_UNION 
  | 1 -> TOKEN_RETURNHEAD 
  | 2 -> TOKEN_INSERTTAIL 
  | 3 -> TOKEN_LBRACKET 
  | 4 -> TOKEN_RBRACKET 
  | 5 -> TOKEN_RARROW 
  | 6 -> TOKEN_EOF 
  | 7 -> TOKEN_LPAR 
  | 8 -> TOKEN_RPAR 
  | 9 -> TOKEN_EQ 
  | 10 -> TOKEN_NE 
  | 11 -> TOKEN_GT 
  | 12 -> TOKEN_LT 
  | 13 -> TOKEN_GE 
  | 14 -> TOKEN_LE 
  | 15 -> TOKEN_PLUS 
  | 16 -> TOKEN_MINUS 
  | 17 -> TOKEN_TIMES 
  | 18 -> TOKEN_DIV 
  | 19 -> TOKEN_MOD 
  | 20 -> TOKEN_ELSE 
  | 21 -> TOKEN_END 
  | 22 -> TOKEN_FALSE 
  | 23 -> TOKEN_IF 
  | 24 -> TOKEN_IN 
  | 25 -> TOKEN_LET 
  | 26 -> TOKEN_NOT 
  | 27 -> TOKEN_THEN 
  | 28 -> TOKEN_TRUE 
  | 29 -> TOKEN_CSTBOOL 
  | 30 -> TOKEN_NAME 
  | 31 -> TOKEN_CSTINT 
  | 34 -> TOKEN_end_of_input
  | 32 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM_QueueList 
    | 2 -> NONTERM_QueueList 
    | 3 -> NONTERM_Main 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_Expr 
    | 6 -> NONTERM_Expr 
    | 7 -> NONTERM_Expr 
    | 8 -> NONTERM_Expr 
    | 9 -> NONTERM_Expr 
    | 10 -> NONTERM_Expr 
    | 11 -> NONTERM_Expr 
    | 12 -> NONTERM_Expr 
    | 13 -> NONTERM_Expr 
    | 14 -> NONTERM_Expr 
    | 15 -> NONTERM_Expr 
    | 16 -> NONTERM_Expr 
    | 17 -> NONTERM_Expr 
    | 18 -> NONTERM_Expr 
    | 19 -> NONTERM_Expr 
    | 20 -> NONTERM_Expr 
    | 21 -> NONTERM_Expr 
    | 22 -> NONTERM_Expr 
    | 23 -> NONTERM_AtExpr 
    | 24 -> NONTERM_AtExpr 
    | 25 -> NONTERM_AtExpr 
    | 26 -> NONTERM_AtExpr 
    | 27 -> NONTERM_AtExpr 
    | 28 -> NONTERM_AppExpr 
    | 29 -> NONTERM_AppExpr 
    | 30 -> NONTERM_Const 
    | 31 -> NONTERM_Const 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 34 
let _fsyacc_tagOfErrorTerminal = 32

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | UNION  -> "UNION" 
  | RETURNHEAD  -> "RETURNHEAD" 
  | INSERTTAIL  -> "INSERTTAIL" 
  | LBRACKET  -> "LBRACKET" 
  | RBRACKET  -> "RBRACKET" 
  | RARROW  -> "RARROW" 
  | EOF  -> "EOF" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | EQ  -> "EQ" 
  | NE  -> "NE" 
  | GT  -> "GT" 
  | LT  -> "LT" 
  | GE  -> "GE" 
  | LE  -> "LE" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | MOD  -> "MOD" 
  | ELSE  -> "ELSE" 
  | END  -> "END" 
  | FALSE  -> "FALSE" 
  | IF  -> "IF" 
  | IN  -> "IN" 
  | LET  -> "LET" 
  | NOT  -> "NOT" 
  | THEN  -> "THEN" 
  | TRUE  -> "TRUE" 
  | CSTBOOL _ -> "CSTBOOL" 
  | NAME _ -> "NAME" 
  | CSTINT _ -> "CSTINT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | UNION  -> (null : System.Object) 
  | RETURNHEAD  -> (null : System.Object) 
  | INSERTTAIL  -> (null : System.Object) 
  | LBRACKET  -> (null : System.Object) 
  | RBRACKET  -> (null : System.Object) 
  | RARROW  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | NE  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | GE  -> (null : System.Object) 
  | LE  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MOD  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | IN  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | CSTBOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CSTINT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 2us; 65535us; 3us; 4us; 50us; 51us; 1us; 65535us; 0us; 1us; 26us; 65535us; 0us; 5us; 3us; 2us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 36us; 17us; 37us; 18us; 38us; 19us; 39us; 20us; 40us; 21us; 41us; 22us; 42us; 23us; 43us; 24us; 44us; 25us; 45us; 26us; 46us; 27us; 47us; 28us; 48us; 29us; 49us; 30us; 50us; 2us; 57us; 31us; 58us; 32us; 61us; 33us; 62us; 34us; 64us; 35us; 28us; 65535us; 0us; 7us; 3us; 7us; 7us; 66us; 8us; 67us; 9us; 7us; 11us; 7us; 13us; 7us; 15us; 7us; 36us; 7us; 37us; 7us; 38us; 7us; 39us; 7us; 40us; 7us; 41us; 7us; 42us; 7us; 43us; 7us; 44us; 7us; 45us; 7us; 46us; 7us; 47us; 7us; 48us; 7us; 49us; 7us; 50us; 7us; 57us; 7us; 58us; 7us; 61us; 7us; 62us; 7us; 64us; 7us; 26us; 65535us; 0us; 8us; 3us; 8us; 9us; 8us; 11us; 8us; 13us; 8us; 15us; 8us; 36us; 8us; 37us; 8us; 38us; 8us; 39us; 8us; 40us; 8us; 41us; 8us; 42us; 8us; 43us; 8us; 44us; 8us; 45us; 8us; 46us; 8us; 47us; 8us; 48us; 8us; 49us; 8us; 50us; 8us; 57us; 8us; 58us; 8us; 61us; 8us; 62us; 8us; 64us; 8us; 28us; 65535us; 0us; 53us; 3us; 53us; 7us; 53us; 8us; 53us; 9us; 53us; 11us; 53us; 13us; 53us; 15us; 53us; 36us; 53us; 37us; 53us; 38us; 53us; 39us; 53us; 40us; 53us; 41us; 53us; 42us; 53us; 43us; 53us; 44us; 53us; 45us; 53us; 46us; 53us; 47us; 53us; 48us; 53us; 49us; 53us; 50us; 53us; 57us; 53us; 58us; 53us; 61us; 53us; 62us; 53us; 64us; 53us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 4us; 6us; 33us; 62us; 89us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 15us; 1us; 2us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 2us; 1us; 2us; 14us; 3us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 3us; 2us; 4us; 28us; 2us; 5us; 29us; 1us; 6us; 14us; 6us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 6us; 14us; 6us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 6us; 14us; 6us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 7us; 14us; 7us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 15us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 16us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 17us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 18us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 19us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 20us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 21us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 25us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 25us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 26us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 26us; 14us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 27us; 1us; 8us; 1us; 9us; 1us; 10us; 1us; 11us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 22us; 1us; 22us; 1us; 23us; 1us; 24us; 2us; 25us; 26us; 2us; 25us; 26us; 1us; 25us; 1us; 25us; 1us; 25us; 1us; 26us; 1us; 26us; 1us; 26us; 1us; 26us; 1us; 27us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 31us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 20us; 22us; 24us; 39us; 41us; 44us; 47us; 49us; 64us; 66us; 81us; 83us; 98us; 100us; 115us; 130us; 145us; 160us; 175us; 190us; 205us; 220us; 235us; 250us; 265us; 280us; 295us; 310us; 325us; 340us; 355us; 370us; 385us; 400us; 402us; 404us; 406us; 408us; 410us; 412us; 414us; 416us; 418us; 420us; 422us; 424us; 426us; 428us; 430us; 432us; 434us; 436us; 438us; 441us; 444us; 446us; 448us; 450us; 452us; 454us; 456us; 458us; 460us; 462us; 464us; 466us; 468us; |]
let _fsyacc_action_rows = 70
let _fsyacc_actionTableElements = [|9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 0us; 49152us; 14us; 16385us; 0us; 47us; 2us; 48us; 5us; 3us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 0us; 16386us; 14us; 32768us; 0us; 47us; 2us; 48us; 6us; 6us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 0us; 16387us; 5us; 16388us; 7us; 64us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 5us; 16389us; 7us; 64us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 14us; 32768us; 0us; 47us; 2us; 48us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 27us; 11us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 14us; 32768us; 0us; 47us; 2us; 48us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 20us; 13us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 13us; 16390us; 0us; 47us; 2us; 48us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 4us; 16391us; 2us; 48us; 17us; 38us; 18us; 39us; 19us; 40us; 4us; 16392us; 2us; 48us; 17us; 38us; 18us; 39us; 19us; 40us; 4us; 16393us; 2us; 48us; 17us; 38us; 18us; 39us; 19us; 40us; 0us; 16394us; 0us; 16395us; 0us; 16396us; 11us; 16397us; 0us; 47us; 2us; 48us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 11us; 16398us; 0us; 47us; 2us; 48us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 7us; 16399us; 0us; 47us; 2us; 48us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 7us; 16400us; 0us; 47us; 2us; 48us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 7us; 16401us; 0us; 47us; 2us; 48us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 7us; 16402us; 0us; 47us; 2us; 48us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 4us; 16403us; 2us; 48us; 17us; 38us; 18us; 39us; 19us; 40us; 0us; 16404us; 0us; 16405us; 14us; 32768us; 0us; 47us; 2us; 48us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 24us; 58us; 14us; 32768us; 0us; 47us; 2us; 48us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 21us; 59us; 14us; 32768us; 0us; 47us; 2us; 48us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 24us; 62us; 14us; 32768us; 0us; 47us; 2us; 48us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 21us; 63us; 14us; 32768us; 0us; 47us; 2us; 48us; 8us; 65us; 9us; 41us; 10us; 42us; 11us; 43us; 12us; 44us; 13us; 45us; 14us; 46us; 15us; 36us; 16us; 37us; 17us; 38us; 18us; 39us; 19us; 40us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 1us; 32768us; 4us; 52us; 0us; 16406us; 0us; 16407us; 0us; 16408us; 1us; 32768us; 30us; 56us; 2us; 32768us; 9us; 57us; 30us; 60us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 0us; 16409us; 1us; 32768us; 9us; 61us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 0us; 16410us; 9us; 32768us; 1us; 49us; 3us; 50us; 7us; 64us; 16us; 15us; 23us; 9us; 25us; 55us; 29us; 69us; 30us; 54us; 31us; 68us; 0us; 16411us; 0us; 16412us; 0us; 16413us; 0us; 16414us; 0us; 16415us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 10us; 11us; 26us; 36us; 37us; 52us; 53us; 59us; 65us; 75us; 90us; 100us; 115us; 125us; 139us; 149us; 154us; 159us; 164us; 165us; 166us; 167us; 179us; 191us; 199us; 207us; 215us; 223us; 228us; 229us; 230us; 245us; 260us; 275us; 290us; 305us; 315us; 325us; 335us; 345us; 355us; 365us; 375us; 385us; 395us; 405us; 415us; 425us; 435us; 445us; 455us; 457us; 458us; 459us; 460us; 462us; 465us; 475us; 485us; 486us; 488us; 498us; 508us; 509us; 519us; 520us; 521us; 522us; 523us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 3us; 2us; 1us; 1us; 6us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 2us; 3us; 1us; 1us; 7us; 8us; 3us; 2us; 2us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 1us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; 6us; 6us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 65535us; 16386us; 65535us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16406us; 16407us; 16408us; 65535us; 65535us; 65535us; 65535us; 16409us; 65535us; 65535us; 65535us; 16410us; 65535us; 16411us; 16412us; 16413us; 16414us; 16415us; |]
let _fsyacc_reductions ()  =    [| 
# 293 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 302 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "FunPar.fsy"
                                                                [_1 ] 
                   )
# 38 "FunPar.fsy"
                 : 'QueueList));
# 313 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'QueueList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "FunPar.fsy"
                                                                _1 :: _3 
                   )
# 39 "FunPar.fsy"
                 : 'QueueList));
# 325 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "FunPar.fsy"
                                                               _1 
                   )
# 44 "FunPar.fsy"
                 : Absyn.expr));
# 336 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "FunPar.fsy"
                                                               _1                     
                   )
# 48 "FunPar.fsy"
                 : Absyn.expr));
# 347 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "FunPar.fsy"
                                                               _1                     
                   )
# 49 "FunPar.fsy"
                 : Absyn.expr));
# 358 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "FunPar.fsy"
                                                               If(_2, _4, _6)         
                   )
# 50 "FunPar.fsy"
                 : Absyn.expr));
# 371 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "FunPar.fsy"
                                                               Prim("-", CstI 0, _2)  
                   )
# 51 "FunPar.fsy"
                 : Absyn.expr));
# 382 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "FunPar.fsy"
                                                               Prim("+",  _1, _3)     
                   )
# 52 "FunPar.fsy"
                 : Absyn.expr));
# 394 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "FunPar.fsy"
                                                               Prim("-",  _1, _3)     
                   )
# 53 "FunPar.fsy"
                 : Absyn.expr));
# 406 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "FunPar.fsy"
                                                               Prim("*",  _1, _3)     
                   )
# 54 "FunPar.fsy"
                 : Absyn.expr));
# 418 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "FunPar.fsy"
                                                               Prim("/",  _1, _3)     
                   )
# 55 "FunPar.fsy"
                 : Absyn.expr));
# 430 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "FunPar.fsy"
                                                               Prim("%",  _1, _3)     
                   )
# 56 "FunPar.fsy"
                 : Absyn.expr));
# 442 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "FunPar.fsy"
                                                               Prim("=",  _1, _3)     
                   )
# 57 "FunPar.fsy"
                 : Absyn.expr));
# 454 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "FunPar.fsy"
                                                               Prim("<>", _1, _3)     
                   )
# 58 "FunPar.fsy"
                 : Absyn.expr));
# 466 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "FunPar.fsy"
                                                               Prim(">",  _1, _3)     
                   )
# 59 "FunPar.fsy"
                 : Absyn.expr));
# 478 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "FunPar.fsy"
                                                               Prim("<",  _1, _3)     
                   )
# 60 "FunPar.fsy"
                 : Absyn.expr));
# 490 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "FunPar.fsy"
                                                               Prim(">=", _1, _3)     
                   )
# 61 "FunPar.fsy"
                 : Absyn.expr));
# 502 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "FunPar.fsy"
                                                               Prim("<=", _1, _3)     
                   )
# 62 "FunPar.fsy"
                 : Absyn.expr));
# 514 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "FunPar.fsy"
                                                               Prim("++", _1, _3)     
                   )
# 63 "FunPar.fsy"
                 : Absyn.expr));
# 526 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "FunPar.fsy"
                                                               Prim("->>", _1, _3)    
                   )
# 64 "FunPar.fsy"
                 : Absyn.expr));
# 538 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "FunPar.fsy"
                                                               Prim1("<<-", _2)       
                   )
# 65 "FunPar.fsy"
                 : Absyn.expr));
# 549 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'QueueList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "FunPar.fsy"
                                                               Queue(_2)              
                   )
# 66 "FunPar.fsy"
                 : Absyn.expr));
# 560 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "FunPar.fsy"
                                                               _1                     
                   )
# 70 "FunPar.fsy"
                 : Absyn.expr));
# 571 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "FunPar.fsy"
                                                               Var _1                 
                   )
# 71 "FunPar.fsy"
                 : Absyn.expr));
# 582 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "FunPar.fsy"
                                                               Let(_2, _4, _6)        
                   )
# 72 "FunPar.fsy"
                 : Absyn.expr));
# 595 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "FunPar.fsy"
                                                               Letfun(_2, _3, _5, _7) 
                   )
# 73 "FunPar.fsy"
                 : Absyn.expr));
# 609 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "FunPar.fsy"
                                                               _2                     
                   )
# 74 "FunPar.fsy"
                 : Absyn.expr));
# 620 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "FunPar.fsy"
                                                               Call(_1, _2)           
                   )
# 78 "FunPar.fsy"
                 : Absyn.expr));
# 632 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "FunPar.fsy"
                                                               Call(_1, _2)           
                   )
# 79 "FunPar.fsy"
                 : Absyn.expr));
# 644 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "FunPar.fsy"
                                                               CstI(_1)               
                   )
# 83 "FunPar.fsy"
                 : Absyn.expr));
# 655 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "FunPar.fsy"
                                                               CstB(_1)               
                   )
# 84 "FunPar.fsy"
                 : Absyn.expr));
|]
# 667 "FunPar.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 35;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Absyn.expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
