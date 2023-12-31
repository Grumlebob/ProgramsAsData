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
  | DOT
  | BAR
  | ENUM
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
    | TOKEN_DOT
    | TOKEN_BAR
    | TOKEN_ENUM
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
    | NONTERM_Main
    | NONTERM_EnumList
    | NONTERM_EnumElement
    | NONTERM_Expr
    | NONTERM_AtExpr
    | NONTERM_AppExpr
    | NONTERM_Const

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | DOT  -> 0 
  | BAR  -> 1 
  | ENUM  -> 2 
  | EOF  -> 3 
  | LPAR  -> 4 
  | RPAR  -> 5 
  | EQ  -> 6 
  | NE  -> 7 
  | GT  -> 8 
  | LT  -> 9 
  | GE  -> 10 
  | LE  -> 11 
  | PLUS  -> 12 
  | MINUS  -> 13 
  | TIMES  -> 14 
  | DIV  -> 15 
  | MOD  -> 16 
  | ELSE  -> 17 
  | END  -> 18 
  | FALSE  -> 19 
  | IF  -> 20 
  | IN  -> 21 
  | LET  -> 22 
  | NOT  -> 23 
  | THEN  -> 24 
  | TRUE  -> 25 
  | CSTBOOL _ -> 26 
  | NAME _ -> 27 
  | CSTINT _ -> 28 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_DOT 
  | 1 -> TOKEN_BAR 
  | 2 -> TOKEN_ENUM 
  | 3 -> TOKEN_EOF 
  | 4 -> TOKEN_LPAR 
  | 5 -> TOKEN_RPAR 
  | 6 -> TOKEN_EQ 
  | 7 -> TOKEN_NE 
  | 8 -> TOKEN_GT 
  | 9 -> TOKEN_LT 
  | 10 -> TOKEN_GE 
  | 11 -> TOKEN_LE 
  | 12 -> TOKEN_PLUS 
  | 13 -> TOKEN_MINUS 
  | 14 -> TOKEN_TIMES 
  | 15 -> TOKEN_DIV 
  | 16 -> TOKEN_MOD 
  | 17 -> TOKEN_ELSE 
  | 18 -> TOKEN_END 
  | 19 -> TOKEN_FALSE 
  | 20 -> TOKEN_IF 
  | 21 -> TOKEN_IN 
  | 22 -> TOKEN_LET 
  | 23 -> TOKEN_NOT 
  | 24 -> TOKEN_THEN 
  | 25 -> TOKEN_TRUE 
  | 26 -> TOKEN_CSTBOOL 
  | 27 -> TOKEN_NAME 
  | 28 -> TOKEN_CSTINT 
  | 31 -> TOKEN_end_of_input
  | 29 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM_Main 
    | 2 -> NONTERM_EnumList 
    | 3 -> NONTERM_EnumList 
    | 4 -> NONTERM_EnumElement 
    | 5 -> NONTERM_EnumElement 
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
    | 22 -> NONTERM_AtExpr 
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

let _fsyacc_endOfInputTag = 31 
let _fsyacc_tagOfErrorTerminal = 29

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | DOT  -> "DOT" 
  | BAR  -> "BAR" 
  | ENUM  -> "ENUM" 
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
  | DOT  -> (null : System.Object) 
  | BAR  -> (null : System.Object) 
  | ENUM  -> (null : System.Object) 
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
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 64us; 65us; 2us; 65535us; 6us; 7us; 64us; 4us; 22us; 65535us; 0us; 2us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 35us; 18us; 36us; 19us; 37us; 20us; 38us; 21us; 39us; 22us; 40us; 23us; 41us; 24us; 42us; 25us; 43us; 26us; 44us; 27us; 45us; 28us; 53us; 29us; 54us; 30us; 57us; 31us; 58us; 32us; 60us; 33us; 66us; 34us; 24us; 65535us; 0us; 8us; 8us; 68us; 9us; 69us; 10us; 8us; 12us; 8us; 14us; 8us; 16us; 8us; 35us; 8us; 36us; 8us; 37us; 8us; 38us; 8us; 39us; 8us; 40us; 8us; 41us; 8us; 42us; 8us; 43us; 8us; 44us; 8us; 45us; 8us; 53us; 8us; 54us; 8us; 57us; 8us; 58us; 8us; 60us; 8us; 66us; 8us; 22us; 65535us; 0us; 9us; 10us; 9us; 12us; 9us; 14us; 9us; 16us; 9us; 35us; 9us; 36us; 9us; 37us; 9us; 38us; 9us; 39us; 9us; 40us; 9us; 41us; 9us; 42us; 9us; 43us; 9us; 44us; 9us; 45us; 9us; 53us; 9us; 54us; 9us; 57us; 9us; 58us; 9us; 60us; 9us; 66us; 9us; 24us; 65535us; 0us; 49us; 8us; 49us; 9us; 49us; 10us; 49us; 12us; 49us; 14us; 49us; 16us; 49us; 35us; 49us; 36us; 49us; 37us; 49us; 38us; 49us; 39us; 49us; 40us; 49us; 41us; 49us; 42us; 49us; 43us; 49us; 44us; 49us; 45us; 49us; 53us; 49us; 54us; 49us; 57us; 49us; 58us; 49us; 60us; 49us; 66us; 49us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 8us; 31us; 56us; 79us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 12us; 1us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 1us; 1us; 3us; 2us; 4us; 5us; 1us; 5us; 1us; 5us; 2us; 6us; 28us; 2us; 7us; 29us; 1us; 8us; 12us; 8us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 8us; 12us; 8us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 8us; 12us; 8us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 1us; 9us; 12us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 12us; 10us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 12us; 10us; 11us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 12us; 10us; 11us; 12us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 12us; 10us; 11us; 12us; 13us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 12us; 10us; 11us; 12us; 13us; 14us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 15us; 16us; 17us; 18us; 19us; 20us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 16us; 17us; 18us; 19us; 20us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 17us; 18us; 19us; 20us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 18us; 19us; 20us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 19us; 20us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 20us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 24us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 24us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 25us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 25us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 26us; 12us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 27us; 1us; 10us; 1us; 11us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 2us; 21us; 23us; 1us; 21us; 1us; 21us; 1us; 22us; 1us; 23us; 2us; 24us; 25us; 2us; 24us; 25us; 1us; 24us; 1us; 24us; 1us; 24us; 1us; 25us; 1us; 25us; 1us; 25us; 1us; 25us; 1us; 26us; 1us; 26us; 1us; 27us; 1us; 27us; 1us; 27us; 1us; 27us; 1us; 27us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 31us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 17us; 19us; 21us; 24us; 26us; 28us; 31us; 34us; 36us; 49us; 51us; 64us; 66us; 79us; 81us; 94us; 107us; 120us; 133us; 146us; 159us; 172us; 185us; 198us; 211us; 224us; 237us; 250us; 263us; 276us; 289us; 302us; 315us; 317us; 319us; 321us; 323us; 325us; 327us; 329us; 331us; 333us; 335us; 337us; 340us; 342us; 344us; 346us; 348us; 351us; 354us; 356us; 358us; 360us; 362us; 364us; 366us; 368us; 370us; 372us; 374us; 376us; 378us; 380us; 382us; 384us; 386us; 388us; 390us; |]
let _fsyacc_action_rows = 72
let _fsyacc_actionTableElements = [|8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 0us; 49152us; 12us; 32768us; 3us; 3us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 0us; 16385us; 0us; 16387us; 1us; 16388us; 1us; 6us; 1us; 32768us; 27us; 5us; 0us; 16389us; 6us; 16390us; 2us; 62us; 4us; 60us; 22us; 51us; 26us; 71us; 27us; 50us; 28us; 70us; 6us; 16391us; 2us; 62us; 4us; 60us; 22us; 51us; 26us; 71us; 27us; 50us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 12us; 32768us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 24us; 12us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 12us; 32768us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 17us; 14us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 11us; 16392us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 3us; 16393us; 14us; 37us; 15us; 38us; 16us; 39us; 3us; 16394us; 14us; 37us; 15us; 38us; 16us; 39us; 3us; 16395us; 14us; 37us; 15us; 38us; 16us; 39us; 0us; 16396us; 0us; 16397us; 0us; 16398us; 9us; 16399us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 9us; 16400us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 5us; 16401us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 5us; 16402us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 5us; 16403us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 5us; 16404us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 12us; 32768us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 21us; 54us; 12us; 32768us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 18us; 55us; 12us; 32768us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 21us; 58us; 12us; 32768us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 18us; 59us; 12us; 32768us; 5us; 61us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 12us; 32768us; 6us; 40us; 7us; 41us; 8us; 42us; 9us; 43us; 10us; 44us; 11us; 45us; 12us; 35us; 13us; 36us; 14us; 37us; 15us; 38us; 16us; 39us; 18us; 67us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 1us; 16407us; 0us; 47us; 1us; 32768us; 27us; 48us; 0us; 16405us; 0us; 16406us; 0us; 16407us; 1us; 32768us; 27us; 52us; 2us; 32768us; 6us; 53us; 27us; 56us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 0us; 16408us; 1us; 32768us; 6us; 57us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 0us; 16409us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 0us; 16410us; 1us; 32768us; 27us; 63us; 1us; 32768us; 6us; 64us; 1us; 16386us; 27us; 5us; 1us; 32768us; 21us; 66us; 8us; 32768us; 2us; 62us; 4us; 60us; 13us; 16us; 20us; 10us; 22us; 51us; 26us; 71us; 27us; 46us; 28us; 70us; 0us; 16411us; 0us; 16412us; 0us; 16413us; 0us; 16414us; 0us; 16415us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 9us; 10us; 23us; 24us; 25us; 27us; 29us; 30us; 37us; 44us; 53us; 66us; 75us; 88us; 97us; 109us; 118us; 122us; 126us; 130us; 131us; 132us; 133us; 143us; 153us; 159us; 165us; 171us; 177us; 190us; 203us; 216us; 229us; 242us; 255us; 264us; 273us; 282us; 291us; 300us; 309us; 318us; 327us; 336us; 345us; 354us; 356us; 358us; 359us; 360us; 361us; 363us; 366us; 375us; 384us; 385us; 387us; 396us; 405us; 406us; 415us; 416us; 418us; 420us; 422us; 424us; 433us; 434us; 435us; 436us; 437us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 0us; 1us; 1us; 3us; 1us; 1us; 6us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; 1us; 7us; 8us; 3us; 7us; 2us; 2us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; 5us; 5us; 5us; 5us; 6us; 6us; 7us; 7us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16387us; 65535us; 65535us; 16389us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16405us; 16406us; 16407us; 65535us; 65535us; 65535us; 65535us; 16408us; 65535us; 65535us; 65535us; 16409us; 65535us; 16410us; 65535us; 65535us; 65535us; 65535us; 65535us; 16411us; 16412us; 16413us; 16414us; 16415us; |]
let _fsyacc_reductions ()  =    [| 
# 276 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 285 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "FunPar.fsy"
                                                               _1 
                   )
# 35 "FunPar.fsy"
                 : Absyn.expr));
# 296 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "FunPar.fsy"
                                                               [] 
                   )
# 39 "FunPar.fsy"
                 : 'EnumList));
# 306 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EnumElement)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "FunPar.fsy"
                                                               _1 
                   )
# 40 "FunPar.fsy"
                 : 'EnumList));
# 317 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "FunPar.fsy"
                                                               [_1]     
                   )
# 44 "FunPar.fsy"
                 : 'EnumElement));
# 328 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EnumElement)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "FunPar.fsy"
                                                               _1 :: _3 
                   )
# 45 "FunPar.fsy"
                 : 'EnumElement));
# 340 "FunPar.fs"
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
# 351 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "FunPar.fsy"
                                                               _1                     
                   )
# 50 "FunPar.fsy"
                 : Absyn.expr));
# 362 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "FunPar.fsy"
                                                               If(_2, _4, _6)         
                   )
# 51 "FunPar.fsy"
                 : Absyn.expr));
# 375 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "FunPar.fsy"
                                                               Prim("-", CstI 0, _2)  
                   )
# 52 "FunPar.fsy"
                 : Absyn.expr));
# 386 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "FunPar.fsy"
                                                               Prim("+",  _1, _3)     
                   )
# 53 "FunPar.fsy"
                 : Absyn.expr));
# 398 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "FunPar.fsy"
                                                               Prim("-",  _1, _3)     
                   )
# 54 "FunPar.fsy"
                 : Absyn.expr));
# 410 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "FunPar.fsy"
                                                               Prim("*",  _1, _3)     
                   )
# 55 "FunPar.fsy"
                 : Absyn.expr));
# 422 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "FunPar.fsy"
                                                               Prim("/",  _1, _3)     
                   )
# 56 "FunPar.fsy"
                 : Absyn.expr));
# 434 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "FunPar.fsy"
                                                               Prim("%",  _1, _3)     
                   )
# 57 "FunPar.fsy"
                 : Absyn.expr));
# 446 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "FunPar.fsy"
                                                               Prim("=",  _1, _3)     
                   )
# 58 "FunPar.fsy"
                 : Absyn.expr));
# 458 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "FunPar.fsy"
                                                               Prim("<>", _1, _3)     
                   )
# 59 "FunPar.fsy"
                 : Absyn.expr));
# 470 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "FunPar.fsy"
                                                               Prim(">",  _1, _3)     
                   )
# 60 "FunPar.fsy"
                 : Absyn.expr));
# 482 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "FunPar.fsy"
                                                               Prim("<",  _1, _3)     
                   )
# 61 "FunPar.fsy"
                 : Absyn.expr));
# 494 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "FunPar.fsy"
                                                               Prim(">=", _1, _3)     
                   )
# 62 "FunPar.fsy"
                 : Absyn.expr));
# 506 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "FunPar.fsy"
                                                               Prim("<=", _1, _3)     
                   )
# 63 "FunPar.fsy"
                 : Absyn.expr));
# 518 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "FunPar.fsy"
                                                               EnumVal(_1, _3)       
                   )
# 64 "FunPar.fsy"
                 : Absyn.expr));
# 530 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "FunPar.fsy"
                                                               _1                     
                   )
# 68 "FunPar.fsy"
                 : Absyn.expr));
# 541 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "FunPar.fsy"
                                                               Var _1                 
                   )
# 69 "FunPar.fsy"
                 : Absyn.expr));
# 552 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "FunPar.fsy"
                                                               Let(_2, _4, _6)        
                   )
# 70 "FunPar.fsy"
                 : Absyn.expr));
# 565 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "FunPar.fsy"
                                                               Letfun(_2, _3, _5, _7) 
                   )
# 71 "FunPar.fsy"
                 : Absyn.expr));
# 579 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "FunPar.fsy"
                                                               _2                     
                   )
# 72 "FunPar.fsy"
                 : Absyn.expr));
# 590 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'EnumList)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "FunPar.fsy"
                                                               Enum(_2, _4, _6)   
                   )
# 73 "FunPar.fsy"
                 : Absyn.expr));
# 603 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "FunPar.fsy"
                                                               Call(_1, _2)           
                   )
# 77 "FunPar.fsy"
                 : Absyn.expr));
# 615 "FunPar.fs"
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
# 627 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "FunPar.fsy"
                                                               CstI(_1)               
                   )
# 82 "FunPar.fsy"
                 : Absyn.expr));
# 638 "FunPar.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "FunPar.fsy"
                                                               CstB(_1)               
                   )
# 83 "FunPar.fsy"
                 : Absyn.expr));
|]
# 650 "FunPar.fs"
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
    numTerminals = 32;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Absyn.expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
