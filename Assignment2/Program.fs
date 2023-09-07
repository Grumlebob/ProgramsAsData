(*

Exercise 2.4 Write a bytecode assembler (in F#) that translates a list of bytecode
instructions for the simple stack machine in Intcomp1.fs into a list of
integers. The integers should be the corresponding bytecodes for the interpreter
in Machine.java. Thus you should write a function assemble : sinstr
list -> int list.
Use this function together with scomp from Intcomp1.fs to make a compiler
from the original expressions language expr to a list of bytecodes int list.
You may test the output of your compiler by typing in the numbers as an int
array in the Machine.java interpreter. (Or you may solve Exercise 2.5 below to
avoid this manual work).

*)


open Assignment2.intcomp1

open System.Text.RegularExpressions


let sinstrToInt (stackInstruction : sinstr) : int list =
    match stackInstruction with
    | SCstI i -> [0;i]
    | SVar i -> [1;i]
    | SAdd -> [2]
    | SSub -> [3]
    | SMul -> [4]
    | SPop -> [5]
    | SSwap -> [6]
   

let assemble (stackinstructions : sinstr list) : int list =
    List.fold (fun acc x -> acc@(sinstrToInt x)) [] stackinstructions

let result = assemble (scomp e1 [])
printfn "%A" result

intsToFile (assemble (scomp e1 [])) "is1.txt";;



(*
Exercise 3.2 Write a regular expression that recognizes all sequences consisting of
a and b where two a’s are always separated by at least one b. For instance, these
four strings are legal: b, a, ba, ababbbaba; but these two strings are illegal: aa,
babaa.
Construct the corresponding NFA. Try to find a DFA corresponding to the NFA.

*)

let regex32 = "^b?(ab|b)*a?$"
let testregexString = "ababbbaba"
let regexMatch = Regex.Match(testregexString, regex32)
printfn "%A" regexMatch.Success


(*
Exercise 2.1
In the following, a number-string is a non-empty sequence of decimal digits, i.e.,
something in the language defined by the regular expression [0-9]+. The value of
a number-string is the usual interpretation of a number-string as an integer number.
Note that leading zeroes are allowed.
Make for each of the following languages a regular expression that describes
that language.

a) All number-strings that have the value 42.

regex: 0*42

b) All number-strings that do not have the value 42.

regex: ^([0-9]|[0-9]*([0-9][0-13-9]|[0-35-9][0-9])|([0-9]*[1-9][0-9]*[0-9][0-9]))$

c) All number-strings that have a value that is strictly greater than 42.

// 

*)

let regexA = "0*42"
let regexB = "^([0-9]|[0-9]*([0-9][0-13-9]|[0-35-9][0-9])|([0-9]*[1-9][0-9]*[0-9][0-9]))$"
let regexC = "0*([4-9][3-9][0-9]*)|([5-9][0-9][0-9]*)"

let regexATest = Regex.Match("000042", regexA)
let regexBTest = Regex.Match("000041", regexB)  
let regexCTest = Regex.Match("000043", regexC)

printfn "%A" regexATest.Success
printfn "%A" regexBTest.Success
printfn "%A" regexCTest.Success






    
    
    

