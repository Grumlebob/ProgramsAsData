
using Assignment1Csharp;
/*1.4,

Exercise 1.4 This chapter has shown how to represent abstract syntax in functional
languages such as F# (using algebraic datatypes) and in object-oriented languages
such as Java or C# (using a class hierarchy and composites).
(i) Use Java or C# classes and methods to do what we have done using the F# datatype
aexpr in the preceding exercises. Design a class hierarchy to represent arithmetic
expressions: it could have an abstract class Expr with subclasses CstI, Var, and
Binop, where the latter is itself abstract and has concrete subclasses Add, Mul
and Sub. All classes should implement the toString() method to format an
expression as a String.
The classes may be used to build an expression in abstract syntax, and then print
it, as follows:
Expr e = new Add(new CstI(17), new Var("z"));
System.out.println(e.toString());
(ii) Create three more expressions in abstract syntax and print them.
(iii) Extend your classes with facilities to evaluate the arithmetic expressions, that
is, add a method int eval(env).
(iv) Add a method Expr simplify() that returns a new expression where algebraic
simplifications have been performed, as in part (iv) of Exercise 1.2.
 */

Expr e = new Add(new CstI(17), new Var("z"));
Console.WriteLine(e.ToString());

//1.4(ii) three more expressions in abstract syntax and print them.

Expr first = new Mul(new Sub(new CstI(10), new Add(new CstI(1), new CstI(10))), new CstI(9));
Console.WriteLine(first.ToString());

Expr second = new Add(new Add(new CstI(1), new CstI(2)), new Sub(new CstI(3), new CstI(4)));
Console.WriteLine(second.ToString());

Expr third = new Sub(new Mul(new CstI(1), new CstI(2)), new Mul(new CstI(3), new Var("e")));
Console.WriteLine(third.ToString());
//1.4(iii) 
Console.WriteLine(second.Eval(new Dictionary<string, int>()));

//1.4(iv) Simplify demo:

var simpleExample =
    new Mul(new Add(new CstI(0), new Var("x")), new Sub(new CstI(1), new Sub(new Var ("e"), new Var ("e")))).Simplify();
Console.WriteLine(simpleExample.ToString());