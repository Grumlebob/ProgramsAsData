
using Assignment1Csharp;

Console.WriteLine("Hello World!");

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