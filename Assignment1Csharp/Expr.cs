namespace Assignment1Csharp;

public abstract class Expr
{
    public abstract int Eval(Dictionary<string, int> env);
    public abstract Expr Simplify();
}

public class CstI : Expr
{
    public int i { get; init; }
    public CstI(int i)
    {
        this.i = i;
    }

    public override string ToString()
    {
        return i.ToString();
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return i;
    }

    public override Expr Simplify()
    {
        return this;
    }
}

public class Var : Expr
{
    public string id { get; init; }
    public Var(string id)
    {
        this.id = id;
    }

    public override string ToString()
    {
        return id;
    }
    public override int Eval(Dictionary<string, int> env)
    {
        return env[id];
    }
    public override Expr Simplify()
    {
        return this;
    }
}

public abstract class Binop : Expr
{
    protected Expr a;
    protected Expr b;
}

public class Add : Binop
{
    public Add(Expr a, Expr b)
    {
        this.a = a;
        this.b = b;
    }

    public override string ToString()
    {
        return $"({a}+{b})";
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return a.Eval(env) + b.Eval(env);
    }

    public override Expr Simplify()
    {
        var s1 = a.Simplify();
        var s2 = b.Simplify();
        if (s1 is CstI { i: 0 })
        {
            return s2;
        }
        if (s2 is CstI { i: 0 })
        {
            return s1;
        }

        return new Add(s1, s2);
    }
}

public class Sub : Binop
{
    public Sub(Expr a, Expr b)
    {
        this.a = a;
        this.b = b;
    }
    public override string ToString()
    {
        return $"({a}-{b})";
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return a.Eval(env) - b.Eval(env);
    }

    public override Expr Simplify()
    {
        var s1 = a.Simplify();
        var s2 = b.Simplify();
        if (s2 is CstI { i: 0 })
        {
            return s1;
        }
        if (s1 is CstI c1 && s2 is CstI c2 && c1.i == c2.i)
        {
            return new CstI(0);
        }
        if (s1 is Var v1 && s2 is Var v2 && v1.id == v2.id)
        {
            return new CstI(0);
        }
        
        return new Sub(s1, s2);
        
    }
}

public class Mul : Binop
{
    public Mul(Expr a, Expr b)
    {
        this.a = a;
        this.b = b;
    }
    public override string ToString()
    {
        return $"({a}*{b})";
    }
    
    public override int Eval(Dictionary<string, int> env)
    {
        return a.Eval(env) * b.Eval(env);
    }

    public override Expr Simplify()
    {
        var s1 = a.Simplify();
        var s2 = b.Simplify();
        if (s1 is CstI { i: 0 } || s2 is CstI { i: 0 })
        {
            return new CstI(0);
        }
        if (s2 is CstI { i: 1 })
        {
            return s1;
        }
        if (s1 is CstI { i: 1 })
        {
            return s2;
        }
        
        return new Mul(s1, s2);
    }
}