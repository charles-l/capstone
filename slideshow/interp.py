class Id():
    def __init__(self, name):
        self.name = name

    def __hash__(self):
        return self.name.__hash__()

class If():
    def __init__(self, cond, thens, elses):
        self.cond = cond
        self.thens = thens
        self.elses = elses

class Lambda():
    def __init__(self, args, *body):
        self.args = args
        self.body = body

class Call():
    def __init__(self, fun, *args):
        self.fun = fun
        self.args = args

class Closure():
    def __init__(self, args, body, env):
        self.args = args
        self.body = body
        self.env = env

class Begin():
    def __init__(self, *body):
        self.body = body

class Assign():
    def __init__(self, lhs, rhs):
        assert(isinstance(lhs, Id))
        self.lhs = lhs
        self.rhs = rhs

class Primfun():
    def __init__(self, fun):
        self.fun = fun

def eval(t, env):
    def lookup(v):
        for f in env:
            if v.name in f:
                return f[v.name]
        raise NameError(v.name + " is not bound")

    def assign(i, v):
        env[0][i.name] = v

    def is_literal(t):
        return any([isinstance(t, x) for x in [int, bool, str, list]])

    if is_literal(t):
        return t
    elif isinstance(t, Id):
        return lookup(t)
    elif isinstance(t, If):
        if eval(t.cond, env):
            return eval(t.thens, env)
        else:
            return eval(t.elses, env)
    elif isinstance(t, Assign):
        r = eval(t.rhs, env)
        assign(t.lhs, r)
        return r
    elif isinstance(t, Begin):
        for e in t.body:
            e = eval(e, env)
        return e
    elif isinstance(t, Call):
        return apply(
                eval(t.fun, env),
                [eval(a, env) for a in t.args])
    elif isinstance(t, Lambda):
        return Closure(t.args, t.body, env)

def apply(fun, args):
    if isinstance(fun, Closure):
        new_env = [dict(zip(map(lambda l: l.name, fun.args), args))] + fun.env
        last = None
        for expr in fun.body:
            last = eval(expr, new_env)
        return last
    elif isinstance(fun, Primfun):
        return fun.fun(*args)
    else:
        raise TypeError("Not a function " + str(fun))

s = Id
program = Begin(
        Assign(s('fac'), Lambda([s('n')],
            If(Call(s('one?'), s('n')),
                1,
                Call(s('*'), s('n'), Call(s('fac'), Call(s('sub1'), s('n'))))))),
            Call(s('print'), Call(s('fac'), 10)))

v = eval(program, [{'one?': Primfun(lambda a: a == 1),
    '*': Primfun(lambda a, b: a * b),
    'sub1': Primfun(lambda n: n - 1),
    'print': Primfun(print)}])
