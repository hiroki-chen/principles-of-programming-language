import sys
from enum import Enum
from greenlet import greenlet
from typing import Any, Callable

# Define the registers
g__e_: object = None
g__env_: object = None
g__k_: object = None
g__v_: object = None

# Define the program counter
g__pc_ : Callable[[], Any] = None

# Define the dismount greenlet
_dismount_blank = None

# Define the union classes
class union_t(object):
    class expr(Enum):
        const = 0
        var = 1
        _if = 2
        mult = 3
        subr1 = 4
        zero = 5
        catch = 6
        pitch = 7
        let = 8
        _lambda = 9
        app = 10

    class clos(Enum):
        closure = 0

    class envr(Enum):
        empty = 0
        extend = 1

    class kt(Enum):
        empty = 0
        subr1 = 1
        zero = 2
        _if = 3
        pitch = 4
        let = 5
        multr1 = 6
        multr2 = 7
        appr1 = 8
        appr2 = 9

    def __init__(self, type: Enum, **kwargs):
        self.type = type
        for key in kwargs:
            setattr(self, key, kwargs[key])


# Union functions
def kt_empty(jumpout):
    return union_t(union_t.kt.empty, 
            jumpout=jumpout)

def kt_subr1(k_cap):
    return union_t(union_t.kt.subr1, 
            k_cap=k_cap)

def kt_zero(k_cap):
    return union_t(union_t.kt.zero, 
            k_cap=k_cap)

def kt_if(conseq_cap, alt_cap, env_cap, k_cap):
    return union_t(union_t.kt._if, 
            conseq_cap=conseq_cap, 
            alt_cap=alt_cap, 
            env_cap=env_cap, 
            k_cap=k_cap)

def kt_pitch(v_exp, env_cap):
    return union_t(union_t.kt.pitch, 
            v_exp=v_exp, 
            env_cap=env_cap)

def kt_let(body_cap, env_cap, k_cap):
    return union_t(union_t.kt.let, 
            body_cap=body_cap, 
            env_cap=env_cap, 
            k_cap=k_cap)

def kt_multr1(xr2_cap, env_cap, k_cap):
    return union_t(union_t.kt.multr1, 
            xr2_cap=xr2_cap, 
            env_cap=env_cap, 
            k_cap=k_cap)

def kt_multr2(lhs, k_cap):
    return union_t(union_t.kt.multr2, 
            lhs=lhs, 
            k_cap=k_cap)

def kt_appr1(rand_cap, env_cap, k_cap):
    return union_t(union_t.kt.appr1, 
            rand_cap=rand_cap, 
            env_cap=env_cap, 
            k_cap=k_cap)

def kt_appr2(rator, k_cap):
    return union_t(union_t.kt.appr2, 
            rator=rator, 
            k_cap=k_cap)

def envr_empty():
    return union_t(union_t.envr.empty, )

def envr_extend(env, y):
    return union_t(union_t.envr.extend, 
            env=env, 
            y=y)

def clos_closure(body, env):
    return union_t(union_t.clos.closure, 
            body=body, 
            env=env)

def expr_const(cexp):
    return union_t(union_t.expr.const, 
            cexp=cexp)

def expr_var(n):
    return union_t(union_t.expr.var, 
            n=n)

def expr_if(test, conseq, alt):
    return union_t(union_t.expr._if, 
            test=test, 
            conseq=conseq, 
            alt=alt)

def expr_mult(nexpr1, nexpr2):
    return union_t(union_t.expr.mult, 
            nexpr1=nexpr1, 
            nexpr2=nexpr2)

def expr_subr1(nexp):
    return union_t(union_t.expr.subr1, 
            nexp=nexp)

def expr_zero(nexp):
    return union_t(union_t.expr.zero, 
            nexp=nexp)

def expr_catch(body):
    return union_t(union_t.expr.catch, 
            body=body)

def expr_pitch(kexp, vexp):
    return union_t(union_t.expr.pitch, 
            kexp=kexp, 
            vexp=vexp)

def expr_let(exp, body):
    return union_t(union_t.expr.let, 
            exp=exp, 
            body=body)

def expr_lambda(body):
    return union_t(union_t.expr._lambda, 
            body=body)

def expr_app(rator, rand):
    return union_t(union_t.expr.app, 
            rator=rator, 
            rand=rand)

# Generate functions
def apply_k():
    global g__pc_
    global g__e_
    global g__k_
    global g__env_
    global g__v_

    match g__k_.type:
        case union_t.kt.zero:
            k_cap = g__k_.k_cap
            g__k_ = k_cap
            g__v_ = (g__v_ == 0)
            g__pc_ = apply_k

        case union_t.kt.subr1:
            k_cap = g__k_.k_cap
            g__k_ = k_cap
            g__v_ = (g__v_ - 1)
            g__pc_ = apply_k

        case union_t.kt._if:
            conseq_cap = g__k_.conseq_cap
            alt_cap = g__k_.alt_cap
            env_cap = g__k_.env_cap
            k_cap = g__k_.k_cap
            g__k_ = k_cap
            g__env_ = env_cap
            if g__v_:
                g__e_ = conseq_cap
            else:
                g__e_ = alt_cap
            g__pc_ = value_of_cps

        case union_t.kt.let:
            body_cap = g__k_.body_cap
            env_cap = g__k_.env_cap
            k_cap = g__k_.k_cap
            g__k_ = k_cap
            g__env_ = envr_extend(env_cap, g__v_)
            g__e_ = body_cap
            g__pc_ = value_of_cps

        case union_t.kt.pitch:
            v_exp = g__k_.v_exp
            env_cap = g__k_.env_cap
            g__k_ = g__v_
            g__e_ = v_exp
            g__env_ = env_cap
            g__pc_ = value_of_cps

        case union_t.kt.multr1:
            xr2_cap = g__k_.xr2_cap
            env_cap = g__k_.env_cap
            k_cap = g__k_.k_cap
            g__k_ = kt_multr2(g__v_, k_cap)
            g__e_ = xr2_cap
            g__env_ = env_cap
            g__pc_ = value_of_cps

        case union_t.kt.multr2:
            lhs = g__k_.lhs
            k_cap = g__k_.k_cap
            g__k_ = k_cap
            g__v_ = lhs * g__v_
            g__pc_ = apply_k

        case union_t.kt.appr1:
            rand_cap = g__k_.rand_cap
            env_cap = g__k_.env_cap
            k_cap = g__k_.k_cap
            g__k_ = kt_appr2(g__v_, k_cap)
            g__e_ = rand_cap
            g__env_ = env_cap
            g__pc_ = value_of_cps

        case union_t.kt.appr2:
            rator_cap = g__k_.rator
            k_cap = g__k_.k_cap
            g__k_ = k_cap
            g__e_ = rator_cap
            g__pc_ = apply_closure

        case union_t.kt.empty:
            jumpout = g__k_.jumpout
            jumpout.switch()

def apply_closure():
    global g__pc_
    global g__e_
    global g__env_
    global g__v_

    match g__e_.type:
        case union_t.clos.closure:
            body = g__e_.body
            env = g__e_.env
            g__env_ = envr_extend(env, g__v_)
            g__e_ = body
            g__pc_ = value_of_cps

def apply_env():
    global g__pc_
    global g__env_
    global g__v_

    match g__env_.type:
        case union_t.envr.empty:
            raise RuntimeError("*v*")

        case union_t.envr.extend:
            env_cap = g__env_.env
            y_cap = g__env_.y
            if (g__v_ == 0):
                g__v_ = y_cap
                g__pc_ = apply_k
            else:
                g__v_ = (g__v_ - 1)
                g__env_ = env_cap
                g__pc_ = apply_env

def value_of_cps():
    global g__pc_
    global g__e_
    global g__k_
    global g__env_
    global g__v_

    match g__e_.type:
        case union_t.expr.const:
            e = g__e_.cexp
            g__v_ = e
            g__pc_ = apply_k

        case union_t.expr.mult:
            xr1 = g__e_.nexpr1
            xr2 = g__e_.nexpr2
            g__k_ = kt_multr1(xr2, g__env_, g__k_)
            g__e_ = xr1
            g__pc_ = value_of_cps

        case union_t.expr.subr1:
            x = g__e_.nexp
            g__k_ = kt_subr1(g__k_)
            g__e_ = x
            g__pc_ = value_of_cps

        case union_t.expr.zero:
            x = g__e_.nexp
            g__k_ = kt_zero(g__k_)
            g__e_ = x
            g__pc_ = value_of_cps

        case union_t.expr._if:
            test = g__e_.test
            conseq = g__e_.conseq
            alt = g__e_.alt
            g__k_ = kt_if(conseq, alt, g__env_, g__k_)
            g__e_ = test
            g__pc_ = value_of_cps

        case union_t.expr.catch:
            body = g__e_.body
            g__env_ = envr_extend(g__env_, g__k_)
            g__e_ = body
            g__pc_ = value_of_cps

        case union_t.expr.pitch:
            k_exp = g__e_.kexp
            v_exp = g__e_.vexp
            g__k_ = kt_pitch(v_exp, g__env_)
            g__e_ = k_exp
            g__pc_ = value_of_cps

        case union_t.expr.let:
            e = g__e_.exp
            body = g__e_.body
            g__k_ = kt_let(body, g__env_, g__k_)
            g__e_ = e
            g__pc_ = value_of_cps

        case union_t.expr.var:
            y = g__e_.n
            g__v_ = y
            g__pc_ = apply_env

        case union_t.expr._lambda:
            body = g__e_.body
            g__v_ = clos_closure(body, g__env_)
            g__pc_ = apply_k

        case union_t.expr.app:
            rator = g__e_.rator
            rand = g__e_.rand
            g__k_ = kt_appr1(rand, g__env_, g__k_)
            g__e_ = rator
            g__pc_ = value_of_cps

def mount_tram():
    global g__pc_
    global g__k_
    global _dismount_blank
    g__k_= kt_empty(_dismount_blank)

    while True:
        greenlet(g__pc_).switch()


def racket_printf(s, *args):
    import re
    print(re.sub(r"~[a-z]", lambda x: "{}", s).format(*args))

if __name__ == '__main__':
    def _blank():
        pass
    jump_mount_tram = greenlet(mount_tram)
    _dismount_blank = greenlet(_blank)
    g__e_ = expr_let(    expr_lambda(    expr_lambda(    expr_if(    expr_zero(    expr_var(0)),     expr_const(1),     expr_mult(    expr_var(0),     expr_app(    expr_app(    expr_var(1),     expr_var(1)),     expr_subr1(    expr_var(0))))))),     expr_mult(    expr_catch(    expr_app(    expr_app(    expr_var(1),     expr_var(1)),     expr_pitch(    expr_var(0),     expr_app(    expr_app(    expr_var(1),     expr_var(1)),     expr_const(5))))),     expr_const(6)))
    g__env_ = envr_empty()
    g__pc_ = value_of_cps
    jump_mount_tram.switch()
    racket_printf("Fact 5: ~s\n", g__v_)
