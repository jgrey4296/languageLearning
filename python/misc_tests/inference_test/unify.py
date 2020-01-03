"""
A Unification algorithm
based on https://eli.thegreenplace.net/2018/unification/
"""
from util import ExOp, Term, ExConst, ExVar

def occurs_check(v, term, subst):
    """Does the variable v occur anywhere inside term?

    Variables in term are looked up in subst and the check is applied
    recursively.
    """
    assert isinstance(v, ExVar)
    if v == term:
        return True
    elif isinstance(term, ExVar) and term.name in subst:
        return occurs_check(v, subst[term.name], subst)
    elif isinstance(term, list):
        return all([occurs_check(v,x,subst) for x in term])
    else:
        return False

def unify_sen(x,y, subst=None):
    if subst is None:
        subst = {}
    for a,b in zip(x,y):
        subst = unify(a,b, subst)
        if subst is None:
            raise Exception("Unification failed on: {} | {}".format(a,b))
    return subst

def unify(x, y, subst):
    """Unifies term x and y with initial subst.

    Returns a subst (map of name->term) that unifies x and y, or None if
    they can't be unified. Pass subst={} if no subst are initially
    known. Note that {} means valid (but empty) subst.
    """
    if subst is None:
        return None
    elif x == y:
        return subst
    elif isinstance(x, ExVar):
        return unify_variable(x, y, subst)
    elif isinstance(y, ExVar):
        return unify_variable(y, x, subst)
    else:
        return None

def unify_variable(v, x, subst):
    """Unifies variable v with term x, using subst.

    Returns updated subst or None on failure.
    """
    assert isinstance(v, ExVar)
    if v.name in subst:
        return unify(subst[v.name], x, subst)
    elif isinstance(x, ExVar) and x.name in subst:
        return unify(v, subst[x.name], subst)
    elif occurs_check(v, x, subst):
        return None
    else:
        # v is not yet in subst and can't simplify x. Extend subst.
        return {**subst, v.name: x}

def apply_unifier(x, subst):
    """Applies the unifier subst to term x.

    Returns a term where all occurrences of variables bound in subst
    were replaced (recursively); on failure returns None.
    """
    if subst is None:
        return None
    elif len(subst) == 0:
        return x
    elif isinstance(x, Const):
        return x
    elif isinstance(x, Var):
        if x.name in subst:
            return apply_unifier(subst[x.name], subst)
        else:
            return x
    elif isinstance(x, App):
        newargs = [apply_unifier(arg, subst) for arg in x.args]
        return App(x.fname, newargs)
    else:
        return None



class InferenceError:

    def __init__(self):
        return

class CannotUnifyError(InferenceError):

    def __init__(self, type1, type2):
        self.t1 = type1
        self.t2 = type2

class OccursCheckFailedError(InferenceError):

    def __init__(self, name, mtype):
        self.name = name
        self.mtype = mtype

class UnknownIdentifierError(InferenceError):

    def __init__(self, name):
        self.name = name
