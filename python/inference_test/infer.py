"""
A Type Inference Algorithm
"""

#Define base Mono Types (var, func, const, list...)
class MonoType:

    def __init__(self, name, components):
        assert(isinstance(components, list))
        self.name = name
        self.components = components

    #define mono types' free variables
    def free_vars(self):
        free = [x.free_vars() for x in self.components if isinstance(x, MonoType)]
        free += [x for x in self.components if not isinstance(x, MonoType)]
        return free


    #define how to apply a substitution
    def apply_substitution(self, sub):
        components = []
        for x in self.components:
            if isinstance(x, MonoType):
                components.append(x.apply_substitution(sub))
            else:
                #lookup
                components.append(sub(x))
        return MonoType(self.name, components)

    def unify(self, other):
        #TODO


    def __contains__(self, other):


    def bind(self, other):
        # name -> mtype -> subst


#define Poly Type
class PolyType:

    def __init__(self, name, foralls, mType):
        self.name = name
        self.foralls = foralls
        self.mType = mType

    #define a poly types' free variables
    def free_vars(self):
        mType_free = self.mType.free_vars()
        return mType_free.difference(self.foralls)

    #define how to apply a substitution
    def apply_substitution(self, sub):
        new_sub = sub - self.foralls
        # return PolyType of foralls and mType.apply_substitution(sub')
        new_mtype = self.mType.apply_substitution(new_sub)
        return PolyType(self.name, self.foralls, new_mtype)

#Define an environment
class Environment:

    def __init__(self, components):
        assert(isinstance(components, dict))
        self.mapping = components

    #define how to get a env's free variables
    def free_vars(self):
        return set([y for x in self.mapping.values() for y in x.free_vars()])


    #define how to apply a substitution
    def apply_substitution(self, sub):
        #apply the substitution to all ptypes in the environment
        new_map = {x: y.apply_substitution(sub) for x,y in self.mapping.items()}
        return Environment(new_map)

    def infer(self, expression):
        #run the inference on the expression


    def unify(self, a, b):
        #run the unification

#define a substitution
class Substitution:

    def __init__(self, mapping):
        self.mapping = mapping

    def apply_substitution(self, sub):
        #stack subs

    def __add__(self, sub):
        return self.apply_substitution(sub)

    def __sub__(self, lst):
        new_map = {x: y for x,y in self.mapping.items if x not in lst}
        return Substitution(new_map)

    def __call__(self, x):
        if x in self.mapping:
            return self.mapping[x]
        else:
            return x


#--------------------------------------------------
# ERRORS:
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
