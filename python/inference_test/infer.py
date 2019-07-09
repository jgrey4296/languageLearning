"""
A Type Inference Algorithm
"""

class MonoType:
    #Define base Mono Types (var, func, const, list...)

    def __init__(self, name, components):
        assert(isinstance(components, list))
        self.name = name
        #Components is a list of: MonoTypes, Lists (for functions) and Tries (for structures)
        self.components = components

    def free_vars(self):
        #define mono types' free variables
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
        return

    def __contains__(self, other):
        return

    def bind(self, other):
        # name -> mtype -> subst
        return

    def generalize(self, env):
        return PolyType()


class PolyType:
    #define Poly Type

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

    def instantiate(self):
        return MonoType()


class Environment:
    #Define an environment

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
        return

    def unify(self, a, b):
        #run the unification
        return


class Substitution:
    #define a substitution

    def __init__(self, mapping):
        self.mapping = mapping

    def apply_substitution(self, sub):
        #stack subs
        return

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
