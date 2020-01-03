import logging as root_logger
import IPython
logging = root_logger.getLogger(__name__)

from enum import Enum

FLAGS = Enum('Flag Types', 'DEFAULT MULTIPLYINCOME UNCANCELLABLE')
GROUP = Enum("Pressure Group Types", "EXTREMIST PROTEST")
SLIDER_T = Enum('Discrete Slider Types', 'DISCRETE PERCENTAGE')
EMOTION_T = Enum('Emotion Valence Type', 'UNKNOWN HIGHBAD HIGHGOOD') 
EDGE_T = Enum("Edge Types", "POLICYGROUP SLIDER INPUT EFFECT PRESSUREREQ PRESSUREPAR PRESSUREVOTER INFLUENCE COSTMUL INCOMEMUL")

def maybeNum(a):
    if isinstance(a, (int, float)):
        return a
    if a.isnumeric():
        if '.' in a:
            return float(a)
        else:
            return int(a)
    return a
    
def he(g, a, b, t):
    if g.has_edge(a, b):
        edge = g[a][b]
        logging.info("{} already has edge to {}. Old: {}, New: {}".format(a,b, edge, t))

class Slider:
    allSliders = []
    
    def __init__(self, id, id_str, stype, default, values):
        self.id = id
        self.id_str = id_str
        self.discrete = SLIDER_T[stype]
        self.default = int(default)
        self.values = [maybeNum(x) for x in values if x != '']
        Slider.allSliders.append(self)

    def __repr__(self):
        return "Slider({}, {})".format(self.id_str, str(self.discrete))
        
class Capital:
    allCapital = []
    
    def __init__(self, intro, cancel, increase, lower):
        self.intro = int(intro)
        self.cancel = int(cancel)
        self.increase = int(increase)
        self.lower = int(lower)
        Capital.allCapital.append(self)

    def __repr__(self):
        return "Capital({}, {}, {}, {})".format(self.intro,
                                                self.cancel,
                                                self.increase,
                                                self.lower)

class Cost:
    allCosts = []
    def __init__(self, minCost, maxCost, mulCost):

        self.min = float(minCost)
        self.max = float(maxCost)
        #mult :: [ ( id_str? ,equation) ]
        #ids of simstates, policies, and situations
        self.mult = {}
        if mulCost.strip() is not '':
            mulSplit = mulCost.split(';')
            for x in mulSplit:
                if x.strip() == '':
                    continue
                k, v = x.strip().split(',')
                self.mult[k] = v
        
        Cost.allCosts.append(self)

    def __repr__(self):
        return "Cost({}, {}, {})".format(self.min,
                                         self.max,
                                         self.mult)

class Income(Cost):
    allIncomes = []
    def __init__(self, minIn, maxIn, mulIn):
        super().__init__(minIn, maxIn, mulIn)
        Income.allIncomes.append(self)

    def __repr__(self):
        if self.mult is not None:
            return "Income({}, {}, {})".format(self.min,
                                               self.max,
                                               self.mult)
        else:
            return "Income({}, {}, N/A)".format(self.min, self.max)
        
class PolicyGroup:
    allGroups = []
    def __init__(self, name):
        self.id_str = name
        self.name = name
        PolicyGroup.allGroups.append(self)

    def __repr__(self):
        return "PolicyGroup({})".format(self.name)
        
class Policy:
    """ Describes a policy. Waht it is, how it can change, its effects, costs etc """
    allPolicies = []
    
    def __init__(self, id, id_str="", name="", slider="", desc="", flag="",
                 capital=[], policy="", cost=[], imTime=0, income=[], effects=[]):
        assert(isinstance(capital, list))
        assert(len(capital) == 4)
        assert(isinstance(cost, list))
        assert(len(cost) == 3)
        assert(isinstance(income, list))
        assert(len(income) == 3)
        
        self.id = id
        self.id_str = id_str
        self.name = name
        #apart from 'default', all are slider.id_str's
        self.slidername = slider
        self.description = desc
        try:
            self.flag = FLAGS[flag]
        except KeyError:
            self.flag = FLAGS.DEFAULT
        self.capital = Capital(*capital)
        #policyGroup :: id_str
        self.policyGroup = policy
        self.cost = Cost(*cost)
        self.implementationTime = int(imTime)
        self.income = Income(*income)

        #always starts with effects[0] == "#effects"
        #effects :: [ (id_str, equation) ]
        #variables can be voters, simstates, situations
        #plus: _global_liberalism, _global_socialism, _security_
        self.effects = {}
        for x in effects[1:]:
            if x.strip() == '':
                continue
            #Key, effect, Maybe(inertia)
            kvi = x.split(',')
            assert(len(kvi) == 2 or len(kvi) == 3)
            self.effects[kvi[0]] = kvi[1:]
        Policy.allPolicies.append(self)

    def __repr__(self):
        return "Policy({}, {}, {})".format(self.name, self.flag, self.policyGroup)

    def link(self, G):
        #link to the policy group
        he(G, self.id_str, self.policyGroup, EDGE_T.POLICYGROUP)
        G.add_edge(self.id_str, self.policyGroup, type=EDGE_T.POLICYGROUP)
        #link to the slider
        he(G, self.id_str, self.slidername, EDGE_T.SLIDER)
        G.add_edge(self.id_str, self.slidername, type=EDGE_T.SLIDER)
        #link Costs
        for k in self.cost.mult.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.COSTMUL)
            G.add_edge(self.id_str, k, type=EDGE_T.COSTMUL)
        #link incomes
        for k in self.income.mult.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.INCOMEMUL)
            G.add_edge(self.id_str, k, type=EDGE_T.INCOMEMUL)        
        #Effect Links:
        for k in self.effects.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.EFFECT)
            G.add_edge(self.id_str, k, type=EDGE_T.EFFECT)

        return G
    
class Approval:
    allApprovals = []
    def __init__(self, leave, join):
        self.leave = leave
        self.join = join
        Approval.allApprovals.append(self)

    def __repr__(self):
        return "Approval({}, {})".format(self.leave, self.join)
        
class Threat:
    allThreats = []
    def __init__(self, base, low, med, high):
        self.base = maybeNum(base)
        self.low = maybeNum(low)
        self.med = maybeNum(med)
        self.high = maybeNum(high)
        Threat.allThreats.append(self)

    def __repr__(self):
        return "Threat({}, {}, {}, {})".format(self.base,
                                               self.low,
                                               self.med,
                                               self.high)

class Radicalisation:
    allRadicalisations = []
    
    def __init__(self, join, leave, rateUp, rateDown, rate):
        self.join =  maybeNum(join)
        self.leave = maybeNum(leave)
        self.rateUp = maybeNum(rateUp)
        self.rateDown = maybeNum(rateDown)
        self.parentRate = maybeNum(rate)
        Radicalisation.allRadicalisations.append(self)

    def __repr__(self):
        return "Radicalisation({}, {}, {}, {}, {})".format(self.join,
                                                           self.leave,
                                                           self.rateUp,
                                                           self.rateDown,
                                                           self.parentRate)
        
class PressureGroup:
    allPressureGroups = []
    
    def __init__(self, i,
                 id_str="",
                 pType=None,
                 name="",
                 approval=[],
                 militancy=0,
                 threat=[],
                 required="",
                 radicalisation=[],
                 groups="",
                 parent=None):
        assert(len(approval) == 2)
        assert(len(threat) == 4)
        assert(len(radicalisation) == 5)
        self.id = i
        self.id_str = id_str
        self.type = pType
        self.name = name
        self.approval = Approval(*approval)
        self.militancy = maybeNum(militancy)
        self.threat = Threat(*threat)
        #required: id_str of Voter
        self.required = required
        
        self.radicalisation = Radicalisation(*radicalisation)
        #groups :: [ id_str : num ]
        #keys are voters
        self.groups = {}
        if groups.strip() != '':
            c = groups.strip().split(',')
            assert(len(c) == 2)
            self.groups[c[0]] = maybeNum(c[1])
        #parent: id_str
        #parent is a pressure group
        self.parent = parent
        PressureGroup.allPressureGroups.append(self)

    def __repr__(self):
        return "PressureGroup({}, {}, {}, {})".format(self.name,
                                                      self.type,
                                                      self.required,
                                                      self.groups,
                                                      self.parent)

    def link(self, G):
        #link to required voter:
        if self.required is not None:
            he(G, self.id_str, self.required, EDGE_T.PRESSUREREQ)
            G.add_edge(self.id_str, self.required, type=EDGE_T.PRESSUREREQ)
        #parent group for extremists
        if self.parent is not None:
            he(G, self.id_str, self.parent, EDGE_T.PRESSUREPAR)
            G.add_edge(self.id_str, self.parent, type=EDGE_T.PRESSUREPAR)

        for k in self.groups.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.PRESSUREVOTER)
            G.add_edge(self.id_str, k, edge=EDGE_T.PRESSUREVOTER)
    
class ValueRange:
    allValueRanges = []
    
    def __init__(self, default, min, max):
        self.default = maybeNum(default)
        self.min = maybeNum(min)
        self.max = maybeNum(max)
        ValueRange.allValueRanges.append(self)

    def __repr__(self):
        return "ValueRange({}, {}, {})".format(self.min,
                                               self.default,
                                               self.max)        
        
class SimState:
    allSimStates = []
    def __init__(self, id, id_str="",
                 name="",
                 desc="",
                 policyGroup="",
                 values=[],
                 emotion="",
                 inputsAndEffects=[]):
        self.id = id
        self.id_str = id_str
        self.name = name
        self.desc = desc
        # policyGroup :: id_str
        self.policyGroup = policyGroup
        self.valueRange = ValueRange(*values)
        try:
            self.emotion = EMOTION_T[emotion]
        except:
            self.emotion = EMOTION_T.UNKNOWN
        #inputs :: [ (id_str, equation) ]
        #input variables are policies, simstates, situations
        #plus _globaleconomy_, _global_liberalism, and _year
        self.inputs = {}
        #effects :: [ (id_str, emotion) ]
        #effects are voters, simstats,
        #and _global_socialism, _global_liberalism, and policy costs 
        self.effects = {}
        #todo: split into inputs and effects  by #'s
        #always starts with an #, may have inputs before the second #, then effects
        isInput = inputsAndEffects.pop(0)
        assert(isInput == '#')
        isInput = True
        while bool(inputsAndEffects):
            next = inputsAndEffects.pop(0)
            if next == '#':
                isInput = False
                continue
            if next.strip() == '':
                continue
            kvi = next.split(',')
            assert(len(kvi) == 2 or len(kvi) == 3)
            if isInput:
                self.inputs[kvi[0]] = kvi[1:]
            else:
                self.effects[kvi[0]] = kvi[1:]
        SimState.allSimStates.append(self)

    def __repr__(self):
        return "SimState({}, {}, {})".format(self.name, self.policyGroup, self.emotion)

    def link(self, G):
        #link to policy group
        he(G, self.id_str, self.policyGroup, EDGE_T.POLICYGROUP)
        G.add_edge(self.id_str, self.policyGroup, type=EDGE_T.POLICYGROUP)
        #link to inputs
        for k in self.inputs.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.INPUT)
            G.add_edge(self.id_str, k, type=EDGE_T.INPUT)
        #link to outpus
        for k in self.effects.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.EFFECT)
            G.add_edge(self.id_str, k, type=EDGE_T.EFFECT)

        return G
    
class Bookends:
    allBookends = []
    def __init__(self, start, end):
        self.start = maybeNum(start)
        self.end = maybeNum(end)
        Bookends.allBookends.append(self)

    def __repr__(self):
        return "Bookend({}, {})".format(self.start, self.end)
        
class Situation:
    allSituations = []
    def __init__(self, id,
                 id_str="",
                 name="",
                 desc="",
                 policyGroup="",
                 text=[],
                 positive=False,
                 triggers=[],
                 perTurn=[],
                 inputsAndEffects=[]):
        self.id = id
        self.id_str = id_str
        self.name = name
        self.desc = desc
        self.policyGroup = policyGroup
        self.text = Bookends(*text)
        self.positive=bool(positive)
        self.triggers = Bookends(*triggers)
        self.perTurnCost = maybeNum(perTurn[0])
        self.perTurnIncome = maybeNum(perTurn[1])
        # inputs :: [ (id_str, equation ) ]
        #keys are policies, simstats, situations
        #plus: _prereq_, _default_, _effectivedebt_, _global_interest_rates_
        self.inputs = {}
        # effects :: [ (id_str, equation) ]
        #keys are: simstates, voters, and policy costs and incomes, voter frequencies,
        # and _global_socialism, _security_, _All_
        self.effects = {}
        #just a separating # in the middle
        isInput = True
        while bool(inputsAndEffects):
            next = inputsAndEffects.pop(0).strip()
            if next == '':
                continue
            if next == '#':
                isInput = False
                continue
            kvi = next.split(',')
            assert(len(kvi) == 2 or len(kvi) == 3)
            if isInput:
                self.inputs[kvi[0]] = kvi[1:]
            else:
                self.effects[kvi[0]] = kvi[1:]
        Situation.allSituations.append(self)
                 
    def __repr__(self):
        return "Situation({}, {}, {}, {}, {})".format(self.name, self.policyGroup,
                                                      self.positive, self.perTurnCost,
                                                      self.perTurnIncome)

    def link(self, G):
        #link the policy group
        he(G, self.id_str, self.policyGroup, EDGE_T.POLICYGROUP)
        G.add_edge(self.id_str, self.policyGroup, type=EDGE_T.POLICYGROUP)
        #link the inputs:
        for k in self.inputs.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.INPUT)
            G.add_edge(self.id_str, k, type=EDGE_T.INPUT)
        #and the outputs:
        for k in self.effects.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.EFFECT)
            G.add_edge(self.id_str, k, type=EDGE_T.EFFECT)

        return G
        
class Voter:
    allVoters = []
    def __init__(self, id, id_str="",
                 name="",
                 plural="",
                 overrides=0,
                 default=0,
                 percentage=0,
                 desc="",
                 influences=[]):

        self.id = id
        self.id_str = id_str
        self.name = name
        self.plural = plural
        self.overrides = bool(overrides)
        self.default = maybeNum(default)
        self.percentage = maybeNum(percentage)
        self.desc = desc
        #influences :: { id_str : value }
        #keys are other voters
        self.influences = {}
        for x in influences[1:]:
            x = x.strip()
            if x == '':
                continue
            k,v = x.split(',')
            self.influences[k] = maybeNum(v)
        #possible:
        #self.freq
        #self.income
        #self.perc
        Voter.allVoters.append(self)

    def __repr__(self):
        return "Voter({})".format(self.name)
        
    def link(self, G):
        for k in self.influences.keys():
            if k[0] != '_' and '_' in k:
                s = k.split('_')
                logging.info("Using key {} instead of {}".format(s[0], k))
                k = s[0]
            he(G, self.id_str, k, EDGE_T.INFLUENCE)
            G.add_edge(self.id_str, k, type=EDGE_T.INFLUENCE)
        return G

    
#------------------------------
# Utilities makers:

def make_policy(i, args):
    assert(len(args) > 18)
    return Policy(i, id_str=args[0],
                  name=args[1], slider=args[2], desc=args[3],
                  flag=args[4],
                  capital=args[5:9], policy=args[9],
                  cost=args[10:13], imTime=args[13],
                  income=args[14:17], effects=args[17:])

def make_pressure_group(i, args):
    if args[0] == '':
        return None
    
    if len(args) < 14:
        logging.warning("Skipping empty pressure group")
        return None
    if i < 12:
        return PressureGroup(i, id_str=args[1],
                             pType=GROUP.PROTEST,
                             name=args[3],
                             approval=args[4:6],
                             militancy=args[6],
                             threat=args[7:11],
                             required=args[11],
                             radicalisation=[0, 0] + args[12:14] + [0],
                             groups=args[14])                             
    else:
        return PressureGroup(i, id_str=args[1],
                             pType=GROUP.EXTREMIST,
                             name=args[3],
                             approval=args[4:6],
                             militancy=args[6],
                             threat=args[7:11],
                             required=args[11],
                             radicalisation=args[12:14] + [0,0] + [args[15]],
                             parent=args[14])
        
def make_simstate(i, args):
    return SimState(i, id_str=args[1], name=args[2], desc=args[3], policyGroup=args[4],
                    values=args[5:8], emotion=args[8], inputsAndEffects=args[10:])

def make_situation(i, args):
    return Situation(i, id_str=args[1],
                     name=args[2],
                     desc=args[3],
                     policyGroup=args[4],
                     text=args[6:8],
                     positive=args[8],
                     triggers=args[9:11],
                     perTurn=args[11:13],
                     inputsAndEffects=args[13:])

def make_slider(i, args):
    return Slider(i, id_str=args[1],
                  stype=args[2],
                  default=args[3],
                  values=args[4:])

def make_voter(i, args):
    return Voter(i,
                 id_str=args[1],
                 name=args[2],
                 plural=args[3],
                 overrides=args[5],
                 default=args[6],
                 percentage=args[7],
                 desc=args[8],
                 influences=args[10:])
