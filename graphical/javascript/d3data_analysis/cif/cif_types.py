#Although rules have names, they are listed because they can be anonymous
from enum import Enum, EnumMeta
import IPython
import logging as root_logger
logging = root_logger.getLogger(__name__)


COMP = Enum('Comparators', 'PLUS SUB GT LT')
COMP_LOOKUP = {
    "-" : COMP.SUB,
    "+" : COMP.PLUS,
    "greaterthan" : COMP.GT,
    "lessthan" : COMP.LT
}
ACTOR_T = Enum('Actor Type', 'INITIATOR RESPONDER OTHER NULL')
PREDICATE_T = Enum("Predicate", "NETWORK TRAIT STATUS RELATIONSHIP SFDBLABEL CKBENTRY")
NETWORK_T = Enum("Network Type", "BUDDY ROMANCE COOL")


class SocialGame:
    all_games = []

    
    def __init__(self, name, data=None):
        self.name = name

        #Lists of rules
        self.intents = []
        #Dictionary of instantiations by name 
        self.instantiations = []
        self.preconditions = []
        #influence rules as lists of rules each
        self.influence_rules = {
            "initiator" : [],
            "responder" : []
            }
        #A List of effects
        self.effects = []

        assert(name not in SocialGame.all_games)
        SocialGame.all_games.append(self)

    def __repr__(self):
        return "SocialGame({}, intents:{}, instants:{}, precons:{}, ini_rules:{}, res_rule:{}, effects:{}".format(self.name, len(self.intents), len(self.instantiations), len(self.preconditions), len(self.influence_rules['initiator']), len(self.influence_rules['responder']), len(self.effects))
        

class Rule:
    all_rules = []
    
    def __init__(self, name=None, weight=None, effect=False):
        self.name = name
        self.predicates = []
        self.weight = weight
        self.effect = effect

        Rule.all_rules.append(self)
        
    def __repr__(self):
        return "Rule({}, predicates:{}, weight:{})".format(self.name, len(self.predicates), self.weight)

class Predicate:
    all_predicates = []
    
    def __init__(self, type=None, subtype=None, pair=("",""),
                 comparator=None, value=0, negated=False,
                 intent=False, sfdb_flag=False, window=0,
                 unique_flag=None, num_unique=0):
        self.type = type
        self.subtype = subtype
        self.actors = pair
        self.comparator = comparator
        self.value = value
        self.negated = negated
        self.intent = intent
        self.sfdb_flag = sfdb_flag
        self.window = window
        self.unique_flag = unique_flag
        self.num_unique = num_unique

        Predicate.all_predicates.append(self)
        
    def __repr__(self):
        neg = ""
        if self.negated:
            neg = "~"
        alt = "{}{}({}, {}, {})".format(neg, str(self.type), str(self.subtype), str(self.comparator), str(self.value))
        return alt
    
#return "Predicate({}, {}, focus:{})".format(str(self.type), str(self.subtype), str(self.actors[0]))

    
        
class Instantiation:
    all_instantiations = []
    
    def __init__(self, name):
        self.name = name
        self.conditionals = []
        self.toc = []
        self.dialogue = []

        Instantiation.all_instantiations.append(self)
            
    def __repr__(self):
        return "Instantiation({}, {}, {})".format(self.name, len(self.conditionals), len(self.dialogue))
    
class Effect:
    all_effects = []
    
    def __init__(self, inst_id):
        self.instantiation_id = inst_id
        self.condition_rules = []
        self.change_rules = []
        self.performance_realization = {}
        self.accept = False

        Effect.all_effects.append(self)

    def __repr__(self):
        return "Effect({}, {})".format(len(self.condition_rules), len(self.change_rules))
        
class LineOfDialogue:
    all_dialogue = []
    
    def __init__(self, data):
        self.data = data

        LineOfDialogue.all_dialogue.append(self)

    def __repr__(self):
        return "LineOfDialogue"
        

class Microtheory:
    all_microtheories = []
    
    def __init__(self, name, definition, initiator_rules, responder_rules):
        self.name = name
        self.definition = definition
        self.initiator_rules = initiator_rules
        self.responder_rules = responder_rules

        Microtheory.all_microtheories.append(self)

    def __repr__(self):
        return "Microtheory({}, i_rules:{}, r_rules:{})".format(self.name, len(self.initiator_rules), len(self.responder_rules))
        
##############################
# Construction Utilities
##############################
def makeSocialGame(data):
    game = SocialGame(data['name'])
    #make intents
    if len(data['Intents']) != 0:
        assert('Rule' in data['Intents'])
        assert(isinstance(data['Intents']['Rule'], dict))
        intent_rule = makeRule(data['Intents']['Rule'])
        game.intents.append(intent_rule)

    if len(data['Preconditions']) == 0:
        logging.info("No preconditions")
    elif isinstance(data['Preconditions'], dict) and isinstance(data['Preconditions']['Rule'], dict):
        game.preconditions.append(makeRule(data['Preconditions']['Rule']))
    elif isinstance(data['Preconditions'], dict) and isinstance(data['Preconditions']['Rule'], list):
        game.preconditions = [makeRule(x) for x in data['Preconditions']['Rule']]
                    

    
    #make influence rules
    if len(data['InitiatorInfluenceRuleSet']) == 0:
        logging.info("No Initiator influence rules")
    elif isinstance(data['InitiatorInfluenceRuleSet']['InfluenceRule'], list):
        assert('InfluenceRule' in data['InitiatorInfluenceRuleSet'])
        game.influence_rules["initiator"] = [makeRule(x) for x in data['InitiatorInfluenceRuleSet']['InfluenceRule']]
    elif isinstance(data['InitiatorInfluenceRuleSet']['InfluenceRule'], dict):
        game.influence_rules['initiator'].append(makeRule(data['InitiatorInfluenceRuleSet']['InfluenceRule']))

    if len(data['ResponderInfluenceRuleSet']) == 0:
       logging.info("No Responder Influence rules")
    elif isinstance(data['ResponderInfluenceRuleSet']['InfluenceRule'], list):
        game.influence_rules['responder'] = [makeRule(x) for x in data['ResponderInfluenceRuleSet']['InfluenceRule']]
    elif isinstance(data['ResponderInfluenceRuleSet']['InfluenceRule'], dict):
        game.influence_rules['responder'].append(makeRule(data['ResponderInfluenceRuleSet']['InfluenceRule']))

    #make instantiations
    if len(data['Instantiations']) == 0:
        logging.info("No instantiations")
    else:
        assert(isinstance(data['Instantiations']['Instantiation'], list))
        game.instantiations = [makeInstantiation(x) for x in data['Instantiations']['Instantiation']]
    
    #make effects
    if len(data['Effects']) == 0:
        logging.info("No Effects")
    else:
        assert(isinstance(data['Effects']['Effect'], list))
        game.effects = [makeEffect(x) for x in data['Effects']['Effect']]

    return game

def to_bool(s):
    assert(isinstance(s, str))
    if s == "true":
            return True
    return False

def to_int(s):
    assert(isinstance(s, str))
    return int(s)

def lookup(s, e):
    assert(isinstance(e, EnumMeta))
    assert(isinstance(s, str))
    assert(hasattr(e, s.replace(' ', '').upper()))
    return e[s.replace(' ', '').upper()]


def makeRule(data, effect=False):
    rule = Rule(effect=effect)

    if 'name' in data:
        rule.name = data['name']

    if 'Predicate' not in data:
        logging.info("No Predicate")
    elif isinstance(data['Predicate'], dict):
        rule.predicates.append(makePredicate(data['Predicate']))
    elif isinstance(data["Predicate"], list):
        rule.predicates = [makePredicate(x) for x in data['Predicate']]
        
    if 'weight' in data:
        rule.weight = to_int(data['weight'])

        
    return rule


def makePredicate(data):
    ptype = None
    subtype = None
    actor_pair = None
    comp = None
    value = None
    negated = None
    intent = None
    sfdbFlag = None
    window = None
    unique_flag = None
    num_unique = None

    
    ptype = lookup(data['type'], PREDICATE_T)
    
    if ptype is PREDICATE_T.NETWORK:
        assert('networkType' in data)
        subtype = lookup(data['networkType'], NETWORK_T)
    elif ptype is PREDICATE_T.TRAIT:
        assert('trait' in data)
        subtype = data['trait']
    elif ptype is PREDICATE_T.STATUS:
        assert('status' in data)
        subtype = data['status']
    elif ptype is PREDICATE_T.RELATIONSHIP:
        assert('relationship' in data)
        subtype = data['relationship']
    elif ptype is PREDICATE_T.SFDBLABEL and 'label' in data:
        subtype = data['label']
    elif ptype is PREDICATE_T.CKBENTRY:
        f = None
        s = None
        if 'firstSubjective' in data:
            f = data['firstSubjective']
        if 'secondSubjective' in data:
            s = data['secondSubjective']
        subtype = [f, s]

    first = None
    second = None
    if 'first' in data:
        first = lookup(data['first'], ACTOR_T)
    if 'second' in data:
        second = lookup(data['second'], ACTOR_T)
    
    actor_pair = (first, second)
    if 'comparator' in data and data['comparator'] in COMP_LOOKUP:
        comp = COMP_LOOKUP[data['comparator']]
    elif 'comparator' in data:
        logging.info("Comparator not recognised: {}".format(data['comparator']))
        comp = data['comparator']
    else:
        logging.debug("No comparator for predicate")

        

    if 'value' in data:
        value = to_int(data['value'])
    if 'negated' in data:
        negated = to_bool(data['negated'])
    if 'intent' in data:
        intent = to_bool(data['intent'])

    assert('isSFDB' in data)
    sfdbFlag = to_bool(data['isSFDB'])
    assert('window' in data)
    window = to_int(data['window'])

    assert('numTimesUniquelyTrueFlag' in data)
    unique_flag = to_bool(data['numTimesUniquelyTrueFlag'])
    assert('numTimesUniquelyTrue' in data)
    num_unique = to_int(data['numTimesUniquelyTrue'])

    predicate = Predicate( type=ptype,
                           subtype=subtype,
                           pair=actor_pair,
                           comparator=comp,
                           value=value,
                           negated=negated,
                           intent=intent,
                           sfdb_flag=sfdbFlag,
                           window=window,
                           unique_flag=unique_flag,
                           num_unique=num_unique
    )

    return predicate

        
def makeInstantiation(data):
    assert('name' in data)
    assert('ConditionalRules' in data)
    assert('LineOfDialogue' in data)
    inst = Instantiation(data['name'])
    assert(isinstance(data['LineOfDialogue'], list))
    inst.dialogue = [makeDialogue(x) for x in data['LineOfDialogue']]
    inst.toc = [data[x] for x in ["ToC1", "ToC2", "ToC3"]]
    assert(isinstance(data['ConditionalRules'], dict))
    if 'Rule' in data['ConditionalRules'] and isinstance(data['ConditionalRules']['Rule'], list):
        inst.conditionals = [makeRule(x) for x in data['ConditionalRules']['Rule']]
    elif 'Rule' in data['ConditionalRules'] and isinstance(data['ConditionalRules']['Rule'], dict):
        inst.conditionals.append(makeRule(data['ConditionalRules']['Rule']))
    elif len(data['ConditionalRules']) != 0:
        logging.info("Conditional Rules not empty")
        IPython.embed(simple_prompt=True)
        
    return inst

    
def makeDialogue(data):
    dialogue = LineOfDialogue(data)
    return dialogue


def makeEffect(data):
    effect = Effect(data['instantiationID'])

    assert('accept' in data)
    effect.accept = to_bool(data['accept'])

    if isinstance(data['ConditionRule'], dict) and len(data['ConditionRule']) != 0:
        effect.condition_rules.append(makeRule(data['ConditionRule']))
    elif isinstance(data['ConditionRule'], list):
        effect.condition_rules = [makeRule(x) for x in data['ConditionRule']]
    if isinstance(data['ChangeRule'], dict) and len(data['ChangeRule']) != 0:
        effect.change_rules.append(makeRule(data['ChangeRule'], effect=True))
    elif isinstance(data['ChangeRule'], list):
        effect.change_rules = [makeRule(x) for x in data['ChangeRule']]

    effect.performance_realization = data['PerformanceRealization']
    return effect
    

def makeMicrotheory(data):
    assert('Name' in data and '$t' in data['Name'])
    assert(len(data['Name']) == 1)
    name = data['Name']['$t']

    definition = None
    responder_rules = []
    initiator_rules = []

    #create the definition rule
    if 'Rule' in data['Definition']:
        definition = makeRule(data['Definition']['Rule'])
    #create the initiator rules
    assert('InitiatorInfluenceRuleSet' in data)
    if 'InfluenceRule' in data['InitiatorInfluenceRuleSet'] and \
       isinstance(data['InitiatorInfluenceRuleSet']['InfluenceRule'], list):
        initiator_rules = [makeRule(x) for x in data['InitiatorInfluenceRuleSet']['InfluenceRule']]
           
    #create the responder rules
    assert('ResponderInfluenceRuleSet' in data)
    if 'InfluenceRule' in data['ResponderInfluenceRuleSet'] and \
       isinstance(data['ResponderInfluenceRuleSet']['InfluenceRule'], list):
        responder_rules = [makeRule(x) for x in data['ResponderInfluenceRuleSet']['InfluenceRule']]

    
    return Microtheory(name, definition, initiator_rules, responder_rules)
    
    
