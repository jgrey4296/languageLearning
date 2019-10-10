/* jshint esversion : 6 */
define(['lodash'],function(_){

    let currId = 0,
        Types = {
            Rule : null,
            Predicate : null,
            MicroTheory : null,
            SocialGame : null,
            Effect : null,
            Instantiation : null,
            LineOfDialogue : null
        };

    /**
       Rule
     */
    Types.Rule = function(rule){
        this.id = currId++;
        //name :: string
        this.name = rule.name;
        //internalId :: string
        this.internalId = rule.id;
        //predicates :: [Predicate]
        this.predicates = [];
        //weight :: Number
        this.weight = null;

        
        if(rule.Predicate instanceof Array && rule.Predicate.length > 0){
            this.predicates = rule.Predicate.map(d=>new Types.Predicate(d));
        }else{
            if(_.keys(rule.Predicate).length > 0){
                this.predicates.push(new Types.Predicate(rule.Predicate));
            }
        }
        if(rule.weight !== undefined){
            if(isNaN(Number(rule.weight))){
                throw new Error('rule weight is NaN');
            }
            this.weight = Number(rule.weight);
        }
    };

    //an individual predicate
    //for the moment import and convert carte blanche
    Types.Predicate = function(pred){
        this.id = currId++;        
        _.keys(pred).forEach(d=>{
            if(pred[d] === 'false'){
                this[d] = false;
            }else if(pred[d] === 'true'){
                this[d] = true;
            }else if(!isNaN(Number(pred[d]))){
                this[d] = Number(pred[d]);
            }else if(pred[d] === 'null'){
                this[d] = null;
            }else{
                this[d] = pred[d];
            }
        });
    };

    //top level microtheory
    Types.MicroTheory = function(mt){
        if(mt.Definition instanceof Array){
            throw new Error('Definition is an array');
        }else if(mt.Definition.Rule instanceof Array){
            throw new Error('Definition.Rule is an array');
        }else if(mt.ResponderInfluenceRuleSet instanceof Array && mt.ResponderInfluenceRuleSet.length > 0 || _.keys(mt.ResponderInfluenceRuleSet).length > 0 || mt.ResponderInfluenceRuleSet.InfluenceRule !== undefined){
            throw new Error('there are Responder Influence Rules in this mt');
        }
        
        this.id = currId++;
        //name :: string
        this.name = mt.Name.$t;
        //definition :: Rule
        this.definition = new Types.Rule(mt.Definition.Rule);
        //initiatorRuleSet :: [Rule]
        this.initiatorRuleSet = [];

        //setup initiatorRuleSet
        if(!(mt.InitiatorInfluenceRuleSet.InfluenceRule instanceof Array)){
            if(_.keys(mt.InitiatorInfluenceRuleSet.InfluenceRule).length > 0){
                this.initiatorRuleSet.push(new Types.Rule(mt.InitiatorInfluenceRuleSet.InfluenceRule));
            }
        }else{
            this.initiatorRuleSet = mt.InitiatorInfluenceRuleSet.InfluenceRule.map(d=>new Types.Rule(d));
        }
        
                
    };

    Types.MicroTheory.prototype.getRules = function(){
        return [this.definition].concat(this.initiatorRuleSet);
    };

    Types.MicroTheory.prototype.getPredicates = function(){
        let rules = this.getRules();            
        return rules.map(d=>d.predicates);
    };


    //----------
    //Social Games:
    Types.SocialGame = function(socGame){
        this.id = currId++;
        //name :: string
        this.name = socGame.name;
        //effects :: [Effect]
        this.effects = [];
        //preconditions :: [Rule]
        this.preconditions = [];
        //instantiations :: [Instantiation]
        this.instantiations = [];
        //intents :: Rule
        this.intents = null;
        //patsy :: Rule
        this.patsy = null;
        //initiatorRules :: [Rule]
        this.initiatorRules = [];
        //responderRules :: [Rule]
        this.responderRules = [];

        
        if(socGame.Intents.Rule !== undefined && _.keys(socGame.Intents.Rule) > 0){
            this.intents = new Types.Rule(socGame.Intents.Rule);
        }
        //patsy rule?
        if(socGame.PatsyRule !== undefined && !(socGame.PatsyRule instanceof Array) && _.keys(socGame.PatsyRule.Rule).length > 0){
            this.patsy = new Types.Rule(socGame.PatsyRule.Rule);
        }
        //setup effects:
        if(!(socGame.Effects.Effect instanceof Array)){
            this.effects.push(new Types.Effect(socGame.Effects.Effect));
        }else{
            this.effects = socGame.Effects.Effect.map(d=>new Types.Effect(d));
        }
        //setup preconditions:
        if(!(socGame.Preconditions instanceof Array)){
            if(socGame.Preconditions.Rule !== undefined){
                this.preconditions.push(new Types.Rule(socGame.Preconditions.Rule));
            }
        }
        //setup instantiations:
        //check for non-array:
        if(!(socGame.Instantiations.Instantiation instanceof Array) && _.keys(socGame.Instantiations.Instantiation).length > 0){
            this.instantiations.push(new Types.Instantiation(socGame.Instantiations.Instantiation));
        }else if(socGame.Instantiations.Instantiation.length > 0){
            this.instantiations = socGame.Instantiations.Instantiation.map(d=>new Types.Instantiation(d));
        }

        //setup initiator influence rules:
        if(socGame.InitiatorInfluenceRuleSet.InfluenceRule instanceof Array){
            if(socGame.InitiatorInfluenceRuleSet.InfluenceRule.length > 0){
                this.initiatorRules = socGame.InitiatorInfluenceRuleSet.InfluenceRule.map(d=>new Types.Rule(d));
            }
        } else if(!(socGame.InitiatorInfluenceRuleSet.InfluenceRule instanceof Array) && _.keys(socGame.InitiatorInfluenceRuleSet.InfluenceRule).length > 0){
            this.initiatorRules.push(new Types.Rule(socGame.InitiatorInfluenceRuleSet.InfluenceRule));
        }else if(_.keys(socGame.InitiatorInfluenceRuleSet).length > 0){
            throw new Error('there are keys in the initiator influence rule set');
        }
        
        //setup responder rules
        if(socGame.ResponderInfluenceRuleSet.InfluenceRule instanceof Array){
            if(socGame.ResponderInfluenceRuleSet.InfluenceRule.length > 0){
                this.responderRules = socGame.ResponderInfluenceRuleSet.InfluenceRule.map(d=>new Types.Rule(d));
            }
        }else if(!(socGame.ResponderInfluenceRuleSet.InfluenceRule instanceof Array) && _.keys(socGame.ResponderInfluenceRuleSet.InfluenceRule).length > 0){
            this.responderRules.push(new Types.Rule(socGame.ResponderInfluenceRuleSet.Influence));
        }else if(_.keys(socGame.ResponderInfluenceRuleSet).length > 0){
            throw new Error('there are keys in the responder influence rule set');
        }
        
    };

    //effect
    Types.Effect = function(effect){
        this.id = currId++;
        //changeRule :: [Predicate]
        this.changeRule = [];
        //conditionRule :: [Predicate]
        this.conditionRule = [];
        //performance :: string
        this.performance = effect.PerformanceRealization.$t;
        //accept :: Boolean
        this.accept = effect.accept === 'true' ? true : effect.accept === 'false' ? false : null;
        //internalId :: String
        this.internalId = effect.id;
        //instantiationId :: String
        this.instantiationId = effect.instantiationID;

        if(this.accept === null){
            console.log(effect);
            throw new Error('unrecognised accept value');
        }
        //setups:
        //change rule
        if(!(effect.ChangeRule.Predicate instanceof Array)){
            if(_.keys(effect.ChangeRule.Predicate).length > 0){
                this.changeRule.push(new Types.Predicate(effect.ChangeRule.Predicate));
            }
        }else{
            if(effect.ChangeRule.Predicate.length > 0){
                this.changeRule = effect.ChangeRule.Predicate.map(d=>new Types.Predicate(d));
            }
        }
        //condition rule:
        if(!(effect.ConditionRule.Predicate instanceof Array)){
            if(_.keys(effect.ConditionRule.Predicate).length > 0){
                this.conditionRule.push(new Types.Predicate(effect.ConditionRule.Predicate));
            }
        }else{
            if(effect.ConditionRule.Predicate.length > 0){
                this.conditionRule = effect.ConditionRule.Predicate.map(d=>new Types.Predicate(d));
            }
        }
    };

    //instantiation
    Types.Instantiation = function(instantiation){
        this.id = currId++;
        this.name = instantiation.name;
        this.internalId = instantiation.id;
        //conditions :: [Rule]
        this.conditions = [];
        //lineOfDialogue :: [LineOfDialogue]
        this.lineOfDialogue = [];
        //sfdb/ckb value update strings
        this.ToC = [];

        if(instantiation.ConditionalRules.Rule === undefined){
            //do nothing, no conditions
        }else if(!(instantiation.ConditionalRules.Rule instanceof Array)){
            //if ConditionalRules is singular
            this.conditions.push(new Types.Rule(instantiation.ConditionalRules.Rule));
        }else if(instantiation.ConditionalRules.Rule.length > 0){
            //if conditionalRules is plural:
            this.conditions = instantiation.ConditionalRules.Rule.map(d=>new Types.Rule(d));
        }

        //set up line of dialogue:
        this.lineOfDialogue = instantiation.LineOfDialogue.map(d=>new Types.LineOfDialogue(d));

        //setup ToC strings:
        if(instantiation.ToC1.$t !== undefined){
            this.ToC.push(instantiation.ToC1.$t);
        }
        if(instantiation.ToC2.$t !== undefined){
            this.ToC.push(instantiation.ToC2.$t);
        }
        if(instantiation.ToC3.$t !== undefined){
            this.ToC.push(instantiation.ToC3.$t);
        }
        
    };

    //copy over everything carte blance, refine later
    //except for rule: build that properly
    //todo: build a generic 'character action' component
    Types.LineOfDialogue = function(loc){
        this.rule = loc.Rule !== undefined ? new Types.Rule(loc.Rule) : null;
        this.initiatorValues = {};
        this.responderValues = {};
        this.otherValues = {};
        
        let keys = _.reject(_.keys(loc),d=>d === 'Rule');
        keys.forEach(d=>{
            //setup the value
            let val = null;
            if(loc[d] === 'false'){
                val = false;
            }else if(loc[d] === 'true'){
                val = true;
            }else if(!isNaN(Number(loc[d]))){
                val = Number(loc[d]);
            }else if(loc[d] === 'null'){
                val = null;
            }else{
                val = loc[d];
            }
            //store in the correct place:
            if(/initiator/.test(d)){
                this.initiatorValues[d] = val;
            }else if(/responder/.test(d)){
                this.responderValues[d] = val;
            }else if(/other/.test(d)){
                this.otherValues[d] = val;
            }else{
                this[d] = val;
            }
        });        
    };
    //----------
    return Types;
});
