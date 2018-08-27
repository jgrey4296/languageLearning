if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}


define(['lodash','./ActionFunctions','./StatusEffectFunctions'],function(_,ActionFunctions,StatusEffectFunctions){
    "use strict";
    let Character = function(charData){
        
        this.name = charData.name;
        
        //Stats:
        //Sets up Health,speed,attack,defense,exp,type1,type2:
        this.stats = charData.stats;

        //Status modifiers that happen each turn, for n turns
        //Of form: [statusType,[params],turnsLeft,isInitial]
        this.statusEffects = [];

        //Actions the character is able to perform
        let allActions = charData.actions.map(function(d){
            if(ActionFunctions[d] !== undefined) return ActionFunctions[d];
            
        }).filter(function(d){return d !== undefined; });

        this.actions = _.sampleSize(allActions,4);
        
    };

    Character.prototype.printStats = function(){
        return this.name + " : " + _.toPairs(this.stats).map(function(d){
            return d.join(" : ");
        }).join(", \n\t  ");
    };
    
    Character.prototype.isAlive = function(){
        if(this.stats.health < 1) return false;
        return true;
    };

    //can act by default, otherwise checks stats
    Character.prototype.canAct = function(){
        return this.stats.canAct || true;
    };
    
    //Register a status effect on the character
    Character.prototype.addStatusEffect = function(type,params,turns){
        if(StatusEffectFunctions[type] === undefined){
            throw new Error("Unrecognised status effect: " + type);
        }
        //Store the bound function, any parameters, and how long
        let currentStatusEffects = this.statusEffects.map(function(d){
            return d.effectType;
        });

        if(!_.includes(currentStatusEffects,type)){
            this.statusEffects.push({
                "effectType" : type,
                "effect" : StatusEffectFunctions[type].bind(this),
                "params" : params,
                "turnsLeft" : turns,
                "initial" : true
            });
            return true;
        }else{
            return false;
        }
    };

    //For all registered status effect, apply them, and
    //decrement the number of turns left for it
    Character.prototype.applyStatusEffects = function(){
        this.statusEffects = this.statusEffects.map(function(d){
            d.params = d.effect(d.params,d.turnsLeft,d.initial);
            d.initial = false;
            d.turnsLeft--;
            if(d.turnsLeft >= 0){
                return d;
            }
        },this).filter(function(d){ return d !== undefined; });
    };

    //Opponents list WILL INCLUDE SELF
    Character.prototype.act = function(opponents){
        //remove targets that dont make sense to act on
        let that = this,
            viableTargets = _.reject(opponents,function(d){
                if(d.name === that.name) return true;
                if(d.stats.health <= 0) return true;
                return false;
            }),
            finalTarget = _.sample(viableTargets),
            observers = _.reject(opponents,function(d){
                if(d.name === that.name) return true;
                if(d.stats.health <= 0) return true;
                return false;
            }),
        //select action
            action = _.sample(this.actions);

        
        
        if(finalTarget){
        //perform action
            action(this,finalTarget,observers);
        }
    };
    
    
    return Character;
});
