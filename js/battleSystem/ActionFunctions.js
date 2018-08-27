if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['lodash','./StatusEffectFunctions'],function(_,StatusEffectFunctions){
    "use strict";
    //Action Function object:
    //Assumes actor and target and observers are passed in as params:
    let Acts = {};

    //A Basic attack. Does damage.
    //from http://bulbapedia.bulbagarden.net/wiki/Damage#Damage_formula
    //pokemon formula: dam = ((2 * lvl + 10)/250 * (attack / defense) * base + 2) * modifier
    //where modifier = sameTypeBonus * type * critical * other * random
    Acts.simpleAttack = function(actor,target,observers,params){
        let baseDamage = 10,
            attackAmt = baseDamage + Math.abs(actor.stats.attack - target.stats.defense);
        target.stats.health -= attackAmt;
        console.log(actor.name + " attacked " + target.name + " for " + attackAmt);
    };

    //Poisons the target for 5 health each turn for a number of turns
    Acts.poisonAttack = function(actor,target,observers,params){
        let length = actor.stats.attack,
            result = target.addStatusEffect("poison",[5],length);
        if(result){
            console.log(actor.name + " poisoned " + target.name + " for " + length + " turns");
        }else{
            console.log(actor.name + " tried to poison " + target.name);
            console.log("But it failed");
        };
        
    };

    //Slows the target by 3 once for a number of turns
    Acts.slowAttack = function(actor,target,observers,params){
        target.addStatusEffect("slow",[3],actor.stats.attack);
        console.log(actor.name + " slowed " + target.name + " for " + actor.stats.attack + " turns");        
    };
    
    return Acts;
});
