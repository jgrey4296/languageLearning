if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['lodash','./Character'],function(_,Character){
    "use strict";
    //Battlesystem object. Keeps track of characters and turns
    let BattleSystem = function(maxTurns,characterData){
        this.maxTurns = maxTurns || 10;
        this.currentTurn = 0;

        this.characters = characterData.map(function(d){
            return new Character(d);
        });
        
        console.log("Loaded Characters: ",this.characters.map(function(d){ return d.name; }));
        
    };

    /**
       Run a single turn of the combat system
     */
    BattleSystem.prototype.fight = function(){
        console.log("\n\n\n---------------");
        console.log("Turn : ", this.currentTurn);
        console.log("---------------");
        //Current status:
        this.characters.forEach(function(d){
            console.log(d.printStats());
        });

        //Sort alive characters by speed:
        let aliveChars = _.filter(this.characters,function(d){
            return d.isAlive();
        });
        //status effects are applied to each character
        aliveChars.forEach(function(d){
            d.applyStatusEffects();
        });
        let speedSortedChars = aliveChars.sort(function(a,b){
            return b.stats.speed - a.stats.speed;
        }),
            charactersWhoCanAct = speedSortedChars.filter(function(d){
                return d.canAct();
            });
        
        console.log("\n");
        //each character acts:
        charactersWhoCanAct.forEach(function(d){
            d.act(speedSortedChars);
        });


        console.log("\n");
        aliveChars.forEach(function(d){
            if(!d.isAlive()){
                console.log(d.name + " died.");
            }else{
                console.log(d.name + " is still alive");
            }
        });
        
        //Turn over, increment
        this.currentTurn++;
    };

    BattleSystem.prototype.inCombat = function(){
        if(this.currentTurn > this.maxTurns){
            return false;
        }
        //return false if everyone is dead, or only one person is alive
        let currentStatus = this.characters.reduce(function(m,c){
            if(c.isAlive()){
                m.alive++;
            }else{
                m.dead++;
            }
            return m;
        },{"alive" : 0, "dead" : 0});

        if(currentStatus.alive <= 1) return false;
        return true;
    };

    BattleSystem.prototype.addExperience = function(){
        //for all alive characters, add to their experience the experience of dead characters
        let aliveChars = _.filter(this.characters,function(d){
            return d.isAlive();
        }),
            deadChars = _.filter(this.characters,function(d){
                return !d.isAlive();
            }),
            totalExperience = deadChars.reduce(function(m,v){
                let newExpAmnt = m + v.stats.exp;
                return newExpAmnt;
            },0);

        aliveChars.forEach(function(d){
            d.stats.exp += totalExperience;
        });
        //todo: check for level up
    };

    
    return BattleSystem;
});
