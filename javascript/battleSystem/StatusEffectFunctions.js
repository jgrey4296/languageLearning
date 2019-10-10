if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['lodash'],function(_){
    "use strict";
    //Object of all status effect functions
    //WRITTEN WITH THE ASSUMPTION THEY WILL BE BOUND TO A CHARACTER
    //AND PARAMETERS ARE PASSED IN
    let sefs = {};

    //Deal health damage each turn
    sefs.poison = function(params,turnsLeft){
        this.stats.health -= params[0];
        console.log(this.name + " was hurt by poison for " + params[0] + " health");
        
        if(turnsLeft === 0){
            console.log("Poison on " + this.name + " ended.");
        }        
        return params;
    };

    //reduce the speed of the character on the first turn,
    //reset on the last turn
    sefs.slow = function(params,turnsLeft,initial){
        if(initial){
            params.push(this.stats.speed);
            this.stats.speed -= params[0];
        }
        if(turnsLeft === 0){
            this.stats.speed = params[params.length-1];
            console.log("Slow on " + this.name + " ran out");
        }
        return params;   
    };

    sefs.paralyze = function(params,turnsLeft,initial){
        if(initial){
            this.stats.canAct = false;
        }
        if(turnsLeft === 0){
            delete this.stats.canAct;
        }
        return params;
    };
    
    return sefs;
});
