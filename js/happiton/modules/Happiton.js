if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['underscore','../libs/Parse','../grammars/Buildings','./Individual'],function(_,Parse,BuildingGrammar,Individual){
    "use strict";
    
    var Happiton = function(popSize,buildingNumber){
        //pending/active/inactive
        this.simulationState = 'pending';
        this.turnNumber = 0;
        /** @type {Object.<String,Individual>} The pop of the town */
        this.population = Array(popSize).fill(0).map(d=>new Individual(this));
        /** @type {Object.<String,Array.<Building>>} The Non-residential buildings of the town  */
        this.buildings = Array(buildingNumber).fill(0).map(d=>Parse([BuildingGrammar],'building'));

        this.institutions = [];
                
        //registered abstraction calculations
        this.registeredAbstractions = [];

        //the time:
        this.time = {
            month : 0,
            week : 0,
            day : 0,
            hour : 0,
            quarter : 0
        };

        this.reteNet = null;
    };

    Happiton.prototype.start = function(){
        this.simulationState = 'active';
    }

    Happiton.prototype.incrementTime = function(){
        this.quarter++;
        if(4 <= this.quarter){
            this.quarter = 0;
            this.hour++;
        }
        if(24 <= this.hour){
            this.hour = 0;
            this.day++;
        }
        if(7 <= this.day){
            this.day = 0;
            this.week++;
        }
        if(4 <= this.week){
            this.week = 0;
            this.month++;
        }        
    };

    Happiton.prototype.stepTime = function(){
        if(this.simulationState !== 'active'){
            return;
        }
        this.incrementTime();

        //for population: enact routine
        this.population.forEach(d=>d.stepTime(this.time));

        this.turnNumber++;
    };
    

    return Happiton;
    
});
