if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['lodash'],function(_){
    "use strict";
    let nextId = 0;
    
    let Agent = function(cell){
        this.id = nextId++;
        this.colour = 'green';
        this.socialColour = "pink";
        //place the agent it a cell:
        this.cell = cell;
        cell.populated = this;

        //Mutable variables of the agent
        this.statuses = {
            alive : true,
            metabolism : Math.floor(1+ Math.random() * 15),
            heldSugar : 0,
            maxHolding : 100,
            report : false,
        };
        this.statuses.heldSugar = this.statuses.metabolism;

        //Non-mutable variables
        this.traits = {
            vision : 5
        };
    };

    //Think and act:
    Agent.prototype.simulate = function(neighbours){
        if(this.statuses.report === true){
            console.log('simulating');
        }
        
        //metabolism:
        if(this.statuses.heldSugar < this.statuses.metabolism){
            this.statuses.alive = false;
            this.cell.populated = null;
            this.cell = null;
            return false;
        };
        //collect:
        this.statuses.heldSugar += this.cell.decrementSugar();
        //eat:
        this.statuses.heldSugar -= this.statuses.metabolism;
        if(this.statuses.heldSugar > this.statuses.maxHolding) {
            this.statuses.heldSugar = this.statuses.maxHolding;
        }
        
        //Move:        
        let maxSugar = Math.max.apply(undefined,_.values(neighbours).map(d=>d.sugarLevel));
        if(maxSugar > this.cell.sugarLevel){
            let maxCell = _.sample(_.values(neighbours).filter(d=>d.sugarLevel === maxSugar));
            //move to the max
            this.move(maxCell);
        }else{
            this.move(_.sample(_.values(neighbours).filter(d=>d.populated === null)));
        }
        
        return true;
    };

    //Move to the given cell
    Agent.prototype.move = function(cell){
        if((!cell) || (cell.populated !== null)){ return; }
        this.cell.populated = null;
        this.cell = cell;
        this.cell.populated = this;
    };

    //remove from the cell existing in
    Agent.prototype.die = function(){
        this.cell.populated = null;
    };
    
    return Agent;
});
