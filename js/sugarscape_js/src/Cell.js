/* jshint esversion : 6 */
define(['lodash','perlin'],function(_,Noise){
    "use strict";
    const MaxSugarLevel = 100,
          MinSugarLevel = 0,
          incrementRange = 15,
          incrementCountRange = 10;
    
    let Cell = function(index){
        this.index = index;
        this.capacity = MaxSugarLevel;
        this.sugarLevel = 0;
        this.populated = null;
        this.edge = "black";
        this.internalCount = 0;
        this.incrementCount = Math.floor(1 + Math.random() * incrementCountRange);
        this.sugarIncrement = Math.floor(1 + Math.random() * incrementRange);
    };
    
    //increment to capacity
    Cell.prototype.incrementSugar = function(val){
        val = val || 1;
        if(this.sugarLevel < this.capacity && this.internalCount++ >= this.incrementCount){
            this.sugarLevel += (this.sugarIncrement * val);
            this.internalCount = 0;
        }
    };

    //reset sugar level, returning the amount
    Cell.prototype.decrementSugar = function(){
        let retValue = this.sugarLevel;
        this.sugarLevel = 0;
        return retValue;
    };
    
    //simulate the cell
    Cell.prototype.simulate = function(neighbours){
        this.incrementSugar();
    };

    //used to initialise the landscape. sets the sugar level and capacity
    Cell.prototype.absorbSugar = function(neighbours,decrementRate){
        let maxSugar = Math.max.apply(undefined,_.values(neighbours).map(d=>d.sugarLevel));
        if(this.sugarLevel < maxSugar - decrementRate){
            this.sugarLevel = maxSugar > decrementRate ? maxSugar - decrementRate : 0;
        }
        this.capacity = this.sugarLevel;
    };

    return Cell;
});
