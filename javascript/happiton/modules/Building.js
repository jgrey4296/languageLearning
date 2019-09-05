//the building class
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['underscore'],function(_){
    var Building = function(name){
        this.name = name;
        this.individuals = [];
        //eg: ['power','-','10']
        this.modification = [];
    };

    return Building;
});
