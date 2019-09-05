/* jshint esversion : 6 */
//the building class
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['underscore'],function(_){
    "use strict";
    let types = [
        "entrance","dormatory","apartment","kitchen","refectory","clinic","showers",
        "power","hydroponics", "waterProcessing","recreation",'corridor',
        'elevator','closet','empty','shop'
    ];

    var Room = function(name,typeNum,position){
        this.name = name;
        this.type = types[typeNum] || "empty";
        this.position = position;
        this.individuals = [];
        //eg: ['power','-','10']
        this.modification = [];
        this.constructionState = {
            state : "inactive", //inactive->constructing->built
            time : 0
        };
    };


    return Room;
});
