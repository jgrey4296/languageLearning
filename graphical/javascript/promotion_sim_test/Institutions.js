/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['lodash','./LanguageGen','./TreeGen'],function(_,LanguageGen,Tree){
    "use strict";
    let randomNum = ()=>Math.floor(1+ Math.random() * 4);

    let Institution = function(){
        //Create a name for the Institution
        this.name = LanguageGen.orgName();
        //Structure:
        //Generate Vertical Hierarchy Levels, ie: Ranks.
        this.structureRoot = new Tree(null,randomNum());
        this.structureRoot.genTree(randomNum());
        this.ranks = this.structureRoot.dfsToObject();
        this.rankNames = _.keys(this.ranks);
        //Semantics of the Institution
        //Possibly a reteNet
        this.rules = null;
        //Atomic Actions of the Institution
        this.atomics = Array(randomNum()).fill(0).map(d=>LanguageGen.atomicAct());
        //Channels of Expression
        this.channels = Array(randomNum()).fill(0).map(d=>LanguageGen.communicationsChannel());
        //Locations
        this.locations = Array(randomNum()).fill(0).map(d=>LanguageGen.locationName());
    };

    Institution.prototype = Object.create({});
    Institution.constructor = Institution;

    Institution.prototype.assignCharacters = function(characterArray,direction){
        let ranks = _.shuffle(this.structureRoot.dfs()),
            charClone = _.shuffle(_.clone(characterArray));
        while(charClone.length > 0){
            let currentChar = charClone.pop(),
                currentPosition = ranks.pop();
            if(currentChar.position === null && currentPosition !== undefined){
                currentPosition.hire(currentChar);
            }
        };
    };


    return Institution;
});
