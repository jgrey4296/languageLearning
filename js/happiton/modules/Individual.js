/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['underscore','../grammars/Names','../libs/Parse'],function(_,NameGrammar,Parse){
    "use strict";
    
    var id = 0;
    //The individual class
    var Individual = function(simRef,startLocation){
        this.simRef = simRef;
        this.id = id++;
        this.sex = Math.random() < 0.5 ? "male" : "female";
        this.age = Math.floor(Math.random() * 70);
        this.firstName = Parse([NameGrammar],`${this.sex}_firstName`);
        this.middleName = Parse([NameGrammar],'middleName');
        this.lastName = Parse([NameGrammar],'surName');
        this.initials = `${this.firstName[0]}.${this.middleName[0]}.${this.lastName[0]}`;
        this.maritalStatus = null;
        this.honourific = this.sex === "male" ? "Mr" : this.maritalStatus === null ? "Miss" : "Mrs";
        this.alive = true;
        this.location = startLocation;
        
        //todo:
        this.relations = {
            "blood" : [],
            "marriage" : [],
        };
        this.birthday = Math.floor(Math.random() * 20);
        this.norms = [];
        
        this.currentActivity = null;
    };

    Individual.prototype.perform = function(time){
        //Get proposed actions for individual
        //this.simRef.rete.proposedActions....

        //filter by action methods individual has defined?
        
        //schedule/perform?  one of the actions

        //Print out the individual's action
        console.log(`\t${this.firstName} ${this.lastName}`);
    };

    //Actions an individual can perform:
    //movement
    Individual.prototype.move = function(location){

    };

    //operating machinery

    //fixing machinery

    //cleaning

    //cooking

    //working

    //playing

    return Individual;
});
