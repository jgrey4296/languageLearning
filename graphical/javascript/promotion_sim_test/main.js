/*jshint esversion : 6 */
/**
   Shadow of Mordor-like Organization Simulator
*/
"use strict";
let repl = require('repl'),
    _ = require('lodash'),
    Character = require('./Character'),
    Institution = require('./Institutions');

let characters = Array(5).fill(0).map(d=>new Character()).sort((a,b)=>{
    if(a.name < b.name){
        return -1;
    }else if(a.name > b.name){
        return 1;
    }
    return 0;
}),
    org = new Institution();


org.assignCharacters(characters,-1);

console.log('---------- Characters');
characters.forEach(function(d){
    console.log(d.name);
});
console.log('\n\n\n-------------------- Institution');
console.log(org);

let i = 10;
while(--i > 0){
    //get a random position
    let selected = _.sample(_.sample(_.values(org.ranks))),
        doAction = Math.floor(Math.random() * 3);
    //either:
    ////fire
    if(doAction === 0){
        if(selected.assignedTo !== null){
        console.log(`Firing ${selected.assignedTo.name} from ${selected.name}`);
            selected.fire();
        }
    }
    ////promote
    if(doAction === 1){
        if(selected.parent !== null && selected.assignedTo !== null){
            console.log(`Promoting ${selected.assignedTo.name} from ${selected.name} to ${selected.parent.name}`);
            selected.promote();
        }
    }
    ////demote
    if(doAction === 2){
        if(selected.assignedTo !== null){
        console.log(`Demoting ${selected.assignedTo.name} from ${selected.name}`);
            selected.demote();
        }
    }

    
    //Fire Symbolic Rules
    //Calculate utility rules
    //Select Action
    //Update Facts    

}

repl.start('> ').context.v = [org,characters];
