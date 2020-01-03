/* jshint esversion : 6 */
/**
   Run a simple simulation of the system.
*/
"use strict";
//imports
let Structure = require('./Structure'),
    _ = require('lodash'),
    //Variables:
    turns = 10,
    currentTurn = 0,
    army = new Structure(); 

console.log('---------- Locations ----------');
_.keys(army.locations).forEach(d=>console.log(d));
console.log('--------------------');

while(++currentTurn <= turns){
    console.log(`---------- Turn : ${currentTurn} / ${turns}`);
    army.stepTime();    
}
