"use strict";
var _ = require('lodash'),
    Vault = require('./modules/Vault');

//create the vault
let vaultInstance = new Vault(10,10),
    numOfDaysToSim = 2;
//turn on the simulation
vaultInstance.simulationState = "active";
//run the simulation untill it ends OR the specified number of days have passed
while(vaultInstance.simulationState === 'active' && vaultInstance.simulatedDays < numOfDaysToSim){
    vaultInstance.stepSim();
}
