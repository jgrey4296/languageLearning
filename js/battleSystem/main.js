"use strict";
let BattleSystem = require('./BattleSystem'),
    characterData = require('./data/CharacterData.json'),
    bs = new BattleSystem(10, characterData);

while(bs.inCombat()){
    bs.fight();
}

bs.addExperience();
console.log("Fight Over");
