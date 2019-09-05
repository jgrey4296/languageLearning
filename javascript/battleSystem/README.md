# Battlesystem

A simple, extensible pokemon like turn based combat system.

## Dependencies
[lodash](lodash.com)

## Basic Usage:
To run: `node main.js`.  

To use yourself, use `amdefine`:
```
    var BattleSystem = require('./BattleSystem'),
        characterData = require('./Data/CharacterData.json'),
        battleInstance = new BattleSystem(10,characterData); //number of turns to run for
```
Can also be used as an AMD module:
```
    require(['BattleSystem','json!./Data/CharacterData'],function(BattleSystem,data){
        ...
    });
```

## Fighting
Calling `battleInstance.inCombat()` returns a boolean as to whether the fight needs to continue.  
Calling `battleInstance.fight()` will cause each actor to perform an action, in speed order.


## Actions
The actions the system can perform are defined in `ActionFunctions.js`. Each function has the following signature:
`function(actor, target, observers, params) { }`. 

## Status Effects
Status functions can be applied by an action to a character, and will apply themselves every turn, with the following signature, and are -bound- to the actor affected.
`function(params,turnsLeft,initial) {}`.


## Characters
Characters load a data specification, and have `stats` (an object of arbitrary keys and values), `statusEffects` (registered with the `addStatusEffect` method, which looks up the status effect by name), and `actions` (an array of action names). 

## Data specification
The `characterData.json` file provides an array of objects, each providing the name, stats, and actions of a character.
