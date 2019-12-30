/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}


define(['lodash','./LanguageGen'],function(_,LanguageGen){
    "use strict";
    let Character = function(){
        //Name : []
        this.name = LanguageGen.charName();
        this.position = null;
        //Location
        this.location = null;
        //Personal Modifier Rules, ie: Utility
        this.personalModiferRules = [];
    };
    Character.prototype = Object.create({});
    Character.constructor = Character;

    Character.prototype.addPersonalModifierRule = function(rule){
        //add to the personal rules object
    };


    return Character;
});
