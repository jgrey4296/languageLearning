/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

/**
   Creates a Character who can hold a position in the organisation /army
 */
define(['lodash','./LanguageGen'],function(_,LGen){
    "use strict";
    const defaultAttributeNumber = 5;
    
    let Character = function(position){
        this.name = LGen.name();
        this.strengths = {};
        this.weaknesses = {};
        this.position = position;
        this.conflicts = [];
        this.location = null;

        this.generateCharacterTraits();
    };

    Character.prototype.generateCharacterTraits = function(){
        //gen strengths
        let strengths = Array(defaultAttributeNumber).fill(0).map(d=>LGen.genParse('attributes'));
        this.strengths = strengths.reduce(function(m,v){
            if(m[v] === undefined){
                m[v] = 0;
            }
            m[v]++;
            return m;
        },this.strengths);
        //gen weaknesses        
        let weaknesses = Array(defaultAttributeNumber).fill(0).map(d=>LGen.genParse('attributes'));
        this.weaknesses = weaknesses.reduce(function(m,v){
            if(m[v] === undefined){
                m[v] = 0;
            }
            m[v]++;
            return m;
        },this.weaknesses);

        //gen initial conflicts?
    };
    
    Character.prototype.calculateConflict = function(char){
        //tally up the strengths c1 has that are weaknesses of c2 and vice versa
        let c1Strength = this.tallyStrengths(char),
            c2Strength = char.tallyStrengths(this),
            result = {
                winner : null,
                loser : null                
            };
        if(c1Strength > c2Strength){
            result.winner = this;
            result.loser = char;
        }else if(c2Strength > c1Strength){
            result.winner = char;
            result.loser = this;
        }else{
            let pair = _.shuffle([this,char]);
            result.winner = pair[0];
            result.loser = pair[1];
        }
        return result;
    };


    Character.prototype.tallyStrengths = function(char){
        let thisRef = this;
        return _.keys(this.strengths).reduce(function(m,v){
            if(char.weaknesses[v] !== undefined){
                m += thisRef.strengths[v];
            }
            return m;
        },0);
    };

                                                
    return Character;
});
    
