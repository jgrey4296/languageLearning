/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}
/**
   tracery style parsing/text generation
   Assumes objects are of the form ${name} to denote rules
   ie: { start: "${greeting}", greeting: "hello" };
   *   Uses 1-10 registered ordered parsing, so "$2{something} $1{else}"
   will have 'else' expanded first
   *   Can store temporary subgrammars using ${#push} and ${#pop},
   assignment with ${#=key:val}
   @module Parse
*/
define(['lodash','./nlp_compromise.min'],function(_,nlp){
    "use strict";

    var ModFunctions = {
        cap : function(string){
            return string.charAt(0).toUpperCase() + string.slice(1);
        },

    };

    
    /**
       Take a grammar, create a trace expansion
       @function
       @alias module:Parse
       @param grammarObj
       @param start
       @param depth
     */
    var ParseObject = function(grammarStack,start,depth){
        //console.log('Start Val:',start);
        depth = depth || 1;
        if(depth > 25){
            //console.warn("Parse depth > 50");
            return start;
        }

        if(!(grammarStack instanceof Array)){
            grammarStack = [grammarStack];
        }
        
        //perform instructions:
        // #push, #pop, #=a:b ?a
        //console.log(start);
        if(typeof start === 'string'){
            if(start.match(/^#push$/)){ //Push a sub grammar
                grammarStack.push({});
                return "";
            }else if(start.match(/^#pop$/)){ //pop a subgrammar
                grammarStack.pop();
                return "";
            }else if(start.match(/^#=/)){ //Assign to the current grammar
                let match = /^#=([\w\s]+):(!)?([\w\s]+)$/.exec(start);
                if(match !== null){
                    if(match[2] !== null){
                        _.last(grammarStack)[pair[1]] = pair[3];
                    }else{
                        _.last(grammarStack)[pair[1]] = ParseObject(grammarStack,pair[3],depth+1);
                    }
                    return "";
                }
            }else if(start.match(/^\?/)){ //rule noted as optional
                if(Math.random() > 0.5){
                        return ParseObject(grammarStack,start.slice(1),depth+1);
                }
                return ""; //option isnt expanded
            }else if(start.match(/^#(\d)-(\d):(\w+)/)){
                let match = /^#(\d+)-(\d+)?:(\w+)/.exec(start),
                    val1 = Number(match[1]) || 0,
                    val2 = Number(match[2]) || (val1 + 1),
                    str = match[3],
                    result = Array(Math.floor((Math.random() * (val2 - val1)) + val1)).fill(0).map(d=>ParseObject(grammarStack,str,depth+1)).join("");
                return result.trim();
            }else if(start.match(/(\w+)\.(\w+)\(\)/)){
                let match = /(\w+)\.(\w+)\(\)/.exec(start);
                if(match !== null && ModFunctions[match[2]] !== undefined){
                    return ModFunctions[match[2]](ParseObject(grammarStack,match[1],depth+1)).trim();
                }else{
                    return match[1];
                }
            }else if(start.match(/--(\w+)/)){
                let match = /--(\w+)/.exec(start);
                grammarStack.push({});
                //choose a component to expand
                let expansionRuleCopy = _.shuffle(_.copy(getExpansionRule(grammarStack,start)));

                //remove it from the top stack copy of the target
                //return the expanded selected component
                return ParseObject(grammarStack,match[1]);
            }
        }
        
        //Get the rule's string
        let currentString = getExpansionRule(grammarStack,start);
        //get one of the options
        if(currentString instanceof Array){
            currentString = _.sample(currentString);
        }

        for(let n = 0; n < 10; n++){
            //ordered EXPANSION
            let VarRegex = new RegExp(`\\$${n < 1 ? "" : n}{(\\??#?=?[\\w:\\.\\(\\)\\-]+)}`),
                variable = VarRegex.exec(currentString);
            //console.log("Vrg:",VarRegex);
            while(variable !== null){
                //console.log("Matched:",variable);
                let result = ParseObject(grammarStack,variable[1],depth+1);
                //console.log("result:",result);
                //Deal with actual variables:
                currentString = strSplice(currentString,variable.index,variable.index+variable[0].length,result);
                variable = VarRegex.exec(currentString);
            }
            
        }
        return currentString.trim();
    };

    var getExpansionRule = function(grammarStack,name){
        let currentString = name,
            i = grammarStack.length,
            found = false;
        while(!found && --i >= 0){
            if(grammarStack[i][name] !== undefined){
                currentString = grammarStack[i][name];
                found = true;
            }
        }
        return currentString;
    };
    
    var strSplice = function(str,start,end,replace){
        return str.slice(0,start) + (replace || "") + str.slice(end);
    };

    
    return ParseObject;
});
