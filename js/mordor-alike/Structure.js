/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

/**
   Creates an Organisation / Army structure
 */
define(['lodash','./Character','./LanguageGen'],function(_,Character,LGen){
    "use strict";

    //An individual position in the structure
    let Position = function(){
        this.name = LGen.position();
        this.assignedTo = new Character(this);
        this.superior = null;
        this.children = [];        
    };

    //DFS from a position on down
    Position.prototype.dfs = function(){
        let found = new Set(),
            stack = [this];

        while(stack.length > 0){
            let current = stack.pop();
            if(!found.has(current)){
                found.add(current);
                stack = stack.concat(current.children);
            }
        }
        return Array.from(found);
    };

    Position.prototype.superiorOf = function(charOrPosition){
        let bottom = charOrPosition,
            isSuperior = false;
        if(charOrPosition instanceof Character){
            bottom = charOrPosition.position;
        }
        //at this point, bottom is a position
        while(bottom !== null){
            bottom = bottom.superior;
            if(bottom === this){
                isSuperior = true;
                break;
            }
        }
        return isSuperior;
    };
    //----------------------------------------
    //The primary structure
    let Structure = function(size,randFunc){
        //controllable random function:
        this.rand = ((a,b)=>Math.floor((Math.random() * (b-a)) + a));
        //name of the organisation
        this.name = LGen.orgName();
        //the root leader of the org
        this.leader = new Position();
        //every position
        this.allPositions = [this.leader];
        //all the individuals of the org
        this.allIndividuals = [];
        //all locations relevant to the org
        this.locations = Array(10).fill(0).map(()=>LGen.location()).reduce(function(m,v){
            m[v] = new Set();
            return m;
        },{});
        //----------
        //Populate the org:
        this.generate(size);
    };
    //Reference to Position Ctor:
    Structure.prototype.Position = Position;
    
    //Generate an org structure, from the leader on down
    Structure.prototype.generate = function(size){
        size = size || 5;
        let positions = Array(size).fill(0).map(d=>new Position()),
            frontier = [this.leader];
        this.allPositions = this.allPositions.concat(positions);
        this.allIndividuals = this.allPositions.map(d=>d.assignedTo);

        //create the hierarchy
        while(positions.length > 0 && frontier.length > 0){
            let current = frontier.shift(),
                selection = positions.splice(0,this.rand(1,4));
            selection.forEach(d=>d.superior = current);
            current.children = current.children.concat(selection);
            frontier = frontier.concat(selection);            
        };
    };

    //--------------------
    //simulate the cycle of conflicts
    Structure.prototype.stepTime = function(){
        //move characters to (random) locations
        this.allIndividuals.forEach(d=>{
            if(d.location !== null){
                this.locations[d.location].delete(d);
                d.location = null;
            }
            d.location = _.sample(_.keys(this.locations));
            this.locations[d.location].add(d);
        });
        
        //choose a conflict between two positions
        let conflictPairs = _.values(this.locations).map(d=>{
            let setArray = Array.from(d),
                pair = _.sampleSize(setArray,2);
            return pair.length === 2 ? pair : [];
        }).filter(d=>d.length === 2);
        
        //enact the conflict
        conflictPairs.forEach(d=>console.log(`Conflict between: ${d[0].name} and ${d[1].name}`));
        let conflictResults = conflictPairs.map(d=>d[0].calculateConflict(d[1])).filter(d=>d.winner !== null && d.loser !== null);
        //demote/remove the loser
        //todo: chance based demote/kill split
        //todo: if a winner and no chance of promotion: increase liklihood of conflict,
        //or cross edge movement
        conflictResults.forEach(d=>{
            console.log(`${d.winner.name} won, ${d.loser.name} lost`);
            this.promote(d.winner);
            if(!this.demote(d.loser)){ this.remove(d.loser); }
        });
    };
    //--------------------
    
    Structure.prototype.promote = function(char){
        //console.log('promoting:',char);
        //if the position above the char is vacant, move to that position
        if(char.position !== null && char.position.superior !== null && char.position.superior.assignedTo === null){
            let superior = char.position.superior;
            char.position.assignedTo = null
            char.position = superior;
            char.position.assignedTo = char;
            console.log(`${char.name} was promoted to ${char.position.name}`);
            return true;
        }
        console.log(`${char.name} was unable to be promoted`);
        return false;
    };

    Structure.prototype.demote = function(char){
        //if there is a position below the character vacant, move to that position
        if(char.position !== null && char.position.children.length > 0){
            let potentials = char.position.children.filter(d=>d.assignedTo === null);
            if(potentials.length > 0){
                let selected = _.sample(potentials);                    
                char.position.assignedTo = null;
                char.position = selected;
                char.position.assignedTo = char;
                console.log(`${char.name} was demoted to ${char.position.name}`);
                return true;
            }
        }
        console.log(`${char.name} was unable to be demoted`);
        return false;
    };

    Structure.prototype.remove = function(char){
        //remove the char from the position they are in
        if(char.position !== null && char.position.assignedTo === char){
            console.log(`${char.name} was fired from ${char.position.name}`);
            char.position.assignedTo = null;
            char.position = null;
            return true;
        }
        return false;
    };

    Structure.prototype.add = function(char,pos){
        //if the position is vacant, assign to the char
        if(char.position === null && pos.assignedTo === null){
            char.position = pos;
            pos.assignedTo = char;
            return true;
        }
        return false;        
    };
    
    //todo: describe the distance between two characters
    //both vertically and horizontally?
    Structure.prototype.distance = function(char,char2){
        return 1;
    };
    

    return Structure;
});
    
