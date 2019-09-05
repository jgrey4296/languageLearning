"use strict";
var _ = require('lodash'),
    Character = require('./Character'),
    Structure = require('./Structure'),
    LGen = require('./LanguageGen');

exports.SHoMlike_tests = {
    
    charInit : function(test){
        let char = new Character();
        test.ok(char !== undefined);
        test.ok(char.name !== undefined);
        test.done();
    },

    structInit : function(test){
        let struct = new Structure();
        test.ok(struct !== undefined);
        test.ok(struct.name !== undefined);
        test.ok(struct.leader !== undefined);
        test.ok(struct.allPositions.length === 6);
        test.ok(_.keys(struct.locations).length === 10);
        test.done();
    },

    PositionInit : function(test){
        let pos = new Structure.prototype.Position();
        test.ok(pos !== undefined);
        test.ok(pos.name !== undefined);
        test.ok(pos.assignedTo instanceof Character);
        test.ok(pos.superior !== undefined);
        test.ok(pos.children.length === 0);
        test.done();
    },

    superiorPositionCheck : function(test){
        let pos1 = new Structure.prototype.Position(),
            pos2 = new Structure.prototype.Position(),
            pos3 = new Structure.prototype.Position();
        //connect them
        pos1.children.push(pos2);
        pos2.superior = pos1;
        pos2.children.push(pos3);
        pos3.superior = pos2;
        //test
        test.ok(pos1.superiorOf(pos2) === true);
        test.ok(pos1.superiorOf(pos3) === true);
        test.done();
    },

    structure_generate_test : function(test){
        let struct = new Structure(),
            allPositions = new Set(struct.allPositions),
            dfsFoundPositions = struct.leader.dfs();
        test.ok(dfsFoundPositions.length === 6);
        test.ok(dfsFoundPositions.length === allPositions.size);
        while(dfsFoundPositions.length > 0){
            let current = dfsFoundPositions.pop();
            if(current !== struct.leader){                                          
                test.ok(current.superior !== null);
            }
            test.ok(allPositions.has(current));
        }
        test.done();
    },

    
    promote_test : function(test){
        let struct = new Structure(),
            leader = struct.leader,
            child = _.sample(leader.children),
            person = child.assignedTo;
        test.ok(child !== undefined);
        //attempt when leader is filled:
        test.ok(leader.assignedTo !== null);
        test.ok(child.assignedTo !== null);
        test.ok(struct.promote(person) === false);//promotion fails
        //attempt when leader is not filled:
        leader.assignedTo = null;
        test.ok(person.position !== null);
        test.ok(person.position.superior !== null);
        test.ok(person.position.superior.assignedTo === null);
        test.ok(struct.promote(person) === true);//promotion succeeds
        //check it worked:
        test.ok(child.assignedTo === null);
        test.ok(leader.assignedTo !== null);
        test.ok(leader.assignedTo === person);        
        test.done();
    },

    demote_test : function(test){
        let struct = new Structure(),
            leader = struct.leader,
            child = _.sample(leader.children),
            person = leader.assignedTo;
        test.ok(person.position === leader);
        test.ok(person.position.children.indexOf(child) !== -1);
        //bad demotion as there isnt an empty place:
        test.ok(struct.demote(person) === false);
        //good demotion as there is an empty place:
        child.assignedTo = null;
        test.ok(struct.demote(person) === true);
        //check it worked:
        test.ok(leader.assignedTo === null);
        test.ok(child.assignedTo === person);
        test.ok(person.position === child);
        
        test.done();
    },

    remove_test : function(test){
        let struct = new Structure(),
            leader = struct.leader,
            person = leader.assignedTo;
        test.ok(struct !== null);
        test.ok(leader !== null);
        test.ok(person !== null);
        test.ok(struct.remove(person) === true);
        test.ok(leader.assignedTo === null);
        test.ok(person.position === null);        
        test.done();
    },

    add_test : function(test){
        let struct = new Structure(),
            leader = struct.leader,
            person = leader.assignedTo;
        struct.remove(person);
        test.ok(leader.assignedTo === null);
        test.ok(person.position === null);
        struct.add(person,leader);
        test.ok(leader.assignedTo === person);
        test.ok(person.position === leader);        
        test.done();
    },
    
};
