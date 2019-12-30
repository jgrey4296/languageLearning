/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['lodash','./LanguageGen'],function(_,LGen){
    "use strict";
    let nextIndex = 0,
        Tree = function(parent,size){
            this.id = ++nextIndex;
            this.parent = parent;
            this.name = LGen.rankName();
            this.assignedTo = null;
            this.children = Array(size).fill(null);
        };
    
    Tree.prototype.genTree = function(depth){
        depth = depth || 0;
        if(depth === 0){
            this.children = this.children.map(d=>new Tree(this,this.children.length));
        }else{
            this.children = this.children.map(d=>new Tree(this,this.children.length));
            this.children.forEach(d=>d.genTree(depth-1));
        }
    };

    Tree.prototype.promote = function(){
        if(this.parent !== undefined && this.parent.assignedTo === null){
            this.parent.assignedTo = this.assignedTo;
            this.fire();
        }        
    };

    Tree.prototype.demote = function(){
        let potentials = this.children.filter(d=>d.assignedTo === null),
            chosen = _.sample(potentials);
        if(chosen !== null && chosen !== undefined){
            chosen.assignedTo = this.assignedTo;
            this.fire();
        }
    };

    Tree.prototype.fire = function(){
        this.assignedTo = null;
    };

    Tree.prototype.hire = function(char){
        if(this.assignedTo !== null){
            throw new Error('hiring on a filled spot');
        }
        this.assignedTo = char;
        char.position = this;
    };
    
    Tree.prototype.dfs = function(){
        let found = new Set(),
            stack = [this];

        while(stack.length > 0){
            let current = stack.pop();
            if(!found.has(current)){
                found.add(current);
                if(current.children !== undefined){
                    stack = stack.concat(current.children.filter(d=>d!==null));
                }
            }
        }
        return Array.from(found);
    };

    Tree.prototype.dfsToObject = function(){
        let found = this.dfs();
        let obj = found.reduce(function(m,v){
            if(m[v.name] === undefined){
                m[v.name] = [];
            }
            m[v.name].push(v);
            return m;
        },{});
        return obj;
    };
    
    return Tree;
});
