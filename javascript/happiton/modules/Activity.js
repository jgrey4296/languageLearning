if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['underscore'],function(_){

    var Activity = function(){
        this.actor = null
        this.targets = [];
        this.fsm = null;        
    };

    return Activity;    
});
