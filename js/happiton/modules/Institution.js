if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['underscore'],function(_){

    var Institution = function(name){
        this.name = name;
        this.members = [];
        this.roles = [];
        this.rules = [];
        this.norms = [];
        this.values = [];
        this.activities = [];
        this.timeAndSpace = {
            time : null,
            space : null
        };
    };


    return Institution;
});
