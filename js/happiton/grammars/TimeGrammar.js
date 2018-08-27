if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define([],function(){
//A simple grammar for days going by
    return {
        'quarterHour' : "Tick Tock".split(" "),
        'timePasses' : [
            "$2{pre} $1{timeStep} goes by",
            "$2{pre} $1{timeStep} passes",
        ],
        "timeStep" : [
            "${#=pre:a}day",
            "${#=pre:an}hour"
        ],
        
    };
});
