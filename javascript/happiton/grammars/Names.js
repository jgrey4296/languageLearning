if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define([],function(){
    return {
        "name" : "${firstName} ${middleName} ${surName}",
        
        "male_firstName" : "Bob Bill James Jim John".split(" "),
        "female_firstName" : "Jill Jane Jessica Laura Megan".split(" "),
        "middleName" : ["","Cane","Wane","Winthrop","Lesley"],
        "surName" : "Smith Jones Hammerfell Grotherby Barnaby Harrell Brunner".split(" ")
    };
});
