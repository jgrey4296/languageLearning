if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define([],function(){
    //Load action constructors in here:
    var actionArray = [

    ];

    //A Utility constructor to bind the context for all of the actions
    //return each action with the context passed in
    var ActionsContextConstructor = function(context){
        return actionArray.map(d=>d(context));
    };

    return ActionsContextConstructor;
});
