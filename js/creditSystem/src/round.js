/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define([],function(){
    "use strict";
    let Round = {};

    Round.toDecimal = function(num,n){
        n = n || 2;
        if(num === 0) {
            return 0;
        }
        let magnitude = Math.pow(10, n),
            shifted = Math.round(num*magnitude);
        return shifted/magnitude;
    };

    return Round;
});
