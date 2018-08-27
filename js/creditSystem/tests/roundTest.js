/* jshint esversion : 6 */
"use strict";

let Round = require('../src/round');

exports.roundTests = {

    simpleTest : function(test){
        test.ok(Round.toDecimal(1.4,1) === 1.4,Round.toDecimal(1.4,1));
        test.ok(Round.toDecimal(1.414,2) === 1.41,Round.toDecimal(1.414,2));
        test.ok(Round.toDecimal(1.414,3) === 1.414);
        test.ok(Round.toDecimal(1.454,1) === 1.5,Round.toDecimal(1.454,1));
        test.done();
    },

};
