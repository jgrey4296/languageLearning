/* jshint esversion : 6 */
"use strict";

let Loan = require('../src/Loan');

exports.LoanTests = {

    init : function(test){
        let aLoan = new Loan('bob','bill',5,0.1,2);
        test.ok(aLoan !== undefined);
        test.ok(aLoan !== null);
        test.ok(aLoan.principal === 5);
        test.ok(aLoan.rate = 0.1);
        test.ok(aLoan.length = 2);
        test.ok(aLoan.isCompound === false);
        test.ok(aLoan.currentValue === 5);
        test.ok(aLoan.remainingLength === 2);
        test.done();
    },

    simpleInterestTest_1year : function(test){
        let aLoan = new Loan('bob','bill',100,0.1,1);
        test.ok(aLoan.principal === 100);
        test.ok(aLoan.currentValue === 100);
        test.ok(aLoan.remainingLength === 1);
        aLoan.applyInterest();
        test.ok(aLoan.principal === 100,aLoan.principal);
        test.ok(aLoan.currentValue === 110,aLoan.currentValue);
        test.ok(aLoan.remainingLength === 0,aLoan.remainingLength);
        test.done();
    },

    simpleInterestTest_2year : function(test){
        let aLoan = new Loan('bob','bill',100,0.1,2);
        test.ok(aLoan.principal === 100);
        test.ok(aLoan.currentValue === 100);
        test.ok(aLoan.remainingLength === 2);
        aLoan.applyInterest();
        test.ok(aLoan.principal === 100,aLoan.principal);
        test.ok(aLoan.currentValue === 110,aLoan.currentValue);
        test.ok(aLoan.remainingLength === 1,aLoan.remainingLength);
        aLoan.applyInterest();
        test.ok(aLoan.principal === 100);
        test.ok(aLoan.currentValue === 120,aLoan.currentValue);
        test.ok(aLoan.remainingLength === 0);
        test.done();
    },

    compoundInterestTest_1year : function(test){
        let aLoan = new Loan('bob','bill',100,0.1,1,true);
        test.ok(aLoan.principal === 100);
        test.ok(aLoan.currentValue === 100);
        test.ok(aLoan.remainingLength === 1);
        aLoan.applyInterest();
        test.ok(aLoan.principal === 100,aLoan.principal);
        test.ok(aLoan.currentValue === 110,aLoan.currentValue);
        test.ok(aLoan.remainingLength === 0,aLoan.remainingLength);
        test.done();
    },

    compoundInterestTest_2year : function(test){
        let aLoan = new Loan('bob','bill',100,0.1,2,true);
        test.ok(aLoan.principal === 100);
        test.ok(aLoan.currentValue === 100);
        test.ok(aLoan.remainingLength === 2);
        aLoan.applyInterest();
        test.ok(aLoan.principal === 100,aLoan.principal);
        test.ok(aLoan.currentValue === 110,aLoan.currentValue);
        test.ok(aLoan.remainingLength === 1,aLoan.remainingLength);
        aLoan.applyInterest();
        test.ok(aLoan.principal === 100, aLoan.principal);
        test.ok(Math.floor(aLoan.currentValue) === 121);
        test.done();
    },        

    simple_calculateAtYear_test : function(test){
        let aLoan = new Loan('bob','bill',100,0.1,1);
        test.ok(Math.floor(aLoan.atYear(1)) === 110);
        test.ok(Math.floor(aLoan.atYear(2)) === 120);
        test.ok(Math.floor(aLoan.atYear(3)) === 130);        
        test.done();
    },

    compound_calculateAtYear_test : function(test){
        let aLoan = new Loan('bob','bill',100,0.1,1,true);
        test.ok(Math.floor(aLoan.atYear(1)) === 110,aLoan.atYear(1));
        let aLoan2 = new Loan('bob','bill',1000,0.05,3,true);
        test.ok(aLoan2.atYear(3) === 1157.63,aLoan2.atYear(3));
        test.done();
    },
};
