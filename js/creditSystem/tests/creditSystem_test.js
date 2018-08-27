/* jshint esversion : 6 */
"use strict";
let _ = require('../libs/lodash'),
    CreditSystem = require('../src/CreditSystem');

exports.CreditSystemTests = {

    init : function(test){
        let cs = new CreditSystem();
        test.ok(cs !== undefined);
        test.done();
    },

    register_character_test : function(test){
        let cs = new CreditSystem();
        test.ok(_.keys(cs.characters).length === 0);
        let bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill');
        test.ok(_.keys(cs.characters).length === 2);
        test.ok(cs.characters['bob'] === bobId);
        test.ok(cs.characters['bill'] === billId);
        test.done();
    },

    add_debt_test : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill');

        test.ok(_.keys(cs.debts[bobId]).length === 0);
        test.ok(_.keys(cs.credits[billId]).length === 0);
        cs.addDebt(bobId,billId,5);
        test.ok(cs.debts[bobId][billId] === 5);
        test.ok(cs.credits[billId][bobId] === 5);
        test.done();
    },

    payDebt_test : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill');
        cs.addDebt(bobId,billId,5);
        test.ok(cs.debts[bobId][billId] === 5);
        test.ok(cs.credits[billId][bobId] === 5);
        cs.payDebt(bobId,billId,2);
        test.ok(cs.debts[bobId][billId] === 3);
        test.ok(cs.credits[billId][bobId] === 3);
        cs.payDebt(bobId,billId,10);
        test.ok(cs.debts[bobId][billId] === undefined);
        test.ok(cs.credits[billId][bobId] === undefined);
        test.done();
    },

    hasDebt_test : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill'),
            jillId = cs.registerCharacter('jill');
        //add debts
        cs.addDebt(bobId,billId,10);
        cs.addDebt(billId,jillId,15);
        //check the debts exist:
        test.ok(cs.hasDebtTo(bobId,billId));
        test.ok(cs.hasDebtTo(billId,jillId));
        test.ok(cs.hasDebtTo(bobId,jillId) === false);
        test.ok(cs.hasDebtTo(billId,bobId) === false);
        test.done();
    },

    debtAmount_test : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill'),
            jillId = cs.registerCharacter('jill');
        //add debts
        cs.addDebt(bobId,billId,10);
        cs.addDebt(billId,jillId,15);
        //check:
        test.ok(cs.debtAmountTo(bobId,billId) === 10);
        test.ok(cs.debtAmountTo(billId,jillId) === 15);
        //add a reciprocal debt:
        cs.addDebt(billId,bobId,5);
        cs.addDebt(jillId,billId,3);
        //check again:
        test.ok(cs.debtAmountTo(bobId,billId) === (10 - 5));
        test.ok(cs.debtAmountTo(billId,jillId) === (15 - 3));
        //and the negative debts the other direction:
        test.ok(cs.debtAmountTo(billId,bobId) === (5 - 10));
        test.ok(cs.debtAmountTo(jillId,billId) === (3 - 15));
        test.done();
    },

    netWorth_test : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill'),
            jillId = cs.registerCharacter('jill');
        //add debt
        cs.addDebt(bobId,billId,10);
        cs.addDebt(bobId,jillId,20);
        //and reciprocal debt:
        cs.addDebt(billId,bobId,3);
        //check networth:
        test.ok(cs.netWorth(bobId) === (3 - 30));
        test.done();
    },
        
    transferDebt_test : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill'),
            jillId = cs.registerCharacter('jill');
        //add debt
        cs.addDebt(bobId,billId,5);
        cs.addDebt(billId,jillId,10);
        //check:
        test.ok(cs.debtAmountTo(bobId,billId) === 5);
        test.ok(cs.debtAmountTo(billId,jillId) === 10);
        //Transfer all of bobs debt to bill, to jill:
        cs.transferDebt(billId,bobId,jillId);
        //check:
        test.ok(cs.debtAmountTo(bobId,billId) === 0);
        test.ok(cs.debtAmountTo(bobId,jillId) === 5,cs.debtAmountTo(bobId,jillId));
        test.ok(cs.debtAmountTo(billId,jillId) === 5);        
        test.done();
    },

    transferSomeDebt_test : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill'),
            jillId = cs.registerCharacter('jill');
        //add debt
        cs.addDebt(bobId,billId,10);
        cs.addDebt(billId,jillId,10);
        //check:
        test.ok(cs.debtAmountTo(bobId,billId) === 10);
        test.ok(cs.debtAmountTo(billId,jillId) === 10);
        //Transfer 5 of bobs debt to bill, to jill:
        cs.transferDebt(billId,bobId,jillId,5);
        //check:
        test.ok(cs.debtAmountTo(bobId,billId) === 5);
        test.ok(cs.debtAmountTo(billId,jillId) === 5);
        test.ok(cs.debtAmountTo(bobId,jillId) === 5);
        test.done();
    },

    transferTooMuchDebt_test : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill'),
            jillId = cs.registerCharacter('jill');
        //add debt
        cs.addDebt(bobId,billId,10);
        cs.addDebt(billId,jillId,10);
        //check:
        test.ok(cs.debtAmountTo(bobId,billId) === 10);
        test.ok(cs.debtAmountTo(billId,jillId) === 10);
        //Transfer 100 of bobs debt to bill, to jill:
        cs.transferDebt(billId,bobId,jillId,100);
        //check that all existing debt is transferred, but no more
        test.ok(cs.debtAmountTo(bobId,billId) === 0);
        test.ok(cs.debtAmountTo(billId,jillId) === 0);
        test.ok(cs.debtAmountTo(bobId,jillId) === 10);
        test.done();
    },

        transferTooMuchDebt_test2 : function(test){
        let cs = new CreditSystem(),
            bobId = cs.registerCharacter('bob'),
            billId = cs.registerCharacter('bill'),
            jillId = cs.registerCharacter('jill');
        //add debt
        cs.addDebt(bobId,billId,10);
        cs.addDebt(billId,jillId,20);
        //check:
        test.ok(cs.debtAmountTo(bobId,billId) === 10);
        test.ok(cs.debtAmountTo(billId,jillId) === 20);
        //Transfer 100 of bobs debt to bill, to jill:
        cs.transferDebt(billId,bobId,jillId,100);
        //check that all existing debt is transferred, but no more
        test.ok(cs.debtAmountTo(bobId,billId) === 0);
        test.ok(cs.debtAmountTo(billId,jillId) === 10);
        test.ok(cs.debtAmountTo(bobId,jillId) === 10);
        test.done();
    },

};
