/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['../libs/lodash','./Loan'],function(_,Loan){
    "use strict";
    let nextId = 0;
    
    let CreditSystem = function(){
        //name -> id
        this.characters = {};
        //record of characters outstanding debts
        // { id : { id : { nonspecific : 0, specific : [loanObj] }
        this.debts = {};
        //record of characters outstanding credits
        //{ id : [id,amnt] }
        this.credits = {};
    };

    //Add a new specific, interest bearing debt 
    CreditSystem.prototype.newLoan = function(borrowerId,lenderId,principal,rate,length,isCompound){
        this.setupRecords(borrowerId,lenderId);
        let theLoan = new Loan(borrowerId,lenderId,principal,rate,length,isCompound),
            debtRecord = this.debts[borrowerId],
            creditRecord = this.credits[lenderId];

        debtRecord[lenderId].specific.push(theLoan);
        creditRecord[borrowerId].specific.push(theLoan);
    };


    //Add a non-specific, non-interest bearing debt amount
    CreditSystem.prototype.addDebt = function(borrowerId,lenderId,amnt){
        this.setupRecords(borrowerId,lenderId);
        let debtRecord = this.debts[borrowerId],
            creditRecord = this.credits[lenderId];

        debtRecord[lenderId].nonspecific += amnt;
        creditRecord[borrowerId].nonspecific += amnt;
    };

    //Pay a non-specific, non-interest bearing debt amount
    CreditSystem.prototype.payDebt = function(borrowerId,lenderId,amnt){
        let debtRecord = this.debts[borrowerId],
            creditRecord = this.credits[lenderId];

        if(debtRecord[lenderId] === undefined || creditRecord[borrowerId] === undefined){
            throw new Error('paying back an unregistered debt');
        }

        debtRecord[lenderId].nonspecific -= amnt;
        creditRecord[borrowerId].nonspecific -= amnt;

        if(debtRecord[lenderId].nonspecific < 0 && creditRecord[borrowerId].nonspecific < 0){
            debtRecord[lenderId].nonspecific = 0;
            creditRecord[borrowerId].nonspecific = 0;
        }        
    };

    //Boolean method to check if there is a debt relationship:
    CreditSystem.prototype.hasDebtTo = function(borrowerId,lenderId){
        let borrowerToLender = this.debts[borrowerId][lenderId] !== undefined,
            lenderToBorrower = this.credits[lenderId][borrowerId] !== undefined;

        if(borrowerToLender !== lenderToBorrower){
            console.log(borrowerToLender,lenderToBorrower);
            throw new Error('non-matching debts');
        }
        return borrowerToLender && lenderToBorrower;
    };

    //get the aggregated debt amount from borrower to lender
    CreditSystem.prototype.debtAmountTo = function(borrowerId,lenderId){
        //todo: map over specific debts too
        let borrowerAmntToLender = this.debts[borrowerId][lenderId] || 0,
            lenderAmntToBorrower = this.debts[lenderId][borrowerId] || 0;
        return borrowerAmntToLender - lenderAmntToBorrower;
    };
    
    CreditSystem.prototype.netWorth = function(id){
        //todo: map over specific debts
        let totalDebts = _.values(this.debts[id]).reduce((m,v)=>m+=v,0),
            totalCredits = _.values(this.credits[id]).reduce((m,v)=>m+=v,0);
        return totalCredits - totalDebts;        
    };

    //if bob (lender)  owes bill (hub) 5, and bill owes jill (borrower) 10,
    //then enable transfer of bob's debt to jill instead
    //amnt is optional
    CreditSystem.prototype.transferDebt = function(hubId,borrowerToHubId,lenderToHubId,amnt){
        amnt = amnt || Infinity;
        //get the lender amount
        let hcDebt = this.debtAmountTo(hubId,lenderToHubId),
            //get the borrower amount
            dhDebt = this.debtAmountTo(borrowerToHubId,hubId),
            //reduce the d->h debt amount by min(h->cDebt,d->hDebt,amnt)
            modAmount = Math.min.apply(null,[hcDebt,dhDebt,amnt]);
        //pay the debt from borrower to hub
        this.payDebt(borrowerToHubId,hubId,modAmount);
        this.payDebt(hubId,lenderToHubId,modAmount);
        //add the debt from borrower to lender
        this.addDebt(borrowerToHubId,lenderToHubId,modAmount);        
    };
    
    //----------------------------------------------------------------------
    //utilies:
    CreditSystem.prototype.setupRecords = function(borrowerId,lenderId){
        let debtRecord = this.debts[borrowerId],
            creditRecord = this.credits[lenderId];

        if(debtRecord === undefined || creditRecord === undefined){
            throw new Error('missing individual');
        }
        
        if(debtRecord[lenderId] === undefined){
            debtRecord[lenderId] = {
                nonspecific : 0,
                specific : []
            };
        }
        if(creditRecord[borrowerId] === undefined){
            creditRecord[borrowerId] = {
                nonspecific : 0,
                specific : []
            };
        }
    };
    
    CreditSystem.prototype.registerCharacter = function(name){
        if(this.characters[name] !== undefined){
            throw new Error('Character already exists');
        }
        let newId = nextId++;
        this.characters[name] = newId;
        this.debts[newId] = {};
        this.credits[newId] = {};
        return newId;
    };

    
    return CreditSystem;    
});
