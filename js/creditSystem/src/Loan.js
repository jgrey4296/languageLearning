/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['../libs/lodash','./round'],function(_,Round){
    "use strict";
    
    let Loan = function(borrower,lender,principal,rate,length,isCompound){
        //consts:
        this.borrower = borrower;
        this.lender = lender;
        this.principal = principal;
        this.rate = rate;
        this.length = length;
        this.isCompound = isCompound || false;
        this.totalValue = this.atYear(length);
        this.principalToInterestRatio = 0.5;
        //----------
        //mutables:
        this.currentValue = principal;//current amount of the loan
        this.remainingLength = length;//how long till maturity
        this.amountPaid = 0;//the balance paid
        this.outstandingPrincipal = principal;//separate record of the principal
    };

    //if the full sum has been paid:
    Loan.prototype.isPaidOff = function(){
        return this.outstandingBalance() <= 0;
    };

    //Calculate the amount still owed
    Loan.prototype.outstandingBalance = function(){
        return this.currentValue - this.amountPaid;
    };
    
    //Pay some of the loan
    Loan.prototype.amortize = function(amnt){
        this.outstandingPrincipal -= amnt;
        this.amountPaid += amnt;
    };
    
    //incrementally apply interest
    //return the amount of interest
    Loan.prototype.applyInterest = function(){
        if(this.isPaidOff()){ return 0; }
        let valueIncreasedBy = 0;
        if(!this.isCompound){
            //simple interest
            valueIncreasedBy = (this.outstandingPrincipal*this.rate);
            this.currentValue += valueIncreasedBy;
        }else{
            //compound interest
            valueIncreasedBy = (this.currentValue*this.rate);
            this.currentValue += valueIncreasedBy;
        }
        this.remainingLength--;
        return valueIncreasedBy;
    };

    //Forward calculate the tatal amount to owe 
    Loan.prototype.atYear = function(i,decimal){
        decimal = decimal || 2;
        let val = this.principal;
        if(!this.isCompound){
            val *= (1 + this.rate * i);
        }else{
            val *= Math.pow((1 + this.rate),i);
        }
        return Round.toDecimal(val,decimal);
    };


       
    return Loan;
});
