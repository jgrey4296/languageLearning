/* jshint esversion : 6 */
"use strict";
let prom = new Promise(function(resolve,reject){
    console.log('entered');
    resolve('blah');
});

let p2 = prom.then(function(val){
    console.log('val',val);
    let promArray = [];
    for(var i = 0; i < 10; i++){
        promArray.push(new Promise(function(re,rej){
            re(i);
        }));
    }
    return promArray;
});

let p3 = p2.then(function(val){
    console.log('val2',val);
    return Promise.all(val).then(function(eachVals){
        console.log('eachVal',eachVals);
        return 'aweghhh';
    }).then(function(val3){
        console.log('val3',val3);
        return 'aweghwahhh';
    });
});

p3.then(function(val4){
    console.log('val4',val4);
});

console.log('p3?',p3);
