/* jshint esversion : 6 */
define(['lodash'],function(_){
    const roundVal = 100;
    //export interface:
    let analysis = {};
    //round function:
    function jgRound(amnt){
        return (Math.round(amnt * roundVal)) / roundVal;
    }

    //get max, min, and avg for the initiator rule set sizes:
    analysis.avgInitiatorRuleSetSize = function(triggerRules){
        let lengths = triggerRules.map(d=>d.initiatorRuleSet.length),
            max = Math.max.apply(null,lengths),
            min = Math.min.apply(null,lengths),
            total = lengths.reduce((m,v)=>{
                m += v
                return m;
            },0),
            avg = total / triggerRules.length;
        return {
            max : max,
            min : min,
            avg : jgRound(avg),
        };
    };

    analysis.ruleComplexityStats = function(triggerRules){
        let definitions = triggerRules.map(d=>d.definition),
            defPredSizes = definitions.map(d=>d.predicates.length),
            max = Math.max.apply(null,defPredSizes),
            min = Math.min.apply(null,defPredSizes),
            total = defPredSizes.reduce((m,v)=>m += v, 0),
            avg = total / triggerRules.length;
        return {
            max : max,
            min : min,
            avg : jgRound(avg)
        };
    };

    analysis.balanceOfWeights = function(triggerRules){
        let weightAggregates = triggerRules.map(d=>{
            let val = {
                name : d.name,
                weights : null,
            },
                agg = d.initiatorRuleSet.reduce((m,v)=>{
                    if(v.weight === null) { return m; }
                    if(m[v.weight] === undefined){ m[v.weight] = 0; }
                    m[v.weight] += 1;
                    return m;
                },{});
            val.weights = agg;
            return val;
        });
        return weightAggregates;
    };
    

    return analysis;
});
