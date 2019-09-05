/* jshint esversion : 6 */
require.config({
    baseUrl : "/",
    paths:{
        d3 : "/libs/d3.min",
        underscore:"/libs/underscore-min",
        EL: '/libs/EL.min',
        BTree : '/libs/bTreeSimple',
        PriorityQueue : '/libs/priorityQueue',
        lodash : '/libs/lodash'
    },
    shim:{
        underscore:{
            exports:'_',
        },
    }
});

require(['d3','lodash','./src/utils','./src/BarChart','./src/MT_Types','./src/analysis'],function(d3,_,utils,BarChart,MT_Types,analysis){
    "use strict";
    const width = 800,
          height = 800,
          createBarChart = ()=>{
              return new BarChart(d3.select('body').append('svg')
                                  .attr('width',width).attr('height',height),width,height);
          };

    d3.json('data/AIIDEDemoLibrary.json',function(data){
        //get the individual microtheories
        let microtheories = utils.getMicroTheories(data),
            convertedMTs = microtheories.map(d=>new MT_Types.MicroTheory(d)),
            socialGames = utils.getSocialGames(data),
            convertedSGs = socialGames.map(d=>new MT_Types.SocialGame(d));
        console.log('Demo Library:',[convertedMTs,convertedSGs]);
     });

    //A social Game
    d3.json('data/Bully.json',function(data){
        let bullyMTs = utils.getMicroTheories(data).map(d=>new MT_Types.MicroTheory(d)),
            bullySGs = utils.getSocialGames(data).map(d=>new MT_Types.SocialGame(d));
        console.log('Bully converted:',[bullyMTs,bullySGs]);
    });

    //A Character
    d3.json('data/Doug.json',function(data){
        console.log('Doug:',data);
    });

    //A Social Network
    d3.json('data/PromWeekCharacters.json',function(data){
        console.log('Characters:',data);
    });

    //A Game Setup
    d3.json('data/QuickPlayStartState3.json',function(data){
        console.log('Quick Play State:',data);
    });

    //Trigger Rules
    d3.json('data/triggers.json',function(data){
        let triggerMTs = utils.getMicroTheories(data).map(d=>new MT_Types.MicroTheory(d)),
            triggerSGs = utils.getSocialGames(data).map(d=>new MT_Types.SocialGame(d));
        console.log('Trigger MTs and SGs:',[triggerMTs,triggerSGs]);

        console.log('Trigger rule initiator rule set size stats:',analysis.avgInitiatorRuleSetSize(triggerMTs));

        console.log('Trigger rule definition complexity:',analysis.ruleComplexityStats(triggerMTs));

        console.log('Trigger rule initiatorRule weight aggregates:',analysis.balanceOfWeights(triggerMTs));
        
    });
    
     
});
