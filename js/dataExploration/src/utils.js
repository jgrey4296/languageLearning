/* jshint esversion : 6 */
define(['lodash'],function(_){
    "use strict";
    let utils = {};

    /*
      MicroTheory based utilities:
    */
    //Get microtheories
    utils.getMicroTheories = function(root){
        try{
            let microTheories = root.CiFLibraries.Microtheories.Microtheory;
            //fix the names
            microTheories.forEach(d=>{
                d.name = d.Name.$t;
            });
            return microTheories;
        }catch(error){
            return [];
        };        
    };

    utils.getSocialGames = function(root){
        try{
            let socialGames = root.CiFLibraries.SocialGameLibrary.SocialGame;
            if(socialGames instanceof Array){
                return socialGames;
            }else{                  
                return [socialGames];
            }
        }catch(error){
            return [];
        }
    };
    
    //verify that all of fields of a type are the same:
    utils.verify = function(arr){
        //get all the keys, create an entry for each:
        let base = _.flatten(arr.map(d=>_.keys(d))).reduce((m,v)=>{
            if(m[v] === undefined){
                m[v] = {};
            }            
            return m;            
        },{}),
            extractedTypes = arr.reduce((m,v)=>{
                let keys = _.keys(v);
                keys.forEach(d=>{
                    if(v[d] === false){
                        m[d].Boolean = 1;
                    }else if(v[d] === true){
                        m[d].Boolean = 1;
                    }else if(typeof v[d] === 'string'){
                        m[d].String = 1;
                    }else if(typeof v[d] === 'number'){
                        m[d].Number = 1;
                    }else if(v[d] instanceof Array){
                        m[d].Array = 1;
                    }else{
                        m[d].Object = 1;
                    }
                });
                return m;
            },base);
        return extractedTypes;        
    };
    return utils;
    
});

