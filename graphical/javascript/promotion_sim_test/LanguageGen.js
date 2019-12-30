/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['lodash','./Parse'],function(_,Parse){
    "use strict";
    let LanguageGen = {},
        grammar = {
            //Organisation generation:
            orgName : ["The ${orgDesc} ${orgType}","The ${orgType} of ${orgDesc} ${X}"],
            orgDesc : "Glorious Horrific Everlasting Silent Joyous Caring Unforgiving Forgiving ${syllableCombination}".split(/ /),
            orgType : "Hospital Church Collection Brotherhood Sisterhood Motherhood Fatherhood ${wayOfX}".split(/ /),
            wayOfX : "Way of ${X}",
            X    : "Blood Ivory Sleep Crumpets Iron Care ${syllableCombination}".split(/ /),
            //Ranks
            rank : "${?title} ${syllableCombination}",
            optPost : "${syllableCombination} and",
            title : "Commander of the ${?optPost},Sergeant at ${?optPost},Watchman of ${?optPost},Sister of ${?optPost},Chancellor of ${?optPost}".split(/,/),
            syllableCombination : "${startSyl.cap()}${#1-2:midSyl}${?midSyl2}${endSyl}",
            startSyl : "j,m,qu,pl,t,n".split(/,/),
            midSyl : "or,ar,eu,e,ie,u".split(/,/),
            midSyl2 : "hn,en,th,er,ar,orp".split(/,/),
            endSyl: "ny,t,mn,ew,rt,ylp,achy".split(/,/),
            //Position
            positionName : "Channeller Leader Group-head Minion Slave Owner Worker Menial".split(/ /),
            //Location
            locationName : "Temple Home Workplace Factory Palace Bar Market".split(/ /),
            //Char name
            charName : "${firstName} ${#0-2:middleName} ${surName}",
            firstName : "${syllableCombination}",
            middleName : "${syllableCombination}",
            surName : "${syllableCombination}",
            //atomic acts
            atomicAct : "Move Stroke Sing Grunt Align Hit Pickup Bow Shout Growl Say".split(/ /),
            //com channels
            communicationsChannel : "lipTightness eyelidHeight foreheadHeight smile jawHeight".split(/ /),


            //syllables mk2:
            voicelessStart : "p,t,c,ch,f,th,s,h".split(/,/),
            voicedStart : "b,d,g,j,v,th,z,m,y,w,r,l".split(/,/),
            consonantPhonemes : "pl,bl,cl,pr,br,tr,dr,cr,kr,gr,tw,dw,qui,pui,fl,sl,fr,thr,shr,wh,sw,r".split(/,/),
            startPhonemes : "${voicelessStart} ${voicedStart} ${consonantPhonemes}".split(' '),
            vowels : "a,e,i,o,u,oo,eu,ar,ee,ei,eh,ea,or".split(/,/),
            endPhonemes : "it,t,ra,ing,am,eam,re,er,ew,nt,r".split(/,/),

            middlePhonemes : "${vowels}${consonantPhonemes} ${vowels}".split(/ /),
            
            namemk2 : "${startPhonemes}${middlePhonemes}${endPhonemes}",
            
        };
    LanguageGen.grammar = grammar;
    LanguageGen.genParse = function(name){
        return Parse(grammar,name);
    };

    //Create a new rank (vertical hierarchy)
    LanguageGen.rankName = function(){
        return Parse(grammar,'rank');
    };

    //Create a position (horizontal)
    LanguageGen.positionName = function(){
        return Parse(grammar,'positionName');
    };

    //Specify a position related to the organisation
    LanguageGen.locationName = function(){
        return Parse(grammar,'locationName');
    };

    //Create a name of a character
    LanguageGen.charName = function(){
        return Parse(grammar,'charName');
    };

    //Create a name of an organisation
    LanguageGen.orgName = function(){
        return Parse(grammar,'orgName');
    };

    //Generate the atomic actions able to be performed
    LanguageGen.atomicAct = function(){
        return Parse(grammar,'atomicAct');
    };

    //Specify a free channel of information
    LanguageGen.communicationsChannel = function(){
        return Parse(grammar,'communicationsChannel');
    };

    return LanguageGen;
});
