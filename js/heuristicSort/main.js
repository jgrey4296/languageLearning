/**A Simple test program to demonstrate sorting a list of non-numeric values
   only by specifying relations between the individual values.
   Ie: [a,b,c,d] sorted using the information (a<c,b<c,b<d)
   @module heuristicSort
 */

//imports
var readline = require('readline');
var us = require("./underscore");
//Set up readline
var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false,
});

console.log("Heuristic Sorting Test 1:");

//The values to sort
var theLetters = "love hate war peace friendship betrayal revenge learning happiness".split(" ");

//shuffle them into a random order
var shuffled = us.shuffle(theLetters);

//The object to hold data about the sorting
var sorted = {};

//Initially defaults to each value ('letter')
//as not being more or less than anything else
//with no bounds on its location
for (var x in shuffled){
    var letter = shuffled[x];
    sorted[letter] = {
        "letter": letter,
        less:[],
        more:[],
        lBound : undefined,
        rBound : undefined
    };
}


//Get two letters
var pair = us.sample(us.keys(sorted),2);
console.log("Use 'a' and 'f' for choices...");
console.log("Which is more important:",pair[0],pair[1]);
//Loop:
rl.on('line',function(line){
    //Based on response,
    //add values to more/less as appropriate in sorted
    if(line === "a"){
        //the left value is greater
        sorted[pair[0]].less.push(pair[1]);
        sorted[pair[1]].more.push(pair[0]);
    }else if(line === "f"){
        //right value is greater
        sorted[pair[1]].less.push(pair[0]);
        sorted[pair[0]].more.push(pair[1]);
    }


    //find out how many items still have no relations
    var allUnsorted = us.filter(us.pairs(sorted),function(d){
        //if either are empty
        if(d[1].less.length === 0
           || d[1].more.length === 0){
            return true;
        };
        return false;
    });

    //if under 3 items have either less or more empty,
    //proceed to give them boundaries
    if(allUnsorted.length < 3){
        console.log("All sorted:");

        var bounds = calcBounds(sorted,us.keys(sorted));
        
        //        console.log("Final Sorting:",sorted);
        var finalSorted = us.sortBy(us.values(sorted),
                               function(d){
                                   return d.lBound;
                               });

        //counts:
        for(var x in finalSorted){
            var obj = finalSorted[x];
            console.log("Obj: ",obj.letter," Less: ",obj.less.length, obj.less.join(" "))
            console.log("Obj: ",obj.letter," More: ",obj.more.length, obj.more.join(" "));
            console.log("Obj: ",obj.letter," Bounds: ",obj.lBound, " , ", obj.rBound);
            console.log();
        }
        //Once boundaries have been given, quit
        process.exit();
    }


    //Having finished yet, so get a new pairing
    //and check that it hasnt been used before
    while(us.indexOf(sorted[pair[0]].less,pair[1]) > -1
          || us.indexOf(sorted[pair[0]].more,pair[1]) > -1
          || us.indexOf(sorted[pair[1]].less,pair[0]) > -1
          || us.indexOf(sorted[pair[1]].more,pair[0]) > -1){
        pair = us.sample(us.keys(sorted),2);
    }
    //For the next loop:
    console.log("Which is bigger:",pair[0],pair[1]);
});


//--------------------
//Heustic sorting algorithm:

//Take a dictionary, recursively
//and calculate bounds of a subselection of it
var calcBounds = function(dict, subselection){
    for(var x in subselection){
        var letter = subselection[x];
        //console.log("Calculating:",letter);
        calcLBound(dict,letter);
        calcRBound(dict,letter);
    }
    
};

//Recursively calculate the upper bound
var calcRBound = function(dict,value){
    var letter = dict[value];
    //If its been defined, exit
    if(letter.rBound !== undefined){
        return;
    }

    //Guard to stop infinite recursion
    //without assigning an actual value
    letter.rBound = null;

    //For each relation that is greater than the current:    
    for(var x in letter.more){
        var moreLetter = dict[letter.more[x]];
        //Calculate ITs rBound
        calcRBound(dict,moreLetter.letter);
    }

    //Get the smallest upper bound of things greater than this:
    var min = minOfArray(dict,letter.more);
    //Say that this upper bound must be at least 1 smaller than anything above it
    dict[letter.letter].rBound = min - 1;
    return;    
};

//Perform the same calulation as calcRBound, but swapping min/max:
var calcLBound = function(dict,value){
    var letter = dict[value];
    //if its already been calculated:
    if(letter.lBound !== undefined){
        return;
    }

    //guard:
    letter.lBound = null;

    //For everything smaller than this:
    for(var x in letter.less){
        var lessLetter = dict[letter.less[x]];
        calcLBound(dict,lessLetter.letter);
    }

    //Get the biggest minimum value:
    var max = maxOfArray(dict,letter.less);
    //Assign a value one bigger than anything smaller than it
    dict[letter.letter].lBound = max + 1;
    return;
}

//Get the max lBound of an array of entries
var maxOfArray = function(dict,array){
    var max = 0;
    for(var i in array){
        var curr = dict[array[i]];
        if(curr.lBound
           && curr.lBound !== null
           && curr.lBound > max){
            max = curr.lBound;
        }
    }

    return max;
};

//Get the min rBound of an array of entries
var minOfArray = function(dict,array){
    var min = us.keys(dict).length;
    for(var i in array){
        var curr = dict[array[i]];
        if(curr.rBound
           && curr.rBound !== null
           && curr.rBound < min){
            min = curr.rBound;
        }
    }
    return min;
};
