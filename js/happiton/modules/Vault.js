/* jshint esversion : 6 */
if(typeof define !== 'function'){
    var define = require('amdefine')(module);
}

define(['underscore','./Individual','./Room','../libs/Rete.min','./ReteActions/ReteActions_Aggregated'],function(_,Individual,Room,Rete,ReteActions_Aggregated){
    "use strict";
    var Vault = function(populationSize,gridSize){
        console.log("Setting up Vault Simulation");
        this.simulationState = "inactive";
        this.simulatedDays = 0;

        //Activate the action constructor, binding context to actions
        this.rete = new Rete(ReteActions_Aggregated(this));
        
        /** @type {Array.<Individual>} */
        this.population = [];

        this.resources = {
            "water" : 100,
            "power" : 100,
            "food" : 100,
            "space" : 100,
            "rawMaterials" : 100
        };

        this.normValues = {
            //The number of days in a week
            "weekLength" : Math.floor(5 + Math.random() * 14),
            //number of weeks in a month
            "monthLength" : Math.floor(2 + Math.random() * 14),
            //number of genders:
            "genders" : Math.floor(Math.random() * 5)
        };
        
        //Root node of the graph of the vault rooms
        this.positionGrid = Array(gridSize).fill(0).map(d=>Array(gridSize).fill(0));
        //The entrance to the vault
        this.entrance = new Room("Entrance",0,{x:0,y:0});
        this.updatePositionGrid(this.entrance);

        //room constructor:
        this.Room = Room;
        //Invidual constructor
        this.Individual = Individual;

        //The global schedule of the simulation
        this.schedule = {
            "Sleep" : [],
            "Wakeup" : [],
            "GetDressed" : [],
            "Eat Breakfast" : [],
            "Slot 1" : [],
            "Slot 2" : [],
            "Break" : [],
            "Slot 3" : [],
            "Slot 4" : [],
            "Lunch" : [],
            "Slot 5" : [],
            "Slot 6" : [],
            "Dinner" : [],
            "Evening" : []
            //repeat
        };
        this.currentScheduleStep = 0;


        //initialise:
        this.init(populationSize);
    };

    Vault.prototype.init = function(popSize){
        console.log("Initialising");
        //create the initial vault
        //get the neighbours
        let entranceNeighbours = this.getRoomNeighbours(this.entrance);
        //todo: add the basics: 1 elevator, dorm, refectory,power,hyrdo,water

        

        //--------------------
        //create population
        this.population = Array(popSize).fill(0).map(d=>new Individual(this,this.entrance)).sort((a,b)=>b.lastName < a.lastName);
        //link relatives:
        let relatives = this.population.reduce(function(m,v){
            if(m[v.lastName] === undefined){ m[v.lastName] = []; }
            m[v.lastName].push(v);
            return m;
        },{});
        _.values(relatives).forEach(function(d){
            d.forEach(e=>e.relations.blood = _.reject(d,d=>d===e));
        });
        //--------------------
        //Print out information about each individual
        console.log(`There are now ${this.population.length} individuals in the vault`);
        this.population.forEach(function(d){
            let relatives = d.relations.blood.map(d=>d.firstName);
            console.log(`\t${d.firstName} ${d.middleName[0] || ""} ${d.lastName}, Blood Relations: ${relatives.length > 0 ? relatives : "none"}`);
        });
    };

    /**
       Increment the simulation by a schedule step
     */
    Vault.prototype.stepSim = function(){
        //assert and retract world based facts like resources?

        //step the rete net
        this.rete.stepTime();
        
        //the part of the day it is:
        let scheduleStep = _.keys(this.schedule)[this.currentScheduleStep],
            //check resources:
            numResourcesBelowZero = _.values(this.resources).reduce(function(m,v){
                if(v < 0) {
                    return ++m;
                }
                return m;
            },0);
        //print it out:
        console.log(`\nDay ${this.simulatedDays} Period: ${scheduleStep}`);
        console.log(`\t\tResources: ${JSON.stringify(this.resources)}`);

        //Kill everyone if necessary
        if(2 <= numResourcesBelowZero){
            console.log(`Everybody died because of lack of resources`);
            this.population.forEach(d=>d.alive = false);
            this.simulationState = 'dead';
            return;
        }
        
        
        //for the population, perform the schedule step
        this.population.forEach(function(d){
            d.perform(scheduleStep);
        });
        
        //Step to the next schedule step, resetting and going to the next day when necessary
        this.currentScheduleStep = ++this.currentScheduleStep % _.keys(this.schedule).length;
        if(this.currentScheduleStep === 0){
            this.simulatedDays++;
        }
    };
    //---------- End of Step Sim

    
    //add a room into the position grid
    Vault.prototype.updatePositionGrid = function(room){
        this.positionGrid[room.position.x][room.position.y] = room;
    };

    //get the neighbours of a specified room
    //@returns the actual room objects
    Vault.prototype.getRoomNeighbours = function(room){
        let vRef = this,
            positions = [["up",0,-1],["down",0,1],["left",-1,0],["right",1,0]],
            startPos = room.position,
            neighbours = positions.reduce(function(m,v){
                let row = vRef.positionGrid[startPos[0]+v[1]] || [],
                    col = row[startPos[1]+v[2]];
                m[v[0]] =  col;
                return m;
            },{},this);
        return neighbours;
    };

    Vault.prototype.modifyResource = function(resource,operator,value){
        if(this.resources[resource] === undefined){
            throw new Error(`Undefined Resource: ${resource}`);
        }
        if(operator === '-'){
            this.resources[resource] -= value;
        }else if(operator === '+'){
            this.resources[resource] += value;
        }
        if(this.resources[resource] <= 0){
            this.resources[resource] = 0;
        }
    };

    
    return Vault;    
});
