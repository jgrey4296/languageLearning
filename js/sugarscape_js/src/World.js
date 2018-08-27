/* jshint esversion : 6 */
/**
   The world of the simulation

 */
define(['lodash','d3','perlin','Cell','Agent'],function(_,d3,noise,Cell,Agent){
    "use strict";
    /**
       @constructor
     */
    let World = function(gridParams){
        //Storage of all parameters passed in
        this.gridParams = gridParams;
        this.cellSize = {
            x : Math.floor(this.gridParams.width/this.gridParams.size),
            y : Math.floor(this.gridParams.height/this.gridParams.size)
        };
        
        //the individual cells
        this.environment = new Array(Math.pow(gridParams.size,2)).fill(0).map((d,i)=>new Cell(i));
        //The agents of the world
        this.deaths = 0;
        this.agents = _.sampleSize(this.environment,gridParams.numOfAgents).map(d=>new Agent(d));
        //the social graph of all agents
        this.socialGraph = new WeakMap();
        //colour scale:
        this.colours = d3.scale.pow()
            .domain([0,25,50,75,100])
            .range(['black','brown','yellow','green','white']);

        // let offset = Math.random() * 20;
        // //Initialise by perlin noise the sugar levels:
        // for(let i = 0; i < this.environment.length; ++i){
        //     let pos = this.indexToWorldPosition(i);
        //     this.environment[i].sugarLevel = Math.floor((noise.simplex2(pos[0],pos[1]) + 1) * 50);
        //     this.environment[i].capacity = Math.floor((noise.simplex2(pos[0] + offset,pos[1] * offset) + 1) * 50);
        // }

        //Initialise sugar level by distance from gridparam specified points:
        gridParams.sugarSources.forEach(d=>{
            console.log('Setting sugar of: ',this.gridPositionToIndex(d[0],d[1]));
            this.environment[this.gridPositionToIndex(d[0],d[1])].sugarLevel = 100;
        });
        
        // //BFS absorb sugar to start:
        this.BFS_Neighbours(gridParams.sugarSources.map(d=>this.environment[this.gridPositionToIndex(d[0],d[1])]),
                            (d,neighbours)=>{
                                d.absorbSugar(neighbours,gridParams.sugarDistanceDrop);
                            });

        this.registeredEvents = {};
        
    };

    World.prototype.registerEvent = function(name,cb){
        if(this.registeredEvents[name] === undefined){
            this.registeredEvents[name] = [];
        }
        this.registeredEvents[name].push(cb);
    };

    World.prototype.fireEvent = function(name){
        if(this.registeredEvents[name] !== undefined){
            this.registeredEvents[name].forEach(d=>d());
        }
    };
    
    //utilities:
    World.prototype.indexToWorldPosition = function(i){
        return [
            i%this.gridParams.size,
            Math.floor(i/this.gridParams.size)
        ];
    };

    World.prototype.gridPositionToIndex = function(x,y){
        return (y * this.gridParams.size) + x;
    };

    
    //The actual draw method:
    World.prototype.drawWorld = function(ctx,params){
        let sizeX = this.cellSize.x,
            sizeY = this.cellSize.y,
            //index to screen position:
            itop = (i)=>{
                let position = this.indexToWorldPosition(i);
                return [
                    position[0] * sizeX,
                    position[1] * sizeY
                ];
            },
            //offsets for centre:
            c_offsetX = Math.floor(sizeX * 0.5),
            c_offsetY = Math.floor(sizeY * 0.5),
            arcSize = c_offsetX * 0.5,
            //offsets for filling:
            f_offset = Math.floor(params.lineWidth * 0.5),
            l_offset = 2;

        ctx.strokeStyle = "black";
        ctx.fillStyle = "red";

        //draw each cell
        this.environment.forEach((d,i)=>{
            let pos = itop(i);
            d.tempColour = this.colours(d.sugarLevel);
            //center
            ctx.fillStyle = d.tempColour;
            ctx.fillRect(pos[0],pos[1],sizeX,sizeY);
            //border
            ctx.strokeStyle = d.edge;
            ctx.strokeRect(pos[0]+1,pos[1]+1,sizeX-2,sizeY-2);
         });

        //draw the agents
        this.agents.forEach((d,i)=>{
            let pos = itop(d.cell.index);
            ctx.fillStyle = d.colour;
            ctx.beginPath();
            ctx.arc(pos[0]+c_offsetX,pos[1]+c_offsetY,arcSize,0,2*Math.PI);
            ctx.fill();
            //draw the neighbour borders
            if(params.neighbourDraw){
                _.values(this.getNeighbours(d.cell.index)).forEach(e=>{
                    let pos = itop(e.index);
                    ctx.strokeStyle = d.colour;
                    ctx.strokeRect(pos[0]+l_offset * 0.5,pos[1]+l_offset * 0.5,sizeX-l_offset,sizeY-l_offset);
                });
            }
            //Draw links between the social graph members:
            if(params.socialGraphDraw){
                let neighbours = this.socialGraph.get(d) || [];
                ctx.strokeStyle = d.socialColour;
                neighbours.forEach(e=>{
                    let endPos = itop(e.cell.index);
                    ctx.beginPath();
                    ctx.moveTo(pos[0]+c_offsetX,pos[1]+c_offsetY);
                    ctx.lineTo(endPos[0]+c_offsetX,endPos[1]+c_offsetY);
                    ctx.stroke();
                });
            }
        });
    };

    //Step the simulation:
    World.prototype.step = function(){
        //step the environment
        _.shuffle(this.environment).forEach((d,i)=>{
            let neighbours = this.getNeighbours(i);
            d.simulate(neighbours);
        });

        //shuffle and step the agents:
        _.shuffle(this.agents).forEach((d,i)=>{
            let neighbours = this.getNeighbours(d.cell.index),
                stillAlive = d.simulate(neighbours);
            if(!stillAlive){
                this.deaths++;
            }
        });

        //remove dead agents
        this.agents = this.agents.filter(d=>d.statuses.alive);
        
        //update social graph
        this.agents.forEach(d=>{
            let newNeighbours = _.values(this.getNeighbours(d.cell.index)).filter(d=>d.populated !== null).map(d=>d.populated);
            this.updateSocialGraph(d,newNeighbours);
        });


        //call registered events
        this.fireEvent('death');
    };

    //update the social graph:
    World.prototype.updateSocialGraph = function(agent,neighbours){
        let currentNeighbours = this.socialGraph.get(agent) || [],
            newNeighbours = neighbours.concat(currentNeighbours).filter(d=>d.statuses.alive);
        this.socialGraph.set(agent,newNeighbours.slice(0,4));
        //check neighbours
        if(_.reject(_.values(neighbours),d=>d.populated===null).length !== 0){
            //if agent is next to another agent,
            agent.colour = agent.socialColour;
        }else{
            agent.colour = "green";
        }
        
    };

        
    //Get the neighbours, by index, returning actual cells
    //todo: get vision ranges
    World.prototype.getNeighbours = function(i,range){
        let xPos = i%this.gridParams.size,
            yPos = Math.floor(i/this.gridParams.size),
            neighbours = {};
        if(yPos > 0){
            //todo: get x < range && 0 < x squares
            neighbours.up = this.environment[i - this.gridParams.size];
        }
        if(yPos < this.gridParams.size-1){
            //todo: get x < range && x < gridParams.size
            neighbours.down = this.environment[i + this.gridParams.size];
        }
        if(xPos > 0){
            //todo: get x < range && 0 < x%gridParams.size
            neighbours.left = this.environment[i - 1];
        }
        if(xPos < this.gridParams.size-1){
            //todo: get x < range && x%gridParams.size < gridParams.size
            neighbours.right = this.environment[i+1];
        }
        return neighbours;
    };

    //Breadth first search from defined cells, applying the callback to each.
    //calback expected to be (cell,neighbourCells)=>{}
    World.prototype.BFS_Neighbours = function(startNodes,callback){
        let queue = _.clone(startNodes),
            found = new Set();
        while(queue.length > 0){
            let current = queue.shift();
            if(!found.has(current)){
                found.add(current);
                let neighbours = this.getNeighbours(current.index);
                callback(current,neighbours);
                queue = queue.concat(_.values(neighbours));
            }
        };
    };

    //Bidirectional search:
    World.prototype.pathFind = function(posA,posB){
        let ai = this.gridPositionToIndex(posA[0],posA[1]),
            bi = this.gridPositionToIndex(posB[0],posB[1]),
            cellA = this.environment[ai],
            cellB = this.environment[bi];

        cellA.edge = "green";
        cellB.edge = "green";

        let aPath = [cellA],
            bPath = [cellB],
            aFocus = true;

        while(this.distance(_.last(aPath).index,_.last(bPath).index)  >= 2 && aPath.length + bPath.length < Math.pow(this.gridParams.size,2) ){
            let focusPath = aFocus ? aPath : bPath,
                targetPath = aFocus ? bPath : aPath,
                frontier = _.last(focusPath),
                target = _.head(targetPath),
                neighbours = this.getNeighbours(frontier.index),
                neighbourDistances = _.values(neighbours).map(d=>[d,this.distance(d.index,target.index)]),
                //get the neighbour with the smallest distance to the target:
                sortedNeighbourDistances = neighbourDistances.sort((a,b)=>a[1] - b[1]),
                min = _.head(sortedNeighbourDistances);

            //console.log('Sorted and min',min,sortedNeighbourDistances);

            //min[0].edge = "purple";
            //add the min to the focus path:
            focusPath.push(min[0]);            
            //flip
            aFocus = !aFocus;
        }

        let finalPath = aPath.concat(_.reverse(bPath));
        //finalPath.forEach(d=>d.edge = "purple");
        console.log('final path',finalPath);
    };

    //expects indices
    World.prototype.distance = function(ai,bi){
        let posA = this.indexToWorldPosition(ai),
            posB = this.indexToWorldPosition(bi),
            dist = Math.pow(posB[0]-posA[0],2) + Math.pow(posB[1]-posA[1],2);
        return dist;
    };
    
    
    return World;
    
});
