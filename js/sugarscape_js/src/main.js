/* jshint esversion : 6 */
require.config({
    baseUrl : "/src",
    paths: {
        lodash : "/libs/lodash",
        d3: "/libs/d3",
        perlin : "/libs/perlin",
        lineGraph : 'vis/lineGraph'
        
    },
    shim : {
        "perlin" : {
            exports : "noise",
        }
    },
});

require(['lodash','d3','perlin','World','Cell','lineGraph'],function(_,d3,noise,World,Cell,LineGraph){
    "use strict";
    //Specify general parameters:
    const gridParams = {
        width : 800,
        height : 500,
        size : 25, //x by x number of locations
        numOfAgents : 300,
        lineWidth : 3,
        //[x,y] array of main sugar sources
        sugarSources : [
            [4,4],[18,18]
        ],
        //Distance dropoff of sugar sources:
        sugarDistanceDrop : 6,
        //Draw neighbours:
        neighbourDraw : false,
        //draw social graph
        socialGraphDraw : false,
    },
          theWorld = new World(gridParams); 
    
    //Search from position a to b:
    theWorld.pathFind([1,1],[18,18]);

    
    theWorld.step();
    
    noise.seed(0.23431555335);
    
    let group = d3.select("body").insert("p").attr('id','sugarScape'),
        title = group.append('h1').text('SugarScape environment:'),
        canvas = group.append("canvas")
        .attr('width',gridParams.width)
        .attr('height',gridParams.height),
        ctx = canvas.node().getContext('2d');
    ctx.strokeStyle = "green";
    ctx.lineWidth = gridParams.lineWidth;
    ctx.strokeRect(0+1,0+1,gridParams.width-1,gridParams.height-1);
    //initial draw:
    theWorld.drawWorld(ctx,gridParams.width,gridParams.height,gridParams);

    //Draw the colour scale at the bottom in increments of 10:
    let reference = group.append('canvas')
        .attr('width',gridParams.width)
        .attr('height',50),
        refCtx = reference.node().getContext('2d'),
        refArray = Array(100).fill(0).map((d,i)=>i),
        refWidth = Math.floor(gridParams.width/100);
    console.log('Ref Array',refArray);
    refArray.forEach((d,i)=>{
        let colour = theWorld.colours(d),
            xPos = refWidth * i;
        refCtx.fillStyle = colour;
        refCtx.fillRect(xPos,0,refWidth,50);
    });

    //Draw command reference:
    let helpSection = group.append('p');
    helpSection.append('text').text('Enter : Log Grid Data').append('br');
    helpSection.append('text').text('k : Stop Simulation Timer').append('br');
    helpSection.append('text').text('r : Resume Simulation Timer').append('br');
    helpSection.append('text').text('n : Toggle neigbour display').append('br');
    helpSection.append('text').text('s : Step Simulation').append('br');    

    //details section:
    let detailSection = group.append('p');
    detailSection.append('text').attr('id','deathStat').text(`Num of Deaths: ${theWorld.deaths}`).append('br');
    detailSection.append('text').attr('id','liveAgents').text(`Num of Alive Agents : ${theWorld.agents.length}`).append('br');

    //----------
    //Line graph drawing:
    let turn = 0,
        graphData = {
            alive:  [],
            dead: []
        },
        graphSection = group.append('p').append('svg')
        .attr('width',gridParams.width)
        .attr('height',gridParams.height)
        .append('g'),
        theGraph = new LineGraph(graphSection,gridParams.width,gridParams.height,gridParams.numOfAgents,'blue');
    
    //Death stat update:
    function updateDeathStat(){
        graphData.alive.push([turn,theWorld.agents.length])
        graphData.dead.push([turn++,theWorld.deaths]);
        d3.select("#deathStat").text(`Num of Deaths: ${theWorld.deaths}`).append('br');
        d3.select("#liveAgents").text(`Num of Alive Agents: ${theWorld.agents.length}`).append('br');
        theGraph.update(graphData.alive);
    }
    theWorld.registerEvent('death', updateDeathStat);
    

    //Set up the simulation repeat timer:
    let timerID = setInterval(()=>{
            theWorld.step();
            theWorld.drawWorld(ctx,gridParams);
    },250);

    //user commands:
    let userCommands = {
        //log
        Enter : ()=>console.log(theWorld),
        //kill sim
        k : ()=>{
            if(timerID !== null){ clearInterval(timerID); timerID = null; }
        },
        //resume sim
        r : ()=>{
            if(timerID === null){ timerID = setInterval(()=>{
                theWorld.step();
                theWorld.drawWorld(ctx,gridParams);
            },500); }
        },
        //neigbour draw
        n : ()=>gridParams.neighbourDraw = !gridParams.neighbourDraw,
        //step
        s : ()=>{
            theWorld.step();
            theWorld.drawWorld(ctx,gridParams);
        },
        //draw social graph:
        g : ()=>gridParams.socialGraphDraw = !gridParams.socialGraphDraw,            
    };

    
    d3.select('body').on('keydown',function(){
        if(userCommands[d3.event.key] !== undefined){
            userCommands[d3.event.key]();
        }
    });

    //----------
    // Mouse click inspection:
    //----------
    
    d3.select('canvas').on('mousedown',function(){
        let pos = screenToElementPosition(d3.event,this),
            cellCoord = [Math.floor(pos.x/theWorld.cellSize.x),Math.floor(pos.y/theWorld.cellSize.y)],
            cell = theWorld.environment[theWorld.gridPositionToIndex(cellCoord[0],cellCoord[1])];
        console.log('Cell: ',cell);
        console.log('Agent: ',cell.populated);
        console.log('Neighbours',theWorld.getNeighbours(cell.index));
        console.log('\n');
        
    });

    //from http://stackoverflow.com/questions/55677/how-do-i-get-the-coordinates-of-a-mouse-click-on-a-canvas-element
     function screenToElementPosition(event,element){
        let totalOffsetX = 0,
            totalOffsetY = 0,
            canvasX = 0,
            canvasY = 0;
        //go up the parent chain, repeatedly offsetting
        do{
            totalOffsetX += element.offsetLeft - element.scrollLeft;
            totalOffsetY += element.offsetTop - element.scrollTop;
            element = element.offsetParent;
        }while(element);

        canvasX = event.clientX - totalOffsetX;
        canvasY = event.clientY - totalOffsetY;
        return {
            x : canvasX,
            y : canvasY
        };
    };
    
});
