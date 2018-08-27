require.config({
    baseUrl : "/src",
    paths:{
        underscore:"/libs/underscore-min",
        d3:"/libs/d3.min",
    },
    shim:{
        underscore:{
            exports:'_',
        },
    }
});

require(['underscore','d3','util','faceGen1','triangleLine','bostock','lineDraw','grid','treeAttempt','bezierTest','arcExpansion','faceTest2','circleExperiment','polygonCycle','hexagon','orbit','RandDrawWalk'],function(_,d3,util,fg1,triLine,bostock,lineDraw,grid,treeAttempt,bezierTest,arcExpansion,faceTest2,circleExperiment,polygonCycle,hexagon,orbit,RandDrawWalk){
    var width = 800,
        height = 500;

    triLine(drawCanvas("triangles","#first"),height,width,"triangles");
    bostock(drawCanvas("bostock","#triangles"),height,width,"bostock");
    lineDraw(drawCanvas('lineDraw','#bostock'),height,width,"lineDraw");
    treeAttempt(drawCanvas('treeAttempt','#lineDraw'),height,width,'treeAttempt');
    fg1(drawCanvas("faces","#treeAttempt"),height,width,"faces");
    bezierTest(drawCanvas('bezierTest','#faces'),height,width,"bezierTest");
    arcExpansion(drawCanvas('arcExpansion','#bezierTest'),height,width,'arcExpansion');
    faceTest2(drawCanvas('faceTest2','#arcExpansion'),height,width,'faceTest2');
    circleExperiment(drawCanvas('circleExperiment','#faceTest2'),height,width,'circleExperiment');
    polygonCycle(drawCanvas('polygonCycle','#circleExperiment'),height,width,'polygonCycle');
    hexagon(drawCanvas('hexagon','#polygonCycle'),height,width,'hexagon');
    grid(drawCanvas('grid','#hexagon'),height,width,'grid');
    orbit(drawCanvas('orbit','#grid'),height,width,'orbit');
    //RandDrawWalk(drawCanvas('randomDrawWalk','#orbit'),height,width,'randomDrawWalk');

    //Add a canvas to the screen, returning the context for use in a draw function
    function drawCanvas(name,before){
        var group = d3.select('body').insert("p",before).attr("id",name),
            title = group.append('h1').text(name),
            canvas = group.append("canvas")
            .attr("width",width)
            .attr("height",height),
            ctx = canvas.node().getContext("2d");
        ctx.strokeRect(0,0,width,height);
        return ctx;
    }
});


