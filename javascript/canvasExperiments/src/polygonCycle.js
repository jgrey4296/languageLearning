define(['underscore','d3','util'],function(_,d3,util){

    var PolygonCycle = function(ctx,height,width,id){
        var centre = [width*0.5,height*0.5],
            radius = 200,
            polygon = 2;
        window.setInterval(drawPolygon,500);

        function drawPolygon(){
            ctx.clearRect(0,0,width,height);
            util.drawPolygon(ctx,centre,radius,polygon,true);
            polygon = polygon>14 ? 2 : polygon+1;
        }
    };
    
    return PolygonCycle;
});
