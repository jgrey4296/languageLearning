define(['underscore','d3'],function(_,d3){

    var GenerateFaces = function(ctx,height,width){
        var cenX = 0.5 * width,
            quarterX = 0.25 * width,
            topMidLine = 0.25 * height;

        ctx.translate(cenX,topMidLine);
        ctx.save();
        //draw the background of the face

        
        ctx.restore();
        ctx.save();
        //draw an eye
        ctx.translate(-quarterX * 0.5,0);
        ctx.beginPath();
        ctx.arc(0,0,50,0,2 * Math.PI);
        ctx.fill();
        
        ctx.restore();
        ctx.save();
        //the other eye
        ctx.translate(quarterX * 0.5,0);
        ctx.beginPath();
        ctx.arc(0,0,50,0,2*Math.PI);
        ctx.fill();

        ctx.restore();
        ctx.save();
        ctx.translate(0,topMidLine);
        ctx.beginPath();
        ctx.arc(0,0,100,0,Math.PI);
        ctx.fill();
        
    };
    
    
    return GenerateFaces;
});
