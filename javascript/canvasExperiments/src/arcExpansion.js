define(['underscore','d3'],function(_,d3){

    var arcExpansion = function(ctx,height,width,idName){
        var cenX = width * 0.5,
            topY = height * 0.1,
            botY = height - topY,
            maxArcs = 15,
            startRadius = width * 0.1,
            radiusInc = (width * 0.8) / maxArcs,
            startAngleL = - (Math.PI * 0.5 + (Math.PI / maxArcs)),
            startAngleR = - (Math.PI * 0.5 - (Math.PI / maxArcs));
        
        
        ctx.translate(cenX,botY);        
        Array(maxArcs).fill(0).forEach(function(d,i){
            ctx.beginPath();
            ctx.arc(0,0,Math.floor(startRadius),
                    startAngleL,startAngleR);
            ctx.stroke();

            startRadius += radiusInc * 0.5;
            startAngleL -= (Math.PI / maxArcs) * 0.25;
            startAngleR += (Math.PI / maxArcs) * 0.25;

            if(startAngleL < -(Math.PI * 0.8)){
                startAngleL = -(Math.PI * 0.8);
            }
            if(startAngleR > -(Math.PI * 0.2)){
                startAngleR = -(Math.PI * 0.2);
            }
        });


    };


    return arcExpansion;
});
