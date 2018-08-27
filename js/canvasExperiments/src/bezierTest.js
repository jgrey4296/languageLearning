
define(['underscore','d3'],function(_,d3){

    var BezierTest = function(ctx,height,width,idName){

        var cenY = Math.floor(height * 0.5),
            //draw points:
            lpX = Math.floor(width * 0.3),
            rpX = Math.floor(width * 0.7),
            //bounds:
            yTopBound = Math.floor(height * 0.1),
            yBotBound = Math.floor(height - yTopBound),
            //control points:
            cpX = lpX,
            cpY = cenY,
            cpX2 = rpX,
            cpY2 = cenY,
            dir = 1;
        
        window.requestAnimationFrame(draw);


        function draw(){
            ctx.clearRect(0,0,width,height);

            ctx.beginPath();
            ctx.moveTo(lpX,cenY);
            ctx.bezierCurveTo(cpX,Math.floor(cpY),cpX2,cpY2,rpX,cenY);
            ctx.stroke();

            ctx.fillRect(cpX,Math.floor(cpY),3,3);
            ctx.fillRect(cpX2,Math.floor(cpY2),3,3);
            
            //mod:
            cpY += dir;
            cpY2 -= dir;
            if(cpY < yTopBound){
                dir *= -1;
            }
            if(cpY > yBotBound){
                dir *= -1;
            }

            
            window.requestAnimationFrame(draw);
        }
    };


    return BezierTest;
});
