define(['underscore','d3'],function(_,d3){

    var CircleExperiment = function(ctx,height,width,id){
        var cenX = width*0.5,
            cenY = height*0.5,
            centre = [cenX,cenY],
            radius = width*0.1,
            squaresInstances = 0;
            numOfSquares = 5,
            MaxNumOfSquares = 300;

        ctx.clearRect(0,0,width,height);
        ctx.translate(cenX,cenY);
        ctx.save();
        ctx.beginPath();
        ctx.arc(0,0,radius,0,2*Math.PI);
        ctx.stroke();

        window.requestAnimationFrame(drawSquares);

        function drawSquares(){
            Array(numOfSquares).fill(0).forEach(function(d){
                var x = cenX - Math.random() * width,
                    y = cenY - Math.random() * height,
                    size = 5;

                ctx.fillStyle = inCircle(0,0,radius,x,y) ? "#AA53FF": "#03F36A";
                                    
                ctx.fillRect(Math.floor(x),Math.floor(y),size,size);
            });

            if(numOfSquares < MaxNumOfSquares){
                squaresInstances += numOfSquares;
                window.requestAnimationFrame(drawSquares);
            }
        }

    };

    var distanceSq = function(x1,y1,x2,y2){
        return Math.pow((x2-x1),2) + Math.pow((y2-y1),2);
    };

    var inCircle = function(cX,cY,r,x,y){
        var r2 = Math.pow(r,2),
            d = distanceSq(cX,cY,x,y);
        return d < r2;
    };
    
    
    return CircleExperiment;
});
