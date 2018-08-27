define(['underscore','d3'],function(_,d3){

    var Orbit = function(canvas,height,width,id){
        var midX = width * 0.5,
            midY = height * 0.5,
            rad = 2 * Math.PI,
            orbitPoint = 0,
            orbitDistance = 50,
            orbitInc = rad * 0.001,
            distanceInc = 0.5,
            c = canvas;

        window.requestAnimationFrame(draw);

        function draw(){
            //c.clearRect(0,0,width,height);
            drawCircle(midX,midY);
            drawCircle(midX,midY,orbitPoint,orbitDistance);
            drawCircle(midX,midY,orbitPoint+Math.PI,orbitDistance);

            if(orbitPoint > 2*Math.PI) orbitPoint = 0;
            orbitPoint += orbitInc;

            if(orbitDistance > 200 || orbitDistance < 0) distanceInc *= -1;
            orbitDistance += distanceInc;
            
            window.requestAnimationFrame(draw);
        };

        function drawCircle(cx,cy,rot,dx,r=1){
            c.save();
            c.translate(cx,cy);
            c.rotate(rot);
            c.translate(dx,0);
            c.beginPath();
            c.arc(0,0,r,0,rad);
            c.fill();

            c.restore();
        };

        
        
    };

    
    return Orbit;
});
