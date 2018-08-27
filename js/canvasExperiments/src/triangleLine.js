define(['underscore','d3'],function(_,d3){

    var drawTriangleLine = function(ctx,height,width,id){
        console.log("Rotating triangle line");
        var TWOPI = 2 * Math.PI,
            numOfSteps = 100,
            step = TWOPI / numOfSteps,
            rotate = 0,
            numOfTriangles = 15,
            triangles = Array(numOfTriangles).fill(0),
            maxTriangles = 300,
            triWidth = width / numOfTriangles,
            triHeight = height / numOfTriangles;
            
        var p = d3.select(`#${id}`),
            bounds = p.node().getBoundingClientRect(),
            innerHeight = window.innerHeight;
        
        window.requestAnimationFrame(draw);

        function draw(){
            bounds = p.node().getBoundingClientRect();
            if(bounds.top < 0 || bounds.top > innerHeight){
                window.requestAnimationFrame(draw);
                return;
            }

            ctx.clearRect(0,0,width,height);
            triangles.forEach(function(d,i){
                var curRotate = i%2==0 ? rotate : -rotate;
                ctx.save();
                ctx.translate(i * triWidth, i * triHeight);
                ctx.rotate(curRotate + i * (2 * Math.PI / 15));
                ctx.beginPath();
                ctx.moveTo(0,0);
                ctx.lineTo(0,30);
                ctx.lineTo(20,0);
                ctx.fill();
                ctx.restore();
            });

            rotate += step;
            if(numOfTriangles > maxTriangles) numOfTriangles = 15;
            if(rotate > 2 * TWOPI){
                rotate = 0;
                numOfTriangles += 15;
                triangles = Array(numOfTriangles).fill(0);
                triWidth = width / numOfTriangles;
                triHeight = height / numOfTriangles;
            }            
            window.requestAnimationFrame(draw);
        };
    };

    
    
    return drawTriangleLine;
});
