define(['underscore','d3'],function(_,d3){

    var lineDrawing = function(ctx,height,width,id){
        var count = 0,
            maxCount = 1000,
            drawnLines = [],
            numOfArcs = 5,
            arcs = Array(numOfArcs).fill(0),
            cenX = Math.floor(width * 0.5),
            cenY = Math.floor(height * 0.5),
            maxRadius = cenX;

        var p = d3.select(`#${id}`),
            bounds = p.node().getBoundingClientRect();
        
        ctx.fillStyle = "#FFFFFF";
        ctx.fillRect(0,0,width,height);
        ctx.strokeStyle = "#000000";

        //draw some arcs first:
        arcs.forEach(function(d,i){
            //get a random radius
            var r = Math.random() * maxRadius;
            ctx.beginPath();
            ctx.arc(cenX,cenY,r,0,2 * Math.PI);
            ctx.stroke();
            
        });
        
        window.requestAnimationFrame(draw);
        
        function draw(){
            var start,stop,direction,currentPixel;

            //console.log("LineDraw:",p.node().getBoundingClientRect());
            
            //pick a random pair of points from a line already drawn, pick randomly if no lines drawn;
            if(drawnLines.length === 0 || Math.random() > 0.5){
                start = randomPoint(width,height);
            }else{
                start = getPointFromLines(drawnLines);
            }
                stop = randomPoint(width,height),
                direction = getVector(start,stop),
                currentPixel = addVectorToPoint(start,direction);

            
            //draw a line from one point to the other point,
            //stopping when you come across a pixel that is not white
            while(isWhite(ctx,currentPixel[0],currentPixel[1])
                  && inBounds(currentPixel,width,height)){
                currentPixel = addVectorToPoint(currentPixel,direction);
            }

            stop = currentPixel;
            drawnLines.push([start,stop]);
            ctx.beginPath();
            ctx.moveTo(Math.floor(start[0]),Math.floor(start[1]));
            ctx.lineTo(Math.floor(stop[0]),Math.floor(stop[1]));
            ctx.stroke();

            if(count++ < maxCount){
                window.requestAnimationFrame(draw);
            }else{
                console.log("Done");
            }
        };
    };

    var getPointFromLines = function(lineArray){
        if(lineArray.length === 0) return [0,0];
        
        var index = Math.floor(Math.random() * lineArray.length),
            lineStart = lineArray[index][0],
            lineEnd = lineArray[index][1],
            lineLength = getLineLength(lineStart,lineEnd),
            lineVector = getVector(lineStart,lineEnd),
            subLength = Math.random() * lineLength,
            subVector = [lineVector[0] * subLength, lineVector[1] * subLength],
            finalPoint = [lineStart[0] + subVector[0], lineStart[1] + subVector[1]];

        return finalPoint;
    };

    var getLineLength = function(start,end){
        return  Math.sqrt(Math.pow((end[0]-start[0]),2) + Math.pow((end[1]-start[1]),2))
    };
    
    var inBounds = function(pixel,width,height){
        if(pixel[0] < 0
           || pixel[0] > width
           || pixel[1] < 0
           || pixel[1] > height){
            return false;
        }
        return true;
    };
    
    var addVectorToPoint = function(point,vector){
        var newX = point[0] + vector[0],
            newY = point[1] + vector[1];        

        return [newX,newY];
    }
        
    var randomPoint = function(width,height){
        var x = Math.floor(Math.random() * width),
            y = Math.floor(Math.random() * height);
        
        return [x,y];
    };

    var getVector = function(p1,p2){
        var d1 = p2[0] - p1[0],
            d2 = p2[1] - p1[1],
        //normalise:
            length = Math.sqrt(Math.pow(d1,2) + Math.pow(d2,2)),
            normd1 = d1/length,
            normd2 = d2/length;

        return [normd1,normd2];
    };

    var isWhite = function(ctx,x,y){
        var pixel = ctx.getImageData(Math.floor(x),Math.floor(y),1,1);
        var data = pixel.data;
        if(data[0] !== 255
           || data[1] !== 255
           || data[2] !== 255){
            return false;
        }
        return true;
    };

    
    return lineDrawing;
});
