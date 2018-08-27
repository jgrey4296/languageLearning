define(['underscore','d3'],function(_,d3){
    var LEAF = Symbol(),
        LINE = Symbol(),
        PUSH = Symbol(),
        POP = Symbol(),
        INCDEPTH = Symbol(),
        DECDEPTH = Symbol(),
        numSegments = 2,
        depth = 4;
    
    var treeAttempt = function(ctx,height,width){
        console.log("Setting interval");
        //window.setInterval(drawTree,2000);
        drawTree();

        function drawTree(){
            console.log("Drawing tree");
            ctx.clearRect(0,0,width,height);
            var tree = [LINE].concat(genTree(depth,numSegments)),
                startPoint = [Math.floor(width * 0.5),Math.floor(height * 0.9)],
                remainder = height * 0.8,
                currDepth = 0,
                lineLengths = Array(depth+2).fill(0).map(function(d){
                    var l = remainder * 0.5;
                    remainder *= 0.5
                    ;
                    return l;
                }),
                angle = Math.PI / (Math.floor(numSegments + 1));

            console.log("Tree"," LineLengths:",lineLengths);

            ctx.strokeStyle = "#000000";
            ctx.translate(startPoint[0],startPoint[1]);
            ctx.save();
            
            while(tree.length > 0){
                var curr = tree.shift();
                    switch(curr){
                    case LEAF:
                        console.log("leaf",lineLengths[currDepth]);
                        //ctx.beginPath();
                        ctx.strokeRect(0,0,20,20);//lineLengths[currDepth],lineLengths[currDepth]);
                        //ctx.arc(0,0,lineLengths[currDepth],0,2*Math.PI);
                        //ctx.stroke();
                        break;
                    case LINE:
                        //console.log("line",lineLengths[currDepth]);
                        ctx.beginPath();
                        ctx.moveTo(0,0);
                        ctx.lineTo(0,-lineLengths[currDepth]);
                        ctx.stroke();
                        break;
                    case PUSH:
                        //console.log("push");
                        ctx.rotate(-angle);
                        break;
                    case POP:
                        //console.log("pop");
                        ctx.rotate(angle);
                        break;
                    case INCDEPTH:
                        //console.log("incdepth");
                        ctx.save();
                        ctx.translate(0,-lineLengths[currDepth]);
                        currDepth++;
                        break;
                    case DECDEPTH:
                        //console.log("decdepth");
                        ctx.restore();
                        currDepth--;
                        break;
                    }                
            }
            console.log("Tree Drawn");
        }
    };


    var genTree = function(depth,segments){
        var treeString = [INCDEPTH],
            sideSegments = Math.floor(segments * 0.5);
        
        if(depth > 0){
            Array(sideSegments).fill(0).forEach(function(d){ treeString.push(PUSH); });
            //treeString.push(PUSH);
            Array(sideSegments).fill(0).forEach(function(d){
                //treeString.push(POP);
                treeString.push(LINE);
                treeString = treeString.concat((genTree(depth -1, segments)));
                treeString.push(POP);
            });
            if(segments%2===1){
                treeString.push(LINE);
                treeString = treeString.concat((genTree(depth -1, segments)));
            }
                treeString.push(POP);
            Array(sideSegments).fill(0).forEach(function(d){
                treeString.push(LINE);
                treeString = treeString.concat((genTree(depth -1, segments)));
                treeString.push(POP);
            });
        }else{
            treeString.push(LEAF);
        }
        treeString.push(DECDEPTH);
        return treeString;
    };
    
    return treeAttempt;
});
