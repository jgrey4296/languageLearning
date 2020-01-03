define(['underscore','d3','util'],function(_,d3,util){

    var Hexagon = function(ctx,height,width,id){
        var centre = [width*0.5,height*0.5],
            radius = 40,
            polygon = 2,
            columns = 10, //q
            rows = 6, //r
            positions = Array(columns*rows).fill(0).map(function(d){
                return {colour : "black"};
            }),
            height = (2 * radius),
            xInc = util.calcPoint([0,0],radius,1,6)[0],
            yInc = height * (3/4),
            count = 0,
            curIndex = offsetToIndex({q:0,r:0});

        console.log(`xInc: ${xInc}`);
        ctx.translate(100,100);
        //window.setInterval(drawPolygon,500);

        console.log("Positions:",positions);
        console.log(curIndex);
        positions[curIndex].colour = "red";
        console.log("Cur Index:",curIndex,indexToOffset(curIndex),offsetToCube(indexToOffset(curIndex)));        
        window.requestAnimationFrame(drawGrid);
        //get neighbours:
        //console.log("Cur index:",curIndex);
        
        //drawGrid();
        
        function drawGrid(){
                      //reset colours
            positions.forEach(function(d){
                d.colour = "black";
            });

            positions[curIndex].colour = "red";
            //set neighbours:
            var n_indices = neighbours(curIndex);
            n_indices.forEach(function(d){
                positions[d].colour = "blue";
            });

            
            ctx.clearRect(0,0,width,height);
            //draw each hexagon
            positions.forEach(function(d,i){
                //get the column and row
                var offset = indexToOffset(i),
                    //calculate position
                    //odd row offset:
                    xpos = offset.r&1 ? xInc + (offset.q * 2*xInc) : offset.q * 2*xInc,
                    ypos = offset.r * yInc;

                //Draw the hexagon itself
                util.drawPolygon(ctx,[xpos,ypos],radius,6,false,d.colour);
            });

            //increment focus:
            if(count++ > 15){
                curIndex++;
                console.log("Cur Index:",curIndex,indexToOffset(curIndex),offsetToCube(indexToOffset(curIndex)));
                count = 0;
            }
            //reset
            if(curIndex >= positions.length){
                curIndex = 0;
            }


            window.requestAnimationFrame(drawGrid);
        }
        
        function offsetToIndex(offset){
            return (offset.q) + (offset.r * columns);
        }

        function indexToOffset(i){
            return {
                q : Math.floor(i%columns),
                r : Math.floor(i/columns)
            }
        }
        
        //odd r offset
        function offsetToCube(offset){
            var x = offset.q - (offset.r - (offset.r%2)) /2,
                z = offset.r,
                y = -x-z;
            return {
                x: x,
                y: y,
                z: z
            };
        }
        
        function cubeToOffset(cube){
            var col = cube.x + (cube.z - (cube.z%2)) /2,
                row = cube.z;
            return {
                q : col,
                r : row
            };
        }
        
        function neighbours(index){
            var cube = offsetToCube(indexToOffset(index)),
                directions = [
                    [1,-1,0],[1,0,-1],[0,1,-1],
                    [-1,1,0],[-1,0,1],[0,-1,1]
                ],
                neighbours = directions.map(function(d){
                    return {
                        x: cube.x + d[0],
                        y: cube.y + d[1],
                        z: cube.z + d[2]
                    };
                }),
                n_offset = neighbours.map(function(d){
                    return cubeToOffset(d);
                }),
                n_offset_filtered = n_offset.filter(function(d){
                    return !(d.q < 0 || d.q >= columns || d.r < 0 || d.r >= rows);
                }),
                n_indices = n_offset_filtered.map(function(d){
                    return offsetToIndex(d);
                }),
                n_indices_filtered = n_indices.filter(function(d){
                    return d >= 0 && d < positions.length;
                });

            return n_indices_filtered;
        }

        
    };
    
    return Hexagon;
});
