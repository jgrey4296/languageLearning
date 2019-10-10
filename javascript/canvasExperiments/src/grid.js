
define(['underscore','d3'],function(_,d3){

    var drawGrid = function(ctx,height,width){
        var gridSizeX = 21,
            gridSizeY = 21,
            sizeX = width / gridSizeX,
            sizeY = height / gridSizeY,
            //create the grid
            grid = Array(gridSizeX * gridSizeY).fill(0).map(function(d,i){
                return {
                    id: i,
                    colour : (i%2 ? "#FFFFFF" : "#000000")
                };
            }),
            pathLength = 30,
            pathStartEnd = _.sample(grid,2),
            path = dfsWalk(grid,pathStartEnd[0].id,pathStartEnd[1].id,gridSizeX,gridSizeY,pathLength),
            tree = pathToTree(path,gridSizeX,gridSizeY);

        _.values(tree).forEach(function(d){
            d.children.forEach(function(e){
                grid[e].colour = "blue";
            });
        });
        
        path.forEach(function(d){
            grid[d].colour = "red";
        });
        pathStartEnd.forEach(function(d){
            grid[d.id].colour = "green";
        });
        
        //Draw the grid;
        grid.forEach(function(d,i){
            var position = calcElementLocation(i,gridSizeX,gridSizeY,sizeX,sizeY);

            ctx.strokeStyle = "grey";
            ctx.fillStyle = grid[i].colour;
            ctx.fillRect(position[0],position[1],sizeX+1,sizeY+1);
            //ctx.strokeRect(position[0],position[1],sizeX,sizeY);
        });
    };

    var indexToGridPosition = function(i,gridSizeX,gridSizeY){
        var position = [i%gridSizeX,Math.floor(i/gridSizeY)];
        if(position[0] < 0 || position[0] >= gridSizeX || position[1] < 0 || position[1] >= gridSizeY){
            return undefined;
        }
        return position;
    };

    var gridPositionToIndex = function(pos,gridSizeX,gridSizeY){
        var index =  (pos[1] * gridSizeY) + pos[0];
        if(index >= (gridSizeX * gridSizeY) || index < 0
          || pos[0] < 0 || pos[1] < 0 || pos[0] >= gridSizeX || pos[1] >= gridSizeY){
            return undefined;
        }
        return index;
    };
    
    var calcElementLocation = function(i,gridSizeX,gridSizeY,sizeX,sizeY){
        var x = i % gridSizeX,
            y = Math.floor(i / gridSizeY),
            posX = x * sizeX,
            posY = y * sizeY;
        
        return [Math.floor(posX),Math.floor(posY)];
    };
    
    var dfsWalk = function(array,startIndex,endIndex,maxX,maxY,maxPath){
        console.log("Walking:",startIndex,endIndex,maxX,maxY,maxPath,array);
        var path = [startIndex],
            startPosition = indexToGridPosition(startIndex,maxX,maxY),
            endPosition = indexToGridPosition(endIndex,maxX,maxY),
            potentialMoves = [[1,0],[0,1],[-1,0],[0,-1]],
            timeout = 0;
        
        while(path.length < maxPath && path[path.length-1] !== endIndex && timeout < 25){
            //get the current position;
            var currentPosition = indexToGridPosition(path[path.length-1],maxX,maxY),
                currentDistance = distanceSquared(currentPosition,endPosition);
            var moves = potentialMoves.map(function(d){
                    return [currentPosition[0] + d[0], currentPosition[1] + d[1]];
                }),
                filteredMoves = moves.filter(function(d){
                    return !(d[0] < 0 || d[0] >= maxX || d[1] < 0 || d[1] >= maxY);
                }),
                minimisingMoves = filteredMoves.filter(function(d){
                    var potential = distanceSquared(d,endPosition)
                    return potential < (currentDistance + 2);
                }),
                selectedMove = _.sample(minimisingMoves,1)[0],
                neighbouring = potentialMoves.map(function(d){
                    return gridPositionToIndex([selectedMove[0] + d[0], selectedMove[1] + d[1]],maxX,maxY);
                });

            if(path.indexOf(gridPositionToIndex(selectedMove,maxX,maxY)) === -1 && _.intersection(path,neighbouring).length <= 1){
                path.push(gridPositionToIndex(selectedMove,maxX,maxY));
                timeout = 0;
            }else{
                timeout++;
            }
        }        
        return path;
    };

    var distanceSquared = function(posA,posB){
        return Math.pow(posB[0] - posA[0],2) + Math.pow(posB[1] - posA[1],2);
    };

    var pathToTree = function(pathIndices,gridSizeX,gridSizeY){
        var tree = pathIndices.reduce(function(m,v,i){
            if(m[v] === undefined){
                var children = pathIndices[i+1] ? [pathIndices[i+1]] : [];
                m[v] = {inPath : true, children: children};
            }
            return m;
        },{}),
            potentialChildren = [[0,1],[1,0],[-1,0],[0,-1]];
        
        pathIndices.forEach(function(d){
            var pathPosition = indexToGridPosition(d,gridSizeX,gridSizeY),
                childrenIndices = potentialChildren.map(function(e){
                    return gridPositionToIndex([pathPosition[0] + e[0],pathPosition[1] + e[1]],gridSizeX,gridSizeY);                    
                }).filter(function(e){ return e; }),
                currentChildren = tree[d].children;
                tree[d].children = _.union(currentChildren,childrenIndices);
        });
        
        return tree;
    };
    return drawGrid;
});
