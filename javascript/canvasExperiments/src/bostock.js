//From http://bost.ocks.org/mike/algorithms/
define(['underscore','d3'],function(_,d3){

    var Bostock = function(context,height,width){
        var N = 1 << 0,
            S = 1 << 1,
            W = 1 << 2,
            E = 1 << 3;

        var cellSize = 6,
            cellSpacing = 6,
            cellWidth = Math.floor((width - cellSpacing) / (cellSize + cellSpacing)),
            cellHeight = Math.floor((height - cellSpacing) / (cellSize + cellSpacing)),
            cells,
            frontier,
            active = 0;

        var id = ++active;

        context.fillStyle = "black";
        context.clearRect(0, 0, width, height);
        context.fillRect(0,0,width,height);
        context.fillStyle = "white";

        cells = new Array(cellWidth * cellHeight); // each cell’s edge bits
        frontier = minHeap(function(a, b) { return a.weight - b.weight; });

        // Add a random cell and two initial edges.
        var start = (cellHeight - 1) * cellWidth;
        cells[start] = 0;
        fillCell(start);
        frontier.push({index: start, direction: N, weight: Math.random()});
        frontier.push({index: start, direction: E, weight: Math.random()});

        // Explore the frontier until the tree spans the graph.
        d3.timer(function() {
            if (id !== active) return true;
            var done, k = 0;
            while (++k < 2 && !(done = exploreFrontier()));
            return done;
        });

        function exploreFrontier() {
            //nothing in the heap-> done.
            if ((edge = frontier.pop()) == null) return true;

            //hoisting.
            var edge,
                i0 = edge.index,
                d0 = edge.direction,
                i1 = i0 + (d0 === N ? -cellWidth : d0 === S ? cellWidth : d0 === W ? -1 : +1),
                x0 = i0 % cellWidth,
                y0 = i0 / cellWidth | 0,
                x1,
                y1,
                d1,
                open = cells[i1] == null; // opposite not yet part of the maze

            context.fillStyle = open ? "white" : "black";
            if (d0 === N) fillSouth(i1), x1 = x0, y1 = y0 - 1, d1 = S;
            else if (d0 === S) fillSouth(i0), x1 = x0, y1 = y0 + 1, d1 = N;
            else if (d0 === W) fillEast(i1), x1 = x0 - 1, y1 = y0, d1 = E;
            else fillEast(i0), x1 = x0 + 1, y1 = y0, d1 = W;

            if (open) {
                fillCell(i1);
                cells[i0] |= d0, cells[i1] |= d1;
                context.fillStyle = "red";
                if (y1 > 0 && cells[i1 - cellWidth] == null) fillSouth(i1 - cellWidth), frontier.push({index: i1, direction: N, weight: Math.random()});
                if (y1 < cellHeight - 1 && cells[i1 + cellWidth] == null) fillSouth(i1), frontier.push({index: i1, direction: S, weight: Math.random()});
                if (x1 > 0 && cells[i1 - 1] == null) fillEast(i1 - 1), frontier.push({index: i1, direction: W, weight: Math.random()});
                if (x1 < cellWidth - 1 && cells[i1 + 1] == null) fillEast(i1), frontier.push({index: i1, direction: E, weight: Math.random()});
            }
        }

        function fillCell(index) {
            var i = index % cellWidth, j = index / cellWidth | 0;
            context.fillRect(i * cellSize + (i + 1) * cellSpacing, j * cellSize + (j + 1) * cellSpacing, cellSize, cellSize);
        }

        function fillEast(index) {
            var i = index % cellWidth, j = index / cellWidth | 0;
            context.fillRect((i + 1) * (cellSize + cellSpacing), j * cellSize + (j + 1) * cellSpacing, cellSpacing, cellSize);
        }

        function fillSouth(index) {
            var i = index % cellWidth, j = index / cellWidth | 0;
            context.fillRect(i * cellSize + (i + 1) * cellSpacing, (j + 1) * (cellSize + cellSpacing), cellSize, cellSpacing);
        }

        function minHeap(compare) {
            var heap = {},
                array = [],
                size = 0;

            heap.empty = function() {
                return !size;
            };

            heap.push = function(value) {
                up(array[size] = value, size++);
                return size;
            };

            heap.pop = function() {
                if (size <= 0) return;
                var removed = array[0], value;
                if (--size > 0) value = array[size], down(array[0] = value, 0);
                return removed;
            };
            
            function up(value, i) {
                while (i > 0) {
                    var j = ((i + 1) >> 1) - 1,
                        parent = array[j];
                    if (compare(value, parent) >= 0) break;
                    array[i] = parent;
                    array[i = j] = value;
                }
            }

            function down(value, i) {
                while (true) {
                    var r = (i + 1) << 1,
                        l = r - 1,
                        j = i,
                        child = array[j];
                    if (l < size && compare(array[l], child) < 0) child = array[j = l];
                    if (r < size && compare(array[r], child) < 0) child = array[j = r];
                    if (j === i) break;
                    array[i] = child;
                    array[i = j] = value;
                }
            }

            return heap;
        };
    };   
    return Bostock;
    
});
