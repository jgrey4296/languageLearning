/* jshint esversion : 6 */
define(['d3','lodash'],function(d3,_){
    "use strict";
    let LineGraph = function(container,width,height,maxY,lineColour){
        this.container = container;
        this.data = [];
        this.xScale = d3.scale.linear().range([0,width]);
        this.yScale = d3.scale.pow().range([height,0])
            .domain([0,maxY]);
        this.line = d3.svg.line()
            .x(d=>this.xScale(d[0]))
            .y(d=>this.yScale(d[1]));
        this.lineColour = lineColour;
    };

    LineGraph.prototype.update = function(data){
        //update the scale
        this.xScale.domain(d3.extent(data,d=>d[0]));
        this.container.selectAll('path').remove();
        this.container.append('path')
            .datum(data)
            .style('stroke',this.lineColour)
            .style('fill','none')
            .style('stroke-width','1.5px')
            .attr('d',this.line);
    };


    return LineGraph;
});
