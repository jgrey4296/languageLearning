define(['d3','lodash'],function(d3,_){
    "use strict";
    
    let BarChart = function(container,width,height){
        this.container = container;
        this.data = [];
        this.height = height;
        this.width = width;
        this.xScale = d3.scale.ordinal()
            .rangeRoundBands([0,width],.1);
        this.yScale = d3.scale.linear().range([height,0]);
    };

    BarChart.prototype.update = function(data){
        this.xScale.domain(data.map(d=>d[0]));
        this.yScale.domain([0,d3.max(data.map(d=>d[1].length))]);
        
        let groups = this.container.selectAll('g').data(data.map(d=>[d[0],d[1].length])),
            enterGroups = groups.enter().append('g').append('rect');
                    
        groups.attr('transform',(d,i)=>`translate(${this.xScale(d[0])},${this.yScale(d[1])})`);
        groups.selectAll('rect').attr('height',(d,i)=>this.height - this.yScale(d[1])).attr('width',this.xScale.rangeBand())
            .on('mouseover',d=>console.log(d));

        groups.exit().remove();

    };

    return BarChart;
});
