define(['underscore','d3'],function(_,d3){

    var DrawRandomWalk = function(canvas,height,width,id){
        console.log("Random walking");
        var c = canvas,
            midX = width * 0.5,
            TwoPi = 2 * Math.PI,
            points = [0, height],
            firstPass = subDiv(points,10),
            secondPass = [];
        console.log("first:",firstPass);
        while(firstPass.length > 0){
            var start = Math.floor(Math.random() * firstPass.length-1),
                end =  Math.floor(Math.random() * (firstPass.length - start)),
                section = subDiv(firstPass.splice(start,end),Math.floor(Math.random() * 15) + 5);
            secondPass = secondPass.concat(section);
        };
        console.log("second:",secondPass);
        c.beginPath();
        c.moveTo(midX,0);
        secondPass.forEach(function(d){
            c.lineTo(midX + ((Math.random() * 200) - 100),d);
        });
        c.stroke();
    
    };

    function subDiv(array,amt){
        while(array.length < amt){
            var index = Math.floor(Math.random() * (array.length-1)),
                next = index + 1,
                v1 = array[index],
                v2 = array[next],
                mid = (v1 + v2) / 2;
            console.log("Points:",v1,mid,v2);
            array.splice(next,0,mid);
        }
        return array;

    }
    
    return DrawRandomWalk;
});
