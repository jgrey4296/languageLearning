define(['underscore','d3'],function(_,d3){

    var FaceTest2 = function(ctx,height,width,idName){
        var cenX = width*0.5,
            cenY = height*0.5,
            faceDesc = FaceDescription(width*0.3);

        ctx.translate(cenX,cenY);
        ctx.save();
        DrawFace(ctx,faceDesc);
        ctx.restore();
    };

    var FaceDescription = function(radius){
        return {
            //Size of the face as a whole
            radius : radius,
            //Description of individual eyes
            eyes : {
                //left eye
                left : {
                    //negative location to move left and up from centre
                    x:-radius*0.3,
                    y:-radius*0.3 
                },
                //right eye
                right : {
                    //positive x to move right, negative y to move up
                    x: radius*0.3,
                    y: -radius*0.3
                },
            },
            //mouth left and right, down from centre
            mouth : {
                leftX: -radius*0.5,
                rightX : radius*0.5,
                y : radius * 0.5,
                cpX : 0,
                cpY : 80
            },
            //node height and width
            nose : {
                height : radius*0.3,
                width : radius*0.1
            }
        }
    };

    
    var DrawFace = function(ctx,faceDescription){
        ctx.beginPath();
        ctx.arc(0,0,faceDescription.radius,0,2*Math.PI);
        ctx.stroke();
        drawEye(ctx,faceDescription.eyes.left);
        drawEye(ctx,faceDescription.eyes.right);
        drawMouth(ctx,faceDescription.mouth);
        drawNose(ctx,faceDescription.nose);
    };

    var drawNose = function(ctx,noseDescription){
        var lx = -noseDescription.width,
            rx = noseDescription.width,
            startY = -noseDescription.height,
            endY = noseDescription.height,
            cpTopX = lx,
            cpTopY = 0,
            cpBottomX = 3 * (lx - noseDescription.width),
            cpBottomY = endY;
        
        ctx.save();
        drawNoseLine(ctx, lx,startY,
                       lx,endY,
                       cpTopX,cpTopY,
                     cpBottomX,cpBottomY);
        ctx.scale(-1,1);
        drawNoseLine(ctx,lx,startY,
                     lx,endY,
                     cpTopX,cpTopY,
                     cpBottomX,cpBottomY);
        
        ctx.restore();
    };

    var drawNoseLine = function(ctx,startX,startY,endX,endY,cp1X,cp1Y,cp2X,cp2Y,drawControlPoints){
        if(drawControlPoints !== undefined){
        ctx.fillRect(startX,startY,3,3);
        ctx.fillRect(endX,endY,3,3);
        ctx.fillRect(cp1X,cp1Y,3,3);
            ctx.fillRect(cp2X,cp2Y,3,3);
        }
        //draw the line
        ctx.beginPath();
        ctx.moveTo(startX,startY);
        ctx.bezierCurveTo(cp1X,cp1Y,cp2X,cp2Y,endX,endY);
        ctx.stroke();
    };

    
    var drawMouth = function(ctx,mouthDescription){
        ctx.save();
        ctx.translate(0,mouthDescription.y);
        ctx.beginPath();
        ctx.moveTo(mouthDescription.leftX,0);
        ctx.quadraticCurveTo(mouthDescription.cpX,mouthDescription.cpY,mouthDescription.rightX,0);
        //ctx.lineTo(mouthDescription.rightX,0);
        ctx.stroke();

        ctx.restore();
    };

    var drawEye = function(ctx,eyeDesc){
        console.log("Drawing:",eyeDesc);
        ctx.save();
        ctx.translate(0,0);
        ctx.beginPath();
        ctx.arc(eyeDesc.x,eyeDesc.y,Math.abs(eyeDesc.x/2),0,2*Math.PI);
        ctx.stroke();
        ctx.beginPath();
        ctx.arc(eyeDesc.x,eyeDesc.y,Math.abs(eyeDesc.x/5),0,2*Math.PI);
        ctx.stroke();
        ctx.restore();
    };


    return FaceTest2;
});
