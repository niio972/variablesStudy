
var plot = document.getElementById('plot').contentWindow,
     pointsToDelete = {},
     deleteCounter = 0;

Array.prototype.multisplice = function( args ){
    args.sort(function(a, b){
        return a - b;
    });
    for(var i = 0; i < args.length; i++){
        var index = args[i] - i;
        this.splice(index, 1);
    }
}

document.getElementById("plot").onload = function() {
    pinger = setInterval(function(){
        plot.postMessage({task: 'ping'}, 'https://plot.ly')
    }, 100);
};

function addCross( message ){
    var x = message['points'][0]['x'],
        y = message['points'][0]['y'];
    plot.postMessage({
        task: 'addTraces',
        traces: [{x:[x], y:[y],
                name: 'Delete '+x+', '+y,
                showlegend: false,
                mode: 'markers',
                marker: {size:12, color:"red", symbol:"cross"} }],
        newIndices: [-1]
    }, 'https://plot.ly');
    deleteCounter++;
}

var clickResponse = function(e) {
     plot = document.getElementById('plot').contentWindow;
    var message = e.data;
     console.log( 'New message from chart', message, pinger );
    if(message.pong) {
        clearAllIntervals();
        plot.postMessage({
              task: 'listen', events: ['click']}, 'https://plot.ly');
          plot.postMessage({
            task: 'relayout',
            'update': {hovermode: 'closest'},
        },
        'https://plot.ly');
    }
    else if(message.type === 'click') {
            var curveNumber = message['points'][0]['curveNumber'],
                 pointNumber = message['points'][0]['pointNumber'];

            if( curveNumber in pointsToDelete == false ){
                pointsToDelete[curveNumber] = [];
            }
            pointsToDelete[curveNumber].push( pointNumber );
            addCross( message );
    }
     else if( 'data' in message['response'] ){
         var allTraces = message['response']['data'];

         Object.keys(pointsToDelete).forEach(function(trace){
                allTraces[trace]['x'].multisplice( pointsToDelete[trace] );
                allTraces[trace]['y'].multisplice( pointsToDelete[trace] );
         });

         var X=[], Y=[];
         allTraces.forEach(function(trace){
             X.push( trace['x'] );
             Y.push( trace['y'] );
         });

         plot.postMessage( {
            task:'restyle',
            update:{x:X,y:Y}
       }, 'https://plot.ly');
         clearSelection();
     }
};

window.addEventListener("message", clickResponse, false);

function clearSelection(){
    for( i=0; i<deleteCounter; i++ ){
        plot.postMessage( {
            task: 'deleteTraces',
            indices: [-1],
        }, 'https://plot.ly');
    }
    pointsToDelete = [];
    deleteCounter=0;
}

function deletePoints(){
    plot.postMessage({
        task: 'getAttributes',
        attributes: [ 'data' ] },
        'https://plot.ly/');
}

function newPlot(){
    var plotURL = document.getElementById('plotURL').value + '.embed';
    var iframe = document.getElementById('plot');
    iframe.src = plotURL;
    pointsToDelete = {},
    deleteCounter = 0;
}

function clearAllIntervals(){
    var interval_id = window.setInterval("", 9999);
    for (var i = 1; i < interval_id; i++)
        window.clearInterval(i);
}
