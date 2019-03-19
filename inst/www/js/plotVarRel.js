variablesList = [];

$(function() {
  // Comment for production case
  // ocpu.seturl("http://138.102.159.37:8004/ocpu/apps/niio972/variablesStudy/R");

  // initialize parameters when token is valid
  initInputs();

  // show graph button
  $("#submit").click(function(e) {
    e.preventDefault();
    functionsParameters = getInputs();
    // basical
    // create a plot from htmlwidget named function name .e.g plotVar with Widget.html
    showPlot("plotDiv", "plotVarRel", functionsParameters);
  });
});

function getInputs() {
  // input parameters in the form of the R function
   var trend = $("#trend").prop("checked");
   var varX = [$("#variableX").val()];
   var varY = [$("#variableY").val()];
   var startDate = $("#startDate").val();
   var endDate = $("#endDate").val();

   if (varY.length == 0) {
     alert("you must choose at least one variable Y");
     return false;
   }
   if (varX.length == 0) {
     alert("you must choose at least one variable X");
     return false;
   }
   functionsParameters = {
     token: config.token,
     varX: varX,
     varY: varY,
     trend: trend
   };
   if (config.wsUrl !== null) {
     functionsParameters["wsUrl"] = config.wsUrl;
   }
   if (startDate !== "") {
     functionsParameters["startDate"] = startDate;
   }
   if (endDate !== "") {
     functionsParameters["endDate"] = endDate;
   }
   
  return functionsParameters;
}

function initInputs() {
  config = initOpenSilexConnection();
  // test token send in url
  if (config.token == null || config.token == "") {
    alert("An accessToken is required");
  } else {
    // set form inputs
    setDateInput("startDate", { dateFormat: "yy-mm-dd" });
    setDateInput("endDate", { dateFormat: "yy-mm-dd" });
    // variables' initialization
    setListInputFromRList("variableX","variableList", config, {
      multiple: false
    })
    setListInputFromRList("variableY","variableList", config, {
      multiple: false
    }) 
  }
}

