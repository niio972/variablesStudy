/*
 * ******************************************************************************
 *                                     plotVar.js
 *  js
 *  Copyright © INRA 2019
 *  Creation date:  06 March, 2019
 *  Contact: arnaud.charleroy@inra.fr
 * ******************************************************************************
 */
config = {};

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
    showPlot("plotDiv", "plotVar", functionsParameters);

    // create multiple dataTables from list
    // advanced
    makeDatatable("getDFDatatable", functionsParameters);
  });
});

function getInputs() {
  // input parameters in the form of the R function
  var smoothing = $("smoothing").prop("checked");
  var varURIs = $("#variable").val();
  var startDate = $("#startDate").val();
  var endDate = $("#endDate").val();

  if (varURIs == null || varURIs.length == 0) {
    alert("you must choose at least one variable");
    return false;
  }
  functionsParameters = {
    varURI: varURIs,
    token: config.token,
    smoothing: smoothing
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
    // if fail disabled input
    setListInput("variable", config, {
      maximumSelectionLength: 2,
      multiple: true
    });
  }
}
