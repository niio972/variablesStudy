/*
 * ******************************************************************************
 *                                     app.js
 *  js
 *  Copyright © INRA 2019
 *  Creation date:  06 March, 2019
 *  Contact: arnaud.charleroy@inra.fr
 * ******************************************************************************
 */
variablesList = [];

$(function() {
  // Comment for production case
  // ocpu.seturl("http://localhost:8004/ocpu/apps/niio972/variablesStudy/R");

  // initialize parameters
  var config = initOpenSilexConnection();
  variablesOutput = [];
  if (config.token == null) {
    alert("An accessToken is required");
    $("input").prop("disabled", true);
  } else {
    // set form inputs
    $("#startDate").datepicker({ dateFormat: "yy-mm-dd" });
    $("#endDate").datepicker({ dateFormat: "yy-mm-dd" });
    // variables' initialization
    // if fail disabled input
    setGlobalVariablesAndInput("variable", config, {
      maximumSelectionLength: 2,
      multiple: true
    }).fail(function(e) {
      $("#variable").prop("disabled", true);
      $("input").prop("disabled", true);
    });
    $("#cssLoader").removeClass("is-active");

    // show graph button
    $("#submit").click(function(e) {
      e.preventDefault();

      // parameters of the R function
      smoothing = document.getElementById("smoothing").checked;
      var varURIs = $("#variable").val();
      var startDate = $("#startDate").val();
      var endDate = $("#endDate").val();
      if (varURIs.length == 0) {
        alert("you must choose at least one variable");
        return false;
      }
      functionsParameters = {
        varURI: varURIs,
        token: config.token,
        smoothing: smoothing
      };
      var btn = $(this).attr("disabled", "disabled");
      if (config.wsUrl !== null) {
        functionsParameters["wsUrl"] = config.wsUrl;
      }
      if (startDate !== "") {
        functionsParameters["startDate"] = startDate;
      }
      if (endDate !== "") {
        functionsParameters["endDate"] = endDate;
      }
      // create a plot
      showPlot("plotDiv", "plotVar", functionsParameters).always(function() {
        btn.removeAttr("disabled");
      });
      // create multiple dataTables from list
      makeDatatable(
        "getDFDatatable",
        functionsParameters,
        variablesList
      ).always(function() {
        btn.removeAttr("disabled");
      });
    });
  }
});
