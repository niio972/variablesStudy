variablesList = [];

$(function() {
  // Comment for production case
  ocpu.seturl("http://localhost:8004/ocpu/apps/niio972/variablesStudy/R");

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
    setGlobalVariablesAndInput("variableX", config, {
      multiple: false
    })
    setGlobalVariablesAndInput("variableY", config, {
      multiple: false
    }) .fail(function(e) {
        $("#variable").prop("disabled", true);
        $("input").prop("disabled", true);
      })
    $("#cssLoader").removeClass("is-active");

    // show graph button
    $("#submit").click(function(e) {
      e.preventDefault();

      // parameters of the R function
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
      showPlot("plotDiv", "plotVarRel", functionsParameters).always(function() {
        btn.removeAttr("disabled");
      });
      // // create multiple dataTables from list
      // makeDatatable(
      //   "getDFDatatable",
      //   functionsParameters,
      //   variablesList
      // ).always(function() {
      //   btn.removeAttr("disabled");
      // });
    });
  }
});
