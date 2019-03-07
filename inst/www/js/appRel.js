// 1. Plot creation
var arrayText;
var array;

function fillInputWithVariables(inputId, config) {
  listVariableParameters = { token: config.token };
  if (config.wsUrl !== null) {
    listVariableParameters["wsUrl"] = config.wsUrl;
  }
  inputData = [];
  // Fill variables
  var variables = ocpu.rpc(
    //Create array of variables' options
    "listVariables",
    listVariableParameters,
    function(variables) {
      variables.forEach(function(rVariables) {
        variable = {};
        variable.id = rVariables.value;
        variable.text = rVariables.name;
        inputData.push(variable);
      });
      // console.log(inputData);
      $("#" + inputId).select2({
        data: inputData,
      });
    }
  );
}

$(function() {
  // Remove this line in the final version
  // ocpu.seturl("http://localhost:8004/ocpu/library/variablesStudy/R")
  
  // Initialisation des variables
  console.log("Bonjour en JavaScript !");
  var nbVar = -1;
  var idSelect = "mySelect";

  // ocpu.seturl("http://localhost:8004/ocpu/apps/niio972/variablesStudy/R");
  var params = new window.URLSearchParams(window.location.search);
  var token = params.get("access_token");
  var wsUrl = params.get("wsUrl");
  //Create array of options to be added
  if (token == null) {
    alert("A token is needed");
  } else {
    var config = initOpenSilexConnection();


    fillInputWithVariables("variableX",config);
    fillInputWithVariables("variableY",config);
    
    $("#submit").click(function(e) {
      e.preventDefault();
      var btn = $(this).attr("disabled", "disabled");
  
      // Use of the R function to create the plot
      smoothing = document.getElementById("trend").checked;
      console.log("trend = ", smoothing);
      var varX = [$("#variableX").val()];
      var varY = [$("#variableY").val()];
      console.log("varX = ", varX);
      console.log("varY = ", varY);

      for (var i = 0; i < nbVar + 1; i++) {
        var idAddArray = idSelect.concat(i.toString());
        var newElement = document.getElementById(idAddArray).value;
        nameVars.push(newElement);
      }

      plotVarRelParameters = {
        token: token,
        varX: varX,
        varY: varY,
        // trend: trend
      };
      // if (startDate !== "") {
      //   plotVarRelParameters["startDate"] = startDate;
      // }
      // if (endDate !== "") {
      //   plotVarRelParameters["endDate"] = endDate;
      // }
      if (wsUrl !== null) {
        plotVarRelParameters["wsUrl"] = wsUrl;
      }
      var req = ocpu
        .call(
          "plotVarRel",
          plotVarRelParameters,
          function(session) {
            $("iframe").attr("src", session.getFileURL("plotVarRelWidget.html"));
          }
        )
        .fail(function(text) {
          alert("Error: " + req.responseText);
        })
        .always(function() {
          btn.removeAttr("disabled");
        })
        .fail(function(text) {
          alert("Error: " + req.responseText);
        });
    });

    function makeHeaders(colnames) {
      var str = "<thead><tr>";
      for (var i = 0; i < colnames.length; i++) {
        str += "<th>" + colnames[i] + "</th>";
      }
      str += "</tr></thead>";
      return str;
    }
  }
});
