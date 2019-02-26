// 1. Plot creation
var arrayText;
var array;
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
    listVariableParameters = { token: token };
    if (wsUrl !== null) {
      listVariableParameters["wsUrl"] = wsUrl;
    }
    var req3 = ocpu.rpc(
      "listVariables",
      listVariableParameters,
      function(output) {
        arrayText = output.name;
        array = output.value;
        console.log(array);
        var selectVariableY = document.getElementById("variableX");
        var selectVariableX = document.getElementById("variableY");
        for (var i = 0; i < array.length; i++) {
          var optionX = document.createElement("option");
          optionX.value = array[i];
          optionX.text = arrayText[i];
          selectVariableX.appendChild(optionX);
          var optionY = document.createElement("option");
          optionY.value = array[i];
          optionY.text = arrayText[i];
          selectVariableY.appendChild(optionY);
        }
      }
    );
    console.log("nbVar = ", nbVar);
    $("#submit").click(function(e) {
      e.preventDefault();
      var btn = $(this).attr("disabled", "disabled");
  
      // Use of the R function to create the plot
      smoothing = document.getElementById("smoothing").checked;
      console.log("smoothing = ", smoothing);
      var varX = [$("#variableX").val()];
      var varY = [$("#variableY").val()];
      console.log("varX = ", varX);
      console.log("varY = ", varY);

      for (var i = 0; i < nbVar + 1; i++) {
        var idAddArray = idSelect.concat(i.toString());
        var newElement = document.getElementById(idAddArray).value;
        console.log("ESSAIS NEW ELEMENT", newElement);
        nameVars.push(newElement);
        console.log("nameVars = ", nameVars);
      }

      plotVarRelParameters = {
        token: token,
        varX: varX,
        varY: varY,
        // smoothing: smoothing
      };

      if (wsUrl !== null) {
        plotVarRelParameters["wsUrl"] = wsUrl;
      }
      var req = ocpu
        .call(
          "plotVarRel",
          plotVarRelParameters,
          function(session) {
            $("iframe").attr("src", session.getFileURL("relVarPlot.html"));
          }
        )
        .fail(function(text) {
          alert("Error: " + req.responseText);
        })
        .always(function() {
          console.log({
            nameVar: nameVars,
            token: token
          });

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
