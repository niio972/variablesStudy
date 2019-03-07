/*
 * ******************************************************************************
 *                                     app.js
 *  js
 *  Copyright © INRA 2019
 *  Creation date:  06 March, 2019
 *  Contact: arnaud.charleroy@inra.fr
 * ******************************************************************************
 */

function plotVar(iframeInput, plotVarParameters) {
  // Run the R function
  return req = ocpu
    .call("plotVar", plotVarParameters, function(session) {
      $("#" + iframeInput).attr("src", session.getFileURL("plotVarWidget.html"));
    })
    .fail(function(text) {
      alert("Error: " + req.responseText);
    })
}

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
        maximumSelectionLength: 2,
        multiple: true
      });
    }
  );
}

// DataTable's header
function makeHeaders(colnames) {
  var str = "<thead><tr>";
  for (var i = 0; i < colnames.length; i++) {
    str += "<th>" + colnames[i] + "</th>";
  }
  str += "</tr></thead>";
  return str;
}

function makeDatatable(inputId, getDFParameters) {
  var tableId = "#" + inputId;
  return ocpu.rpc("getDF", getDFParameters, function(df) {
    // get the column names
    var colnames = Object.keys(df[0][0]);
    // create the JSON array for the columns required by DataTable
    var columns = [];
    getDFParameters.varURI.forEach(function(columnName){
      colnames.forEach(function(columnName){
          var obj = {};
          obj["data"] = columnName;
          columns.push(obj);
      });
    });
    console.log(columns)
    data =[]
    df.forEach(function(dataVal){
      temp_array = [];
      getDFParameters.varURI.forEach(function(columnName){
        columns.forEach(function(col){
          temp_array.push[dataVal[col]];
        });
      });
      data.push(temp_array);
    });
    console.log(data)

    // DataTable update
    if ($.fn.DataTable.isDataTable(tableId)) {
      $(tableId)
        .DataTable()
        .clear()
        .destroy();
      $(tableId).html("");
    }
    $(tableId).append(makeHeaders(colnames));
    $(tableId).dataTable({
      data: data,
      columns: columns
    });
  }).fail(function(){
     // DataTable update
     if ($.fn.DataTable.isDataTable(tableId)) {
      $(tableId)
        .DataTable()
        .clear()
        .destroy();
      $(tableId).html("");
    }
    alert("Error: " + req.responseText);

  });
}

$(function() {
  // Remove this line in the final version
  ocpu.seturl("http://localhost:8004/ocpu/apps/niio972/variablesStudy/R");
  $( "#startDate" ).datepicker({"dateFormat": "yy-mm-dd"});
  $( "#endDate" ).datepicker({"dateFormat": "yy-mm-dd"});
  var config = initOpenSilexConnection();

  if (config.token == null) {
    alert("A token is needed");
  } else {
    // Variables' initialization
    fillInputWithVariables("variable", config);
    //Show graph button
    $("#submit").click(function(e) {
      e.preventDefault();

      // Parameters of the R function
      smoothing = document.getElementById("smoothing").checked;
      var nameVars = $("#variable").val();
      var startDate = $("#startDate").val();
      var endDate = $("#endDate").val();
      if($("#variable").val().length == 0){
        alert("you must choose at least one variable")
          return false;
      }
      functionsParameters = {
        varURI: nameVars,
        token: config.token,
        smoothing: smoothing,
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
      plotVar("plotVarFrame", functionsParameters)
      .always(function() {
        btn.removeAttr("disabled");
      });;
      // DataTable
      makeDatatable("getDFDatatable", functionsParameters)
      .always(function() {
        btn.removeAttr("disabled");
      });
      // Run the R function
    });
  }
});
