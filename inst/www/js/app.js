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

function plotVar(iframeInput, plotVarParameters) {
  // Run the R function
  return (req = ocpu
    .call("plotVar", plotVarParameters, function(session) {
      $("#" + iframeInput).attr(
        "src",
        session.getFileURL("plotVarWidget.html")
      );
    })
    .fail(function(text) {
      alert("Error: " + req.responseText);
    }));
}

function setGlobalVariablesAndInput(inputId, config) {
  listVariableParameters = { token: config.token };
  if (config.wsUrl !== null) {
    listVariableParameters["wsUrl"] = config.wsUrl;
  }
  inputData = [];
  // Fill variables
  return ocpu.rpc(
    //Create array of variables' options
    "listVariables",
    listVariableParameters,
    function(variables) {
      variablesList = variables;
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
      return variables;
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

function getNameOfVariableUri(uri) {
  var name = null;
  variablesList.forEach(function(variableObject) {
    if (variableObject.value == uri) {
      name = variableObject.name;
    }
  });
  return name;
}

function makeDatatable(inputId, getDFParameters) {
  return ocpu
    .rpc("getDF", getDFParameters, function(df) {
      makeDatatableWithList(getDFParameters, df);
    })
    .fail(function() {
      $(tablesDiv).html("");
      alert("Error: " + req.responseText);
    });
}

function makeDatatableWithList(getDFParameters, df) {
  console.log(variablesList);
  // div tab
  var tablesDivId = "tables";
  var tabNavId = "navtabs";
  var active = true;

  $("#" + tablesDivId).html("");
  $("#" + tabNavId).html("");

  // create the JSON array for the columns required by DataTable
  var varURIs = getDFParameters.varURI;
  varCount = 0;
  varURIs.forEach(function(varUri) {
    varName = getNameOfVariableUri(varUri);
    var classTab = "";
    if (active) {
      classTab = 'class="active"';
    }
    $("#" + tabNavId).append(
      '<li role="presentation" ' +
        classTab +
        ' ><a href="#var' +
        varCount +
        '" aria-controls="home" role="tab" data-toggle="tab">' +
        varName +
        "</a></li>"
    );
    var colnames = Object.keys(df[varUri][0]);
    var columns = [];

    var tableId = "table" + varCount;
    colnames.forEach(function(columnName) {
      columns.push({ title: columnName });
    });
    data = [];
    df[varUri].forEach(function(dataVal) {
      temp_array = [];
      colnames.forEach(function(col) {
        temp_array.push(dataVal[col]);
      });
      data.push(temp_array);
    });
    tabId = "var" + varCount;
    var classTabPanel = 'class="tab-pane"';
    if (active) {
      classTabPanel = 'class="tab-pane active"';
      active = false;
    }

    var tab =
      '<div role="tabpanel" ' + classTabPanel + ' id="' + tabId + '"></div>';

    $("#" + tablesDivId).append(tab);
    var table =
      '<table id="' + tableId + '" class="display generateTB" width="100%"></table>';
    $("#" + tabId).append(table);
    $("#" + tableId).append(makeHeaders(colnames));
    $("#" + tableId).dataTable({
      data: data,
      columns: columns,
      responsive: true
    });

    varCount++;
    });
}

$(function() {
  // Remove this line in the final version
  ocpu.seturl("http://localhost:8004/ocpu/apps/niio972/variablesStudy/R");
  $("#startDate").datepicker({ dateFormat: "yy-mm-dd" });
  $("#endDate").datepicker({ dateFormat: "yy-mm-dd" });
  var config = initOpenSilexConnection();
  variablesOutput = [];
  if (config.token == null) {
    alert("A token is needed");
  } else {
    // Variables' initialization
    setGlobalVariablesAndInput("variable", config);

    //Show graph button
    $("#submit").click(function(e) {
      e.preventDefault();

      // Parameters of the R function
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
      plotVar("plotVarFrame", functionsParameters).always(function() {
        btn.removeAttr("disabled");
      });
      // DataTable
      makeDatatable(
        "getDFDatatable",
        functionsParameters,
        variablesList
      ).always(function() {
        btn.removeAttr("disabled");
      });
      // Run the R function
    });
  }

  $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
    $($.fn.dataTable.tables(true)).DataTable()
       .columns.adjust()
       .responsive.recalc();
}); 
});
