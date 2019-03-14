/*
 * ******************************************************************************
 *                                     opensilexdemo.js
 *  js
 *  Copyright © INRA 2019
 *  Creation date:  11 March, 2019
 *  Contact: arnaud.charleroy@inra.fr
 * ******************************************************************************
 */

/**
 * return object the following object from url 
    {
    wsUrl : "http://www.opensilex.org:8080/openSilexAPI/rest/",
    accessToken : "16193fdee6ead394adf63466b49241fc"
    }
 */
function initOpenSilexConnection() {
  var params = new window.URLSearchParams(window.location.search);
  var config = {};
  config.wsUrl = params.get("wsUrl");
  if ($("#token").length != 0) {
    config.token = $("#token").val();
  } else {
    config.token = params.get("accessToken");
  }

  return config;
}

$(function() {
  $('a[data-toggle="tab"]').on("shown.bs.tab", function(e) {
    $($.fn.dataTable.tables(true))
      .DataTable()
      .columns.adjust()
      .responsive.recalc();
  });
});

/**
 * Link a R plotly graph to a div
 * @param {string} iframeInput div input
 * @param {string} functionName R function name
 * @param {object} plotVarParameters function parameters
 */
function showPlot(iframeInput, functionName, plotVarParameters) {
  // Run the R function
  return (req = ocpu
    .call(functionName, plotVarParameters, function(session) {
      $("#" + iframeInput).attr(
        "src",
        session.getFileURL(functionName + "Widget.html")
      );
    })
    .fail(function(text) {
      alert("Error: " + req.responseText);
    }).always(function() {
      $("#submit").removeAttr("disabled");
    }))
}

function setDateInput(inputId, parameters= {dateFormat :"yy-mm-dd"}){
    $("#" + inputId).datepicker(parameters);
}

/**
 *
 * @param {string} inputId div input id
 * @param {object} variables
 *   {
 *   value : "http://www.opensilex.org:8080/openSilexAPI/rest/",
 *   name : "16193fdee6ead394adf63466b49241fc"
 *   }
 * @param {object} selectParameters custom select2 parameters
 */
function fillListInput(inputId, variables, selectParameters = {}) {
  variables.forEach(function(rVariables) {
    variable = {};
    variable.id = rVariables.value;
    variable.text = rVariables.name;
    inputData.push(variable);
  });
  // console.log(inputData);
  defaultSelectParameters = {
    data: inputData
  };
  finalSelectParameters = { ...defaultSelectParameters, ...selectParameters };
  console.log(finalSelectParameters);
  $("#" + inputId).select2(finalSelectParameters);
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

/**
   * 
   * @param {string} inputId variable div id
   * @param {object} config    
     {
      wsUrl : "http://www.opensilex.org:8080/openSilexAPI/rest/",
      accessToken : "16193fdee6ead394adf63466b49241fc"
      }
   */
function setGlobalVariablesAndInput(inputId, config, selectParameters = {},variablesList) {
  functionListParameters = { token: config.token };

  if (config.wsUrl !== null) {
    functionListParameters["wsUrl"] = config.wsUrl;
  }
  inputData = [];
  // Fill variables
  return ocpu.rpc(
    //Create array of variables' options
    "listVariables",
    functionListParameters,

    function(inputList) {
      variablesList = inputList;
      fillListInput(inputId, inputList, selectParameters);
      return inputList;
    }
  );
}

/**
 * return the name of a particular variable
 * @param {string} uri uri of the variable
 */
function getNameOfVariableUri(uri) {
  var name = null;
  variablesListOutput.forEach(function(variableObject) {
    if (variableObject.value == uri) {
      name = variableObject.name;
    }
  });
  return name;
}
/**
 * Create a datatable from a R list
 * @param {string} inputId id of the div
 * @param {object} getDFParameters function parameters
 */
function makeDatatable(inputId, getDFParameters) {
  return ocpu
    .rpc("getDF", getDFParameters, function(dataframe) {
      makeDatatableWithList(getDFParameters, dataframe);
    })
    .fail(function() {
      $(tablesDiv).html("");
      alert("Error: " + req.responseText);
    }).always(function() {
      $("#submit").removeAttr("disabled");
    });
}
/**
 *
 * @param {object} getDFParameters function parameters
 * @param {array} dataframe list of object given by the R function
 */
function makeDatatableWithList(getDFParameters, dataframe) {
  // div required
  var tablesDivId = "tables";
  var tabNavId = "navtabs";
  var active = true;

  // unset all content
  $("#" + tablesDivId).html("");
  $("#" + tabNavId).html("");

  // create the JSON array for the columns required by DataTable
  var varURIs = getDFParameters.varURI;
  varCount = 0;
  //create a tab by variables
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
    // create datatable columns for one variable
    var colnames = Object.keys(dataframe[varUri][0]);
    var columns = [];
    var tableId = "table" + varCount;
    colnames.forEach(function(columnName) {
      columns.push({ title: columnName });
    });
    // get data for one variable
    data = [];
    dataframe[varUri].forEach(function(dataVal) {
      temp_array = [];
      colnames.forEach(function(col) {
        temp_array.push(dataVal[col]);
      });
      data.push(temp_array);
    });
    // create tab header for this variable
    tabId = "var" + varCount;
    var classTabPanel = 'class="tab-pane"';
    if (active) {
      classTabPanel = 'class="tab-pane active"';
      active = false;
    }
    // create tab panel for this variable
    var tab =
      '<div role="tabpanel" ' + classTabPanel + ' id="' + tabId + '"></div>';

    $("#" + tablesDivId).append(tab);
    // create datatable in tab panel
    var table =
      '<table id="' +
      tableId +
      '" class="display generateTB" width="100%"></table>';
    $("#" + tabId).append(table);
    $("#" + tableId).append(makeHeaders(colnames));
    $("#" + tableId).dataTable({
      dom: "Bfrtip",
      buttons: [
        {
          extend: "copyHtml5",
          messageTop: varName + "Data"
        },
        {
          extend: "excelHtml5",
          messageTop: varName + "Data"
        },
        {
          extend: "csvHtml5",
          messageTop: varName + "Data"
        },
        {
          extend: "pdfHtml5",
          messageTop: varName + "Data"
        }
      ],
      data: data,
      columns: columns,
      responsive: true
    });
    // next variable
    varCount++;
  });
}
