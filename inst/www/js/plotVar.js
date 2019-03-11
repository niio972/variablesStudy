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
    $("input").prop('disabled', true);
  } else {
    // set form inputs
    $("#startDate").datepicker({ dateFormat: "yy-mm-dd" });
    $("#endDate").datepicker({ dateFormat: "yy-mm-dd" });
    // variables' initialization 
    // if fail disabled input
    setGlobalVariablesAndInput("variable", config)
    .fail(function(e){
      $("#variable").prop("disabled",true)
      $("input").prop('disabled', true);
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
      plotVar("plotVarFrame", functionsParameters).always(function() {
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

  $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
    $($.fn.dataTable.tables(true)).DataTable()
       .columns.adjust()
       .responsive.recalc();
  }); 
});

/**
 * 
 * @param {string} inputId variable div id
 * @param {object} config    
   {
    wsUrl : "http://www.opensilex.org:8080/openSilexAPI/rest/",
    accessToken : "16193fdee6ead394adf63466b49241fc"
    }
 */
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
      fillListInput(inputId,variables)
      return variables;
    }
  )
}


/**
 * return the name of a particular variable
 * @param {string} uri uri of the variable
 */
function getNameOfVariableUri(uri) {
  var name = null;
  variablesList.forEach(function(variableObject) {
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
      '<table id="' + tableId + '" class="display generateTB" width="100%"></table>';
    $("#" + tabId).append(table);
    $("#" + tableId).append(makeHeaders(colnames));
    $("#" + tableId).dataTable({
      dom: 'Bfrtip',
        buttons: [
            'copyHtml5',
            'excelHtml5',
            'csvHtml5',
            'pdfHtml5'
        ],
      data: data,
      columns: columns,
      responsive: true
    });
    // next variable
    varCount++;
    });
}