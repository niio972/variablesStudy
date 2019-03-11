/*
 * ******************************************************************************
 *                                     opensilex.js
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
function initOpenSilexConnection(){
    var params = new window.URLSearchParams(window.location.search);
    var config = {};
    config.wsUrl = params.get("wsUrl");
    config.token = params.get("accessToken");
    return config;
}

/**
 * Link a R plotly graph to a div 
 * @param {string} iframeInput div input
 * @param {object} plotVarParameters function parameters 
 */
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

/**
 * 
 * @param {string} inputId div input id
 * @param {object} variables  
 *   {
 *   value : "http://www.opensilex.org:8080/openSilexAPI/rest/",
 *   name : "16193fdee6ead394adf63466b49241fc"
 *   }
 */
function fillListInput(inputId,variables){
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

  // DataTable's header
function makeHeaders(colnames) {
    var str = "<thead><tr>";
    for (var i = 0; i < colnames.length; i++) {
      str += "<th>" + colnames[i] + "</th>";
    }
    str += "</tr></thead>";
    return str;
  }