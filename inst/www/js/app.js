
// 1. Plot creation
var arrayText;
var array;
$(function(){
  // Remove this line in the final version
  ocpu.seturl("http://localhost:8004/ocpu/library/webapp/R")

  // Variables' initialization
  console.log("Bonjour en JavaScript !");
  var nbVar = -1;
  var idSelect = "mySelect";

  //Form
  var req3 = ocpu.rpc("listVariables",{
    //Create array of variables' options
    token : $("#token").val()
  },function(output){
      arrayText = output.name;
      array = output.value;
      // Variable's selector
  var selectVariable = document.getElementById("variable");
    for (var i = 0; i < array.length; i++) {
        var option = document.createElement("option");
        option.value = array[i];
        option.text = arrayText[i];
        selectVariable.appendChild(option);
      }
  });

  //Show graph button
  $("#submit").click(function(e){
    e.preventDefault();
    var btn = $(this).attr("disabled", "disabled");

    // Parameters of the R function
    smoothing = document.getElementById('smoothing').checked;
    var nameVars = [$("#variable").val()];
    var startDate = $("#startDate").val();
    var endDate = $("#endDate").val();
    for (var i=0;i < (nbVar+1); i++){
      var idAddArray = idSelect.concat(i.toString());
      var newElement = document.getElementById(idAddArray).value;
      nameVars.push(newElement);
    }

    // Run the R function
    var req = ocpu.call("plotVar", {
      nameVar : nameVars,
      token: $("#token").val(),
      smoothing: smoothing,
      startDate: startDate,
      endDate: endDate
    }, function(session){
       $("iframe").attr('src', session.getFileURL("enviroVarPlot.html"));
    }).fail(function(text){
      alert("Error: " + req.responseText);
    }).always(function(){
      console.log({
        nameVar : nameVars,
        token: $("#token").val()
      });

      btn.removeAttr("disabled");
    }).fail(function(text){
      alert("Error: " + req.responseText);
    });

    // DataTable
    // Run the R function
    var req = ocpu.rpc(
          "getDF",
          {
            nameVar: nameVars,
            token: $("#token").val(),
            smoothing: smoothing,
            startDate: startDate,
            endDate: endDate
          },
          function(df) {
            // get the column names
            var colnames = Object.keys(df[0]);
            // create the JSON array for the columns required by DataTable
            var columns = [];
            for (i = 0; i < colnames.length; i++) {
              var obj = {};
              obj['data'] = colnames[i]
              columns.push(obj);
            } 

            // DataTable update
            if ($.fn.DataTable.isDataTable("#mytable")) {
              $('#mytable').DataTable().clear().destroy();
              $('#mytable thead tr').remove();
            }
            $('#mytable').append(makeHeaders(colnames));
            $("#mytable").dataTable({
                data: df,
                columns: columns
              });
          }
        );
  });

  // Add a variable on the form
  $("#addVar").click(function(e){
    e.preventDefault();
    var btn = $(this).attr("disabled", "disabled");
    if (nbVar <0){
      nbVar  = nbVar + 1;
      console.log("nbVar = ", nbVar);

      // Adda select in the form
      var myDiv = document.getElementById("var-form");

      //Create and append select list
      var selectList = document.createElement("select");
      selectList.id = idSelect.concat(nbVar.toString());
      console.log("Select id", nbVar, " = ",  idSelect.concat(nbVar.toString()));
      selectList.setAttribute("class", "form-control");
      myDiv.appendChild(selectList);

      //Create and append the options
      for (var i = 0; i < array.length; i++) {
          var option = document.createElement("option");
          option.value = array[i];
          option.text = arrayText[i];
          selectList.appendChild(option);
      }
    } else {
    }
    btn.removeAttr("disabled");
  });

  // Remove a variable on the form
  $("#removeVar").click(function(e){
    e.preventDefault();
    var btn = $(this).attr("disabled", "disabled");
    if (nbVar >= 0){
      console.log("nbVar = ", nbVar);

      // Remove the last select created
      var element = idSelect.concat(nbVar.toString());
      element = document.getElementById(element);
      element.parentNode.removeChild(element);

      nbVar  = nbVar - 1;
    } else {
    }
    btn.removeAttr("disabled");
  });

  // DataTable's header
  function makeHeaders(colnames) {
    var str = "<thead><tr>";
    for (var i = 0; i < colnames.length; i++) {
      str += "<th>" + colnames[i] + "</th>";
    }
    str +="</tr></thead>"
    return (str);
  }

});
