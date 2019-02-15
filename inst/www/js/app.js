
// 1. Plot creation
var arrayText;
var array;
$(function(){
  // Remove this line in the final version
  ocpu.seturl("http://localhost:8004/ocpu/library/webapp/R")
   // Initialisation des variables
  console.log("Bonjour en JavaScript !");
  var nbVar = -1;
  var idSelect = "mySelect";
  //Create array of options to be added

  var req3 = ocpu.rpc("listVariables",{
    token : $("#token").val()
  },function(output){
    arrayText = output.name;
    array = output.value;
    console.log(array);
var selectVariable = document.getElementById("variable");
    for (var i = 0; i < array.length; i++) {
        var option = document.createElement("option");
        option.value = array[i];
        option.text = arrayText[i];
        selectVariable.appendChild(option);
      }
  });
  console.log("nbVar = ", nbVar);
  $("#submit").click(function(e){
    e.preventDefault();
    var btn = $(this).attr("disabled", "disabled");
    console.log("variable " +  $("#variable").val()[0]);
    console.log("token " + $("#token").val()[0]);
    // Use of the R function to create the plot
    smoothing = document.getElementById('smoothing').checked;
    console.log("smoothing = ", smoothing);
    var nameVars = [$("#variable").val()];
    console.log("nameVars = ", nameVars);

    for (var i=0;i < (nbVar+1); i++){
      var idAddArray = idSelect.concat(i.toString());
      var newElement = document.getElementById(idAddArray).value;
      console.log("ESSAIS NEW ELEMENT", newElement);
      nameVars.push(newElement);
      console.log("nameVars = ", nameVars);
    }

    var req = ocpu.call("plotVar", {
      nameVar : nameVars,
      token: $("#token").val(),
      smoothing: smoothing
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

    var req = ocpu.rpc(
          "getDF",
          {
            nameVar: nameVars,
            token: $("#token").val(),
            smoothing: smoothing
          },
          function(df) {
            console.log(df);
            // get the column names
            var colnames = Object.keys(df[0]);
            // create the JSON array for the columns required by DataTable
            var columns = [];
            for (i = 0; i < colnames.length; i++) {
              var obj = {};
              obj['data'] = colnames[i]
              columns.push(obj);
            }
            
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
    // On n'autorise de n'ajouter qu'un select pour l'instant
        console.log(array);
    if (nbVar <0){
      nbVar  = nbVar + 1;
      console.log("nbVar = ", nbVar);


      // Ajout d'un select dans la <div> formulaire
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
          //Remplir select variable

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

  function makeHeaders(colnames) {
    var str = "<thead><tr>";
    for (var i = 0; i < colnames.length; i++) {
      str += "<th>" + colnames[i] + "</th>";
    }
    str +="</tr></thead>"
    return (str);
  }

});
