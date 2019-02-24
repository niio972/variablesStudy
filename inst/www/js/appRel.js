
// 1. Plot creation
var arrayText;
var array;
$(function(){
  // Remove this line in the final version
  ocpu.seturl("http://localhost:8004/ocpu/library/variablesStudy/R")
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
    var varX = [$("#variableX").val()];
    var varY = [$("#variableY").val()];
    console.log("varX = ", varX);
    console.log("varY = ", varY);

    for (var i=0;i < (nbVar+1); i++){
      var idAddArray = idSelect.concat(i.toString());
      var newElement = document.getElementById(idAddArray).value;
      console.log("ESSAIS NEW ELEMENT", newElement);
      nameVars.push(newElement);
      console.log("nameVars = ", nameVars);
    }

    var req = ocpu.call("plotVarRel", {
        varX : varX,
        varY : varY,
      token: $("#token").val(),
      smoothing: smoothing
    }, function(session){
       $("iframe").attr('src', session.getFileURL("relVarPlot.html"));
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