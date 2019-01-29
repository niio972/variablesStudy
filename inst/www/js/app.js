
// 1. Plot creation
$(function(){
  // Remove this line in the final version
  ocpu.seturl("http://localhost/ocpu/library/webapp/R")
   // Initialisation des variables
  console.log("Bonjour en JavaScript !");
  var nbVar = 0;
  var idSelect = "mySelect";

  console.log("nbVar = ", nbVar);
  $("#submit").click(function(e){
    e.preventDefault();
    var btn = $(this).attr("disabled", "disabled");
    console.log("variable" +  $("#variable").val()[0]);
    console.log("token" + $("#token").val()[0]);
    // Use of the R function to create the plot
    var nameVars = [$("#variable").val()];
    console.log("nameVars = ", nameVars);

    for (var i=0;i < nbVar; i++){
      var idAddArray = idSelect.concat(i.toString());
      var newElement = document.getElementById(idAddArray).value;
      console.log("ESSAIS NEW ELEMENT", newElement);
      nameVars.push(newElement);      
      console.log("nameVars = ", nameVars);
    }
    
    var req = ocpu.call("plotVar", {
      nameVar : nameVars,
      token: $("#token").val()
    }, function(session){
       $("iframe").attr('src', session.getFileURL("test1var.html"));
    }).fail(function(text){
      alert("Error: " + req.responseText);
    }).always(function(){
      btn.removeAttr("disabled");
    });
  });
  $("#addVar").click(function(e){
    e.preventDefault();
    console.log("nbVar = ", nbVar);
    var btn = $(this).attr("disabled", "disabled");
    console.log("click add var");
    var myDiv = document.getElementById("formulaire");
    //Create array of options to be added
    //var array = ["Wind","Temperature (instant)","Temperature (actinometric)", "Radiation (global)", "Radiation (global PAR)", "Precipitation (hourly)"];
    var array = ["wind", "temperature_instant","temperature_actinothermic", "radiation_global", "radiation_PAR", "precipitation_hourly rainfall"];

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
        option.text = array[i];
        selectList.appendChild(option);
    }
    nbVar  = nbVar + 1;

    btn.removeAttr("disabled");
  });
});
