
// 1. Plot creation
$(function(){
  // Remove this line in the final version
  // ocpu.seturl("http://localhost/ocpu/library/webapp/R")
   // Initialisation des variables
  console.log("Bonjour en JavaScript !");
  var nbVar = -1;
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

    for (var i=0;i < (nbVar+1); i++){
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

    // Add a variable on the form
  $("#addVar").click(function(e){
    e.preventDefault();
    var btn = $(this).attr("disabled", "disabled");
    // On n'autorise de n'ajouter qu'un select pour l'instant
    if (nbVar <0){
      nbVar  = nbVar + 1;
      console.log("nbVar = ", nbVar);


      // Ajout d'un select dans la <div> formulaire
      var myDiv = document.getElementById("formulaire");

      //Create array of options to be added
      var arrayText = ["Wind","Temperature (instant)","Temperature (actinometric)", "Radiation (global)", "Radiation (global PAR)", "Precipitation (hourly)"];
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

  
});
