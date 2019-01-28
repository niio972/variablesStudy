
// 1. Plot creation
$(function(){
  // Remove this line in the final version
  ocpu.seturl("http://localhost/ocpu/library/webapp/R")
  $("#submit").click(function(e){
    e.preventDefault();
    var btn = $(this).attr("disabled", "disabled");

    console.log("variable" +  $("#variable").val()[0]);
    console.log("token" + $("#token").val()[0]);
    // Use of the R function to create the plot
    var req = ocpu.call("plotVar", {
      nom_var : $("#variable").val(),
      token: $("#token").val()
    }, function(session){
       $("iframe").attr('src', session.getFileURL("test1var.html"));
    }).fail(function(text){
      alert("Error: " + req.responseText);
    }).always(function(){
      btn.removeAttr("disabled");
    });
  });
});
