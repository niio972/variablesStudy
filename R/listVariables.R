#' @title listVariables
#'
#' @return list
#' @export
#'
#' @examples
#' \donttest{
#' listVariables()
#' }
listVariables <-function(){
  phisWSClientR::initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
  varPretty <- getVarPretty(token = "c5e123ff73245a77c4147a48aae5c2e2")
  listVar <- NULL
  for (i in 1:8){
    listVar$name[i] <-  paste(toupper(substr(varPretty$name[i],1,1)), substr(varPretty$name[i],2,nchar(varPretty$name[i])), " (", varPretty$method[i], ")", sep = "")
    listVar$value[i] <- paste(varPretty$name[i], "_", varPretty$method[i], sep = "")
  }
  return(listVar)
}
