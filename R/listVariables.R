#-------------------------------------------------------------------------------
# Program: getDF.R
# Objective: retrieves the list of variables for selection user interface
# Authors: Chourrout Elise
# Creation: 15/02/2019
# Update:
#-------------------------------------------------------------------------------

#' @title listVariables
#'
#' @param token a token from \code{\link{getToken}}
#'
#' @return list
#' @export
#'
#' @examples
#' \donttest{
#'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
#'  aToken <- getToken("guest@opensilex.org","guest")
#'  token <- aToken$data
#'  listVariables(token = token)
#' }
listVariables <-function(token){

  phisWSClientR::initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
  varPretty <- getVarPretty(token = token)

  # creation of the dataTable with name and methods of the variables
  listVar <- NULL
  for (i in 1:8){
    listVar$name[i] <-  paste(toupper(substr(varPretty$name[i],1,1)), substr(varPretty$name[i],2,nchar(varPretty$name[i])), " (", varPretty$method[i], ")", sep = "")
    listVar$value[i] <- paste(varPretty$name[i], "_", varPretty$method[i], sep = "")
  }
  return(listVar)
}
