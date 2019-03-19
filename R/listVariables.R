#-------------------------------------------------------------------------------
# Program: getDF.R
# Objective: retrieves the list of variables for selection user interface
# Authors: Chourrout Elise
# Creation: 15/02/2019
# Update:
#-------------------------------------------------------------------------------

##' @title listVariables
##'
##' @param token a token from \code{\link{getToken}}
##' @param wsUrl url of the webservice
##'
##' @return list
##' @export
##'
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##'  aToken <- getToken("guest@opensilex.org","guest")
##'  token <- aToken$data
##'  listVariables(token = token)
##' }
listVariables <-function(token, wsUrl="www.opensilex.org/openSilexAPI/rest/"){

  phisWSClientR::initializeClientConnection(apiID="ws_private", url = wsUrl)
  varPretty <- getVarPretty(token = token)

  # creation of the dataTable with name and methods of the variables
  listVar <- NULL
  for (i in 1:8){
    listVar$name[i] <- varPretty$label[i]
    listVar$value[i] <- varPretty$uri[i]

  }
  listVar <- data.frame(lapply(listVar, as.character), stringsAsFactors=FALSE)
  return(listVar)
}
