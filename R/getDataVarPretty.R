#-------------------------------------------------------------------------------
# Program: getDF.R
# Objective: transform data in suitable form for graph
# Authors: Chourrout Elise
# Creation: 15/02/2019
# Update:
#-------------------------------------------------------------------------------

##' @title Get Data from WS2 and formate them
##'
##' @importFrom phisWSClientR initializeClientConnection
##' @importFrom phisWSClientR getVariables2
##'
##' @param nameVar name of the variable to plot
##' @param methodVar name of the method used to collect data
##' @param varPretty from \code{\link{getVarPretty}}
##' @param token a token from \code{\link{getToken}} function
##'
##' @return WSResponse
##' @export
##'
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##'  aToken <- getToken("guest@opensilex.org","guest")
##'  token <- aToken$data
##'  varPrettyTot <- getVarPretty(token = token)
##'  getDataVarPretty(nameVar = "temperature", varPretty = varPrettyTot, token = token)
##' }
getDataVarPretty <- function(nameVar, methodVar = NULL, varPretty, token) {

  phisWSClientR::initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")

  # Recuperation of the uri of the variable of interest
  if(!is.null(methodVar) && !is.na(methodVar)){
    numVar <- 1
    while(grepl(methodVar, varPretty$method[numVar]) == FALSE && numVar < dim(varPretty)[1]){
      numVar <- numVar+1
    }
  } else {
    numVar <- match(nameVar, varPretty$name)
  }
  nameUriVar <-  varPretty$uri[numVar]

  # Recuperation of the data from the WS
  myCount <- phisWSClientR::getEnvironmentData(token = token, variable = nameUriVar)$totalCount
  enviroData <- phisWSClientR::getEnvironmentData(token=token, variable =  nameUriVar, verbose = TRUE, pageSize = myCount)$data

  # Creation of the dataTable to return
  nomVar <- paste(toupper(substr(varPretty$name[numVar],1,1)), substr(varPretty$name[numVar],2,nchar(varPretty$name[numVar])), sep = "")
  methodVar <- as.character(varPretty$method[numVar])
  acronymVar <- as.character(varPretty$acronym[numVar])
  unityVar <- as.character(varPretty$unity[numVar])
  varPretty <- list(name = nomVar, method = methodVar, acronym = acronymVar, unity = unityVar)

  return(list(enviroData = enviroData, varPretty = varPretty))
}
