#' @title getDF
#'
#' @param nameVar name of the variables to add in the data frame
#' @param token a token from getToken function
#' @param smoothing logical, smoothing of the data
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \donttest{
#' initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
#' aToken <- getToken("guest@opensilex.org","guest")
#' token <- aToken$data
#' getDF(token = token, nameVar = list("wind","temperature"))
#' }
getDF <- function(nameVar, token, smoothing = FALSE){
  print(nameVar)
  phisWSClientR::initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
  varPrettyTot <- getVarPretty(token = token)
  varPretty <- NULL
  # Chosen variable
  for (i in 1: length(nameVar)){
    nameString <- toString(nameVar[i])
    varMeth <- strsplit(nameString, split="_")
    methodVar <- varMeth[[1]][2]
    subNameVar <- varMeth[[1]][1]
    # Recuperation of the data from the WS
    enviroData <- getDataVarPretty(nameVar = subNameVar, methodVar = methodVar, varPretty = varPrettyTot, token = token)
    enviroData <- enviroData$enviroData
    # Values
    yVar <- enviroData$value
    if (i == 1){
      xVar <- as.POSIXct(enviroData$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
      df <- data.frame(Date = xVar)
    }
    df[,subNameVar] <-  yVar
    if(smoothing == TRUE){
      # Création du modèle
      print(i)
      varSpline <- gam::gam(df[,dim(df)[2]]~s(xVar, df = 20))
      varPred <- stats::predict(varSpline, se.fit = TRUE)
      distance <- abs(yVar - varPred$fit)
      nameDist <- paste("Distance of", subNameVar, "from the curve", sep = " ")
      df[,as.character(nameDist)] <- distance
    }
  }
  return(df)
}

