#-------------------------------------------------------------------------------
# Program: getDF.R
# Objective: functions to get the incertitude data related to environment measures
# Authors: Chourrout Elise
# Creation: 15/02/2019
# Update:
#-------------------------------------------------------------------------------

#' @title getDF
#'
#' @description get the dataset of incertitudes for the data selected
#'
#' @param nameVar name of the variables to add in the data frame
#' @param token a token from \code{\link{getToken}} function
#' @param smoothing logical, smoothing of the data
#' @param sensor character, name of a sensor
#' @param endDate date, date from which to filter data, format "\%Y-\%m-\%dT\%H:\%M:\%S"
#' @param startDate date, date to which filter the data, format "\%Y-\%m-\%dT\%H:\%M:\%S"
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
getDF <- function(nameVar, token, smoothing = FALSE, sensor = NULL, endDate = NULL, startDate = NULL){

  ## Data recuperation
  # Variable's information
  varPrettyTot <- getVarPretty(token = token)

  # Data
  Data <- NULL
  varPretty <- NULL
  # Chosen variable
  for (i in 1: length(nameVar)){

    # Extraction of the variable's name
    nameString <- toString(nameVar[i])
    varMeth <- strsplit(nameString, split="_")
    methodVar <- varMeth[[1]][2]
    subNameVar <- varMeth[[1]][1]

    # Recuperation of the variable's data from the WS
    enviroData <- getDataVarPretty(nameVar = subNameVar, methodVar = methodVar, varPretty = varPrettyTot, token = token)
    varPrettyI <- t(data.frame(matrix(unlist(enviroData$varPretty))))

    # Variable's information
    varPretty <- rbind(varPretty, varPrettyI)

    # Values
    enviroData <- enviroData$enviroData

    # Values
    yVar <- enviroData$value

    # Casting Date in the right format
    if (i == 1){
      xVar <- as.POSIXct(enviroData$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
      colnames(varPretty) <- c("name", "method", "acronym", "unity")
      Data$date <- xVar
    }
    Data$values <- cbind(Data$values, y = yVar)
  }

  ## Filters on the data
  # Chosen sensor
  if(!is.null(sensor)){
    sensorsNames <- enviroData$sensorUri
    for (i in 1:length(sensorsNames)){
      sensorName <- strsplit(sensorsNames[i], split="/")
      sensorsNames[i] <- sensorName[[1]][6]
    }
    Data$sensorsNames <- sensorsNames
    if(length(grep(sensor, Data$sensorsNames)) != 0){
      Data <- Data[which(Data$sensorsNames == sensor),]
    }
  }

  # Chosen dates
  if(!is.null(startDate)){
    startDate <- as.POSIXct(startDate, tz = "UTC", format = "%Y-%m-%d")
    if(startDate <= max(Data$date)){
      if(length(nameVar) == 1){
        Data$values <- Data$values[which(Data$date >= startDate)]
      } else {
        Data$values <- Data$values[which(Data$date >= startDate),]
      }
      Data$sensorsNames <- Data$sensorsNames[which(Data$date >= startDate)]
      Data$date <- Data$date[which(Data$date >= startDate)]
    }
  }
  if (!is.null(endDate)){
    endDate <- as.POSIXct(endDate, tz = "UTC", format = "%Y-%m-%d")
    if(endDate >= min(Data$date)){
      if(length(nameVar) == 1){
        Data$values <- Data$values[which(Data$date <= endDate)]
      } else {
        Data$values <- Data$values[which(Data$date <= endDate),]
      }
      Data$sensorsNames <- Data$sensorsNames[which(Data$date <= endDate)]
      Data$date <- Data$date[which(Data$date <= endDate)]
    }
  }

  ## Data formatting
  dataFrame <- data.frame(Date = Data$date)

  # Values
  for (i in 1:(length(nameVar))){
    if(length(nameVar)==1){
      yVar <- Data$values
    } else {
      yVar <- Data$values[,i]
    }
    dataFrame <- cbind(dataFrame, yVar)

    # Labels
    names(dataFrame)[length(dataFrame)] <- as.character(varPretty[i,"name"])

    # Smoothing model - GAM
    if(smoothing == TRUE){

      # Parameters
      if(length(Data$date) > 20){
        df = 20
      } else {
        df <- length(Data$date)-1
      }
      # Model
      varSpline <- gam::gam(yVar~s(Data$date, df = df))
      varPred <- stats::predict(varSpline, se.fit = TRUE)

      # Distance
      dist <- abs(as.numeric(yVar) - as.numeric(varPred$fit))
      dataFrame <- cbind(dataFrame, dist)

      # Labels
      names(dataFrame)[length(dataFrame)] <- paste("Distance of ",varPretty[i,"name"], " from smoothed curve", sep = "")
    }
  }

  return(dataFrame)
}
