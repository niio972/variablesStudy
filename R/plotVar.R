#-------------------------------------------------------------------------------
# Program: getDF.R
# Objective: draw graph of environment variable over time
# Authors: Chourrout Elise
# Creation: 15/02/2019
# Update:
#-------------------------------------------------------------------------------

##' @title Plot Environmental Data
##'
##' @importFrom magrittr %>%
##' @importFrom gam s
##' @importFrom plotly layout
##' @importFrom plotly plot_ly
##' @importFrom plotly add_trace
##' @importFrom stats qnorm
##'
##' @param nameVar name of the variable to plot
##' @param startDate date from which to plot
##' @param endDate date to which to plot
##' @param sensor sensor's name that recorded the values
##' @param token a token from \code{\link{getToken}} function
##' @param smoothing logical, smoothing of the data
##'
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##'  aToken <- getToken("guest@opensilex.org","guest")
##'  token <- aToken$data
##'  plotVar("temperature", token = token)
##' }
##'
##' @export
##'
plotVar <- function(nameVar, startDate = NULL, endDate = NULL, sensor = NULL, token, smoothing = TRUE){

  ## Data recuperation
  # Variable's information
  varPrettyTot <- getVarPretty(token = token)

  # Data
  Data <- NULL
  varPretty <- NULL
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

  ### Plotting

  ## Theme
  # Color Palette
  colorVar <- list("#7CB5EC", "#0F528A", "#003152", "#577A003")
  colorRibbon <- colorVar
  colorFill <- colorVar
  for (i in 1:length(colorVar)){
    colorRibbon[i] <- paste(colorRibbon[i], "0D", sep = "")
    colorFill[i] <- paste(colorFill[i], "4D", sep = "")
  }
  colorBgHover <- "#F8F8F8"
  colorText <- "#525252"

  # Labels and grid
  y <- list(title = paste('<b>', varPretty[1,"name"], ' (',varPretty[1,"unity"], ')' , '</b>', sep = ""), color = '#282828',
            tickfont = list(family = 'serif'), gridwidth = 2)
  x <- list(title = '<b>Date</b>', tickfont = list(family = 'serif'), gridwidth = 2)
  title <- list(size = 20, color = '#282828', tickfont = list(family = 'serif'))

  ## Plot
  p <- plotly::plot_ly()

  # Backgound creation
  p <- plotly::layout(p, xaxis = x, yaxis = y,
                      titlefont = title,
                      margin = list(l = 60, r = 70, t = 70, b =  60))

  for (i in 1:(length(nameVar))){

    # Markers and Lines formatting
    nameY <- paste('y', i, sep = "")
    marker <- NULL
    marker$color <- as.character(colorVar[i])
    hoverlabel <- list(bgcolor = colorBgHover, font = list(color = colorText), hoveron = "")
    hoverlabel$bordercolor <- as.character(colorVar[i])

    # Values of the graph
    if(length(nameVar)==1){
      yVar <- Data$values
    } else {
      yVar <- Data$values[,i]

    }

    # Smoothing - Generalized additive model
    if(smoothing == TRUE){

      # Parameters of the model
      if(length(Data$date) > 20){
        df = 20
      } else {
        df <- length(Data$date)-1
      }

      # Model creation
      varSpline <- gam::gam(yVar~s(Data$date, df = df))
      varPred <- stats::predict(varSpline, se.fit = TRUE)
      modeleDf <- data.frame(x = Data$date[order(Data$date)] , y = varPred$fit,
                             lb = as.numeric(varPred$fit - qnorm(0.975) * varPred$se.fit),
                             ub = as.numeric(varPred$fit + qnorm(0.975) * varPred$se.fit))

      # Screening of the smoothed curve
      p <- plotly::add_lines(p, x = Data$date, y = varPred$fit, line = list(color = as.character(colorVar[i])), yaxis = nameY,
                             name = paste(varPretty[i,"acronym"], "(smoothed curve)", sep = " "))

      # Screening of the confidence interval
      p <- plotly::add_ribbons(p, x = Data$date, ymin = modeleDf$lb, ymax = modeleDf$ub,  yaxis = nameY,
                               line = list(color = as.character(colorRibbon[i])),
                               fillcolor = as.character(colorFill[i]),
                               name = "Standard Error", showlegend = FALSE)

      # Screening of the values as markers
      p <- plotly::add_markers(p, x = Data$date, y = yVar, marker = marker, opacity = 0.2, name = varPretty[i,"method"], yaxis = nameY, hoverlabel = hoverlabel,
                               text = ~paste(Data$date, '<br>', varPretty[i,"acronym"], ': <b>', yVar, varPretty[i,"unity"], '</b>'), hoverinfo = 'text')
    } else {
      # Screening of the values without smoothing as lines
      p <- plotly::add_lines(p, x = Data$date, y = yVar, line = list(color = as.character(colorVar[i])), name = varPretty[i,"method"], yaxis = nameY, hoverlabel = hoverlabel,
                               text = ~paste(Data$date, '<br>', varPretty[i,"acronym"], ': <b>', yVar, varPretty[i,"unity"], '</b>'), hoverinfo = 'text')    }

  }
  # Labels
  if (length(nameVar) == 1){
    p <- plotly::layout(p, title = paste('<b>Tendency of ', varPretty[1,"name"], '</b><br><i>', varPretty[1,"method"], '</i>' , sep = ""))
  } else if (i == 2) {
    y <- list(title = paste('<b>', varPretty[2, "name"], ' (', varPretty[2, "unity"], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
              gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
    p <- plotly::layout(p, yaxis2 = y)
    p <- plotly::layout(p, title = "<b>Tendency of environmental variables among time</br>")
  } else {
    y <- list(title = paste('<b>', varPretty[2, "name"], ' (', varPretty[2, "unity"], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
              gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
    p <- plotly::layout(p, yaxis = y)
    p <- plotly::layout(p, title = "<b>Tendency of environmental variables among time</br>")
  }

  # Creation of the html object to screen in the WebApp
  htmlwidgets::saveWidget(p, "enviroVarPlot.html", selfcontained = FALSE)
}
