#-------------------------------------------------------------------------------
# Program: getDF.R
# Objective: draw graph of environment variable over time
# Authors: Chourrout Elise
# Creation: 15/02/2019
# Update:
#-------------------------------------------------------------------------------

#' @title Plot Environmental Data
#'
#' @importFrom magrittr %>%
#' @importFrom gam s
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly plotly_json
#' @importFrom stats qnorm
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite write_json
#'
#' @param varURI uri of the variable to plot from the \code{\link{variableList}} function or the web service directly
#' @param startDate date from which to plot
#' @param endDate date to which to plot
#' @param sensor uri of the sensor that recorded the values
#' @param token a token from \code{\link{getToken}} function
#' @param smoothing logical, smoothing of the data
#' @param wsUrl url of the webservice

#' @examples
#' \donttest{
#'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
#'  aToken <- getToken("guest@opensilex.org","guest")
#'  token <- aToken$data
#'  vars <- variableList(token = token)
#'  plotVar(vars$value[1], token = token)
#' }
#'
#' @export

plotVar <- function(varURI, startDate = NULL, endDate = NULL, sensor = NULL, token, smoothing = TRUE, wsUrl = "www.opensilex.org/openSilexAPI/rest/"){
  phisWSClientR::initializeClientConnection(apiID="ws_private", url = wsUrl)

  ### Data recuperation
  variableList <- variableList(token = token)
  ## Data
  Data <- list()
  Data = lapply(varURI,FUN = function(uri){
    enviroData <- getDataVar(varURI = uri, variableList = variableList, token = token)$enviroData
    yVar <- enviroData$value
    # Casting Date in the right format
    xVar <- as.POSIXct(enviroData$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
    DataX <- data.frame(date = xVar, value = yVar)

    ## Data filtering
    # Chosen dates
    if(!is.null(startDate)){
      startDate <- as.POSIXct(startDate, tz = "UTC", format = "%Y-%m-%d")
      DataX <- DataX[which(DataX$date >= startDate),]
    }
    if (!is.null(endDate)){
      endDate <- as.POSIXct(endDate, tz = "UTC", format = "%Y-%m-%d")
      DataX <- DataX[which(DataX$date <= endDate),]
    }
    # Chosen sensor
    if(!is.null(sensor)){
      if(length(grep(sensor, enviroData$sensorUri)) != 0){
        DataX <- DataX[which(enviroData$sensorUri == sensor),]
      }else{
        warning("This variable is not measured by the sensor. Change either one or the two.")
      }
    }
    return(DataX)
  })
  variableList <- variableList[which(variableList$uri %in% varURI), ]

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
  y <- list(title = paste('<b>', variableList[1,"name"], ' (',variableList[1,"unity"], ')' , '</b>', sep = ""), color = '#282828',
            tickfont = list(family = 'serif'), gridwidth = 2)
  x <- list(title = '<b>Date</b>', tickfont = list(family = 'serif'), gridwidth = 2)
  title <- list(size = 20, color = '#282828', tickfont = list(family = 'serif'))

  ## Plot
  p <- plotly::plot_ly()
  # Backgound creation
  p <- plotly::layout(p, xaxis = x, yaxis = y,
                      titlefont = title,
                      margin = list(l = 60, r = 70, t = 70, b =  60))

  for (i in 1:(length(varURI))){

    # Markers and Lines formatting
    nameY <- paste('y', i, sep = "")
    marker <- NULL
    marker$color <- as.character(colorVar[i])
    hoverlabel <- list(bgcolor = colorBgHover, font = list(color = colorText), hoveron = "")
    hoverlabel$bordercolor <- as.character(colorVar[i])
    # Values of the graph
    yVar <- Data[[i]]$value
    # Smoothing - Generalized additive model
    if(smoothing == TRUE){

      # Parameters of the model
      if(length(Data[[i]]$date) > 20){
        df = 20
      } else {
        df <- length(Data[[i]]$date)-1
      }

      # Model creation
      varSpline <- gam::gam(yVar~s(Data[[i]]$date, df = df))
      varPred <- stats::predict(varSpline, se.fit = TRUE)
      modeleDf <- data.frame(x = Data[[i]]$date[order(Data[[i]]$date)] , y = varPred$fit,
                             lb = as.numeric(varPred$fit - qnorm(0.975) * varPred$se.fit),
                             ub = as.numeric(varPred$fit + qnorm(0.975) * varPred$se.fit))

      # Screening of the smoothed curve
      p <- plotly::add_lines(p, x = Data[[i]]$date, y = varPred$fit, line = list(color = as.character(colorVar[i])), yaxis = nameY,
                             name = paste(variableList[i,"acronym"], "(smoothed curve)", sep = " "))

      # Screening of the confidence interval
      p <- plotly::add_ribbons(p, x = Data[[i]]$date, ymin = modeleDf$lb, ymax = modeleDf$ub,  yaxis = nameY,
                               line = list(color = as.character(colorRibbon[i])),
                               fillcolor = as.character(colorFill[i]),
                               name = "Standard Error", showlegend = FALSE)

      # Screening of the values as markers
      p <- plotly::add_markers(p, x = Data[[i]]$date, y = yVar, marker = marker, opacity = 0.2, name = variableList[i,"method"], yaxis = nameY, hoverlabel = hoverlabel,
                               text = ~paste(Data[[i]]$date, '<br>', variableList[i,"acronym"], ': <b>', yVar, variableList[i,"unity"], '</b>'), hoverinfo = 'text')
    } else {
      # Screening of the values without smoothing as lines
      p <- plotly::add_lines(p, x = Data[[i]]$date, y = yVar, line = list(color = as.character(colorVar[i])), name = variableList[i,"method"], yaxis = nameY, hoverlabel = hoverlabel,
                               text = ~paste(Data[[i]]$date, '<br>', variableList[i,"acronym"], ': <b>', yVar, variableList[i,"unity"], '</b>'), hoverinfo = 'text')    }

  }
  # Labels
  if (length(varURI) == 1){
    p <- plotly::layout(p, title = paste('<b>Tendency of ', variableList[1,"name"], '</b><br><i>', variableList[1,"method"], '</i>' , sep = ""))
  } else if (i == 2) {
    y <- list(title = paste('<b>', variableList[2, "name"], ' (', variableList[2, "unity"], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
              gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
    p <- plotly::layout(p, yaxis2 = y)
    p <- plotly::layout(p, title = "<b>Tendency of environmental variables among time</br>")
  } else {
    y <- list(title = paste('<b>', variableList[2, "name"], ' (', variableList[2, "unity"], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
              gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
    p <- plotly::layout(p, yaxis = y)
    p <- plotly::layout(p, title = "<b>Tendency of environmental variables among time</br>")
  }

  # Creation of the html object to screen in the variablesStudy
  # print(plotly::plotly_json(p))
  htmlwidgets::saveWidget(p, "plotVarWidget.html", selfcontained = FALSE)
  # htmlwidgets::
  jsonlite::write_json(plotly::plotly_json(p), "plotlySchema")
  jsonlite::write_json(jsonlite::fromJSON(plotly::plotly_json(p)), "plotlyData")
  jsonlite::write_json(Data,"gridData")
}
