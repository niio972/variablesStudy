#-------------------------------------------------------------------------------
# Program: getDF.R
# Objective: draw graph of environment variable over time
# Authors: Chourrout Elise
# Creation: 15/02/2019
# Update:
#-------------------------------------------------------------------------------


#' @title Plot two Environmental variables relationship
#'
#' @importFrom magrittr %>%
#' @importFrom gam s
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom stats qnorm
#'
#' @param varX uri of the variable to plot in X axis, from the \code{\link{variableList}} function or the web service directly
#' @param varY uri of the variable to plot in Y axis, from the \code{\link{variableList}} function or the web service directly
#' @param startDate date from which to plot
#' @param endDate date to which to plot
#' @param trend logical, draw the trend of the scatterplot
#' @param wsUrl url of the webservice
#' @param token a token from \code{\link{getToken}} function
#'

#' @examples
#' \donttest{
#'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
#'  aToken <- getToken("guest@opensilex.org","guest")
#'  token <- aToken$data
#'  vars <- variableList(token = token)
#'  vars
#'  plotVarRel( vars$value[1],
#'        vars$value[2],
#'        token = token,
#'        trend = TRUE)
#' }
#'
#' @export
#'
plotVarRel <- function(varX, varY, startDate = NULL, endDate = NULL, trend = FALSE, wsUrl = "www.opensilex.org/openSilexAPI/rest/", token){

  phisWSClientR::initializeClientConnection(apiID="ws_private", url = wsUrl)

  ### Collecting Data
  variableList <- variableList(token = token)
  ## Data
  varURI <- list(varX, varY)
  Data = lapply(varURI,FUN = function(uri){
    enviroData <- getDataVar(varURI = uri, variableList = variableList, token = token)$enviroData
    yVar <- enviroData$value
    # Casting Date in the right format
    xVar <- as.POSIXct(enviroData$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
    DataX <- data.frame(date = xVar, value = yVar)

    ## Filtering
    if(!is.null(startDate)){
      startDate <- as.POSIXct(startDate, tz = "UTC", format = "%Y-%m-%d")
      DataX <- DataX[which(DataX$date >= startDate),]
    }
    if (!is.null(endDate)){
      endDate <- as.POSIXct(endDate, tz = "UTC", format = "%Y-%m-%d")
      DataX <- DataX[which(DataX$date <= endDate),]
    }
    return(DataX)
  })
  variableList <- variableList[which(variableList$uri %in% varURI), ]

  DataX <- Data[[1]]
  DataY <- Data[[2]]

  # Test synchronization
  if(max(DataX$date) != max(DataY$date) | min(DataX$date) != min(DataY$date)){
    Interpolation = TRUE
    warning("The two variables are not synchronised or avaliable during this period of time. \n Interpolation realized on a new common time period.")
    upper <- min( max(DataX$date) , max(DataY$date))
    lower <- max( min(DataX$date) , min(DataY$date))
    Data <- lapply(X = Data, FUN =  function(Data){
      return(Data[ which(Data$date >= lower & Data$date<=upper),])
    })
  }else{
    Interpolation = FALSE
  }
  DataX <- Data[[1]]
  DataY <- Data[[2]]

  ## Interpolation over the same dates
  if( Interpolation == TRUE){
    timeDuration <- max(DataX$date, DataY$date)-min(DataX$date, DataY$date)
    stepDuration <- min(length(DataX$date), length(DataY$date))
    newDates <- seq(from = min(DataX$date, DataY$date), to = max(DataX$date, DataY$date), by = timeDuration/stepDuration)[-1]
    if(length(Data$date) > 20){
      degFree <- 20
    } else {
      degFree <- length(Data$date)-1
    }
    varSplineX <- gam::gam(value~s(date, df = 20), data = DataX)
    varPredX <- stats::predict(object = varSplineX, newdata = data.frame(date = newDates), type = "response")

    varSplineY <- gam::gam(value~s(date, df = 20), data = DataY)
    varPredY <- stats::predict(object = varSplineY, newdata = data.frame(date = newDates), type = "response")
    DataPred <- data.frame(dates = newDates, X = varPredX, Y = varPredY)
  }else{
    DataPred = data.frame(dates = DataX$date, X = DataX$value, Y = DataY$value)
  }

  ## Plotting
  # Labels
  colorVar <- list("#7CB5EC", "#0F528A", "#003152", "#577A003")
  colorRibbon <- colorVar
  colorFill <- colorVar
  for (i in 1:length(colorVar)){
    colorRibbon[i] <- paste(colorRibbon[i], "0D", sep = "")
    colorFill[i] <- paste(colorFill[i], "4D", sep = "")
  }
  colorBgHover <- "#F8F8F8"
  colorText <- "#525252"
  y <- list(title = paste('<b>', variableList[2,"name"], ' (',variableList[2,"unity"], ')' , '</b>', sep = ""), color = '#282828',
            tickfont = list(family = 'serif'), gridwidth = 2)
  x <- list(title = paste('<b>', variableList[1,"name"], ' (',variableList[1,"unity"], ')' , '</b>', sep = ""), color = '#282828',
            tickfont = list(family = 'serif'), gridwidth = 2)

  title <- list(size = 20, color = '#282828', tickfont = list(family = 'serif'))

  # Plot
  p <- plotly::plot_ly()
  p <- plotly::layout(p, xaxis = x, yaxis = y,
                      titlefont = title,
                      margin = list(l = 60, r = 70, t = 70, b =  60))
  # Mise en forme des traces
  marker <- NULL
  marker$color <- as.character(colorVar[1])
  hoverlabel <- list(bgcolor = colorBgHover, font = list(color = colorText), hoveron = "")
  hoverlabel$bordercolor <- as.character(colorVar[1])

  p <- plotly::add_markers(p, x = DataPred$X, y = DataPred$Y, marker = marker, name = variableList[1,"method"], yaxis = y$title, hoverlabel = hoverlabel,
                           text = paste(
                             paste( round(DataPred$Y, digits = 2),variableList[2,"unity"]),
                             paste(" ~ ", round(DataPred$X,2),variableList[1,"unity"] ),
                             sep = "\n"),
                           hoverinfo = 'text')

  if(trend==TRUE){
    if(length(DataPred$dates) > 20){
      degFree <- 20
    } else {
      degFree <- length(DataPred$dates)-1
    }
    varSpline <- gam::gam(Y ~ s(X, df = 20), data = DataPred)
    varPred<- stats::predict(object = varSpline, se.fit = TRUE)

    smoothData <- data.frame(Dates = DataPred$dates, X = DataPred$X, Y = varPred$fit)

    p <- plotly::add_lines(p, x = smoothData$X, y = smoothData$Y, line = list(color = as.character(colorVar[i])), yaxis = y$title,
                           name = "smoothed curve)")
  }

  y <- list(title = paste('<b>', variableList[2, "name"], ' (', variableList[2, "unity"], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
            gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
  p <- plotly::layout(p, yaxis = y)
  p <- plotly::layout(p, title =paste( "<b>Tendency of ", y$title, " ~ ", x$title, "</br>"))

  htmlwidgets::saveWidget(p, "plotWidget.html", selfcontained = FALSE)

  # export PlotlySchema and configuration
  jsonlite::write_json(plotly::plotly_data(p), "plotlyData")
  plotlyJson <- plotly::plotly_json(p, pretty = FALSE)
  plot <- jsonlite::fromJSON(plotlyJson)
  jsonlite::write_json(plot, "plotlySchema")
  jsonlite::write_json(Data,"gridData")
}
