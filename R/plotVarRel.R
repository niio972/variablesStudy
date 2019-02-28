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
#' @param varX name of the variable to plot in X axis
#' @param varY name of the variable to plot in Y axis
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
#'  plotVarRel( "temperature_hourly instant temperature_degree celsius",
#'        "radiation_hourly global radiation_joule per square centimeter", token = token, trend = TRUE)

#' }
#'
#' @export
#'
plotVarRel <- function(varX, varY, startDate = NULL, endDate = NULL, trend = FALSE, wsUrl = "www.opensilex.org/openSilexAPI/rest/", token){

  phisWSClientR::initializeClientConnection(apiID="ws_private", url = wsUrl)

  # Data recuperation
  # variable's informations
  varPrettyTot <- getVarPretty(token = token)
  Data <- NULL
  varPretty <- NULL
  # Chosen variable
  nameVar <- list(varX, varY)
  for (i in 1: length(nameVar)){
    nameString <- toString(nameVar[i])
    varMeth <- strsplit(nameString, split="_")
    methodVar <- varMeth[[1]][2]
    subNameVar <- varMeth[[1]][1]
    # Recuperation of the data from the WS
    enviroData <- getDataVarPretty(nameVar = subNameVar, methodVar = methodVar, varPretty = varPrettyTot, token = token)
    varPrettyI <- t(data.frame(matrix(unlist(enviroData$varPretty))))
    varPretty <- rbind(varPretty, varPrettyI)
    enviroData <- enviroData$enviroData

    # Values
    value <- enviroData$value
    dates <- as.POSIXct(enviroData$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
    colnames(varPretty) <- c("name", "method", "acronym", "unity")
    nbVar <- rep(i, length(enviroData))

    if(i==1){
      DataX <- data.frame(nbVar = nbVar, dates = dates, values = value)
    }else{
      DataY <- data.frame(nbVar = nbVar, dates = dates, values = value)
    }
  }
  Data <- rbind(DataX, DataY)

  # Chosen dates
  if(!is.null(startDate)){
    startDate <- as.POSIXct(startDate, tz = "UTC", format = "%Y-%m-%d")
    if(startDate <= max(Data$dates)){
      Data <- Data[which(Data$dates >= startDate), ]
    }
  }
  if (!is.null(endDate)){
    endDate <- as.POSIXct(endDate, tz = "UTC", format = "%Y-%m-%d")
    if(endDate >= min(Data$dates)){
      Data <- Data[which(Data$dates <= endDate), ]
    }
  }
  DataX = Data[which(Data$nbVar == 1), ]
  DataY = Data[which(Data$nbVar == 2), ]


  # Test synchronization
  if(max(DataX$dates) != max(DataY$dates) | min(DataX$dates) != min(DataY$dates)){
    Interpolation = TRUE
    warning("The two variables are not synchronised or avaliable during this period of time. \n Interpolation realized on a new common time period.")
    upper <- min( max(DataX$dates) , max(DataY$dates))
    lower <- max( min(DataX$dates) , min(DataY$dates))
    Data <- Data[ which(Data$dates >= lower & Data$dates<=upper),]
  }else{
    Interpolation = FALSE
    Data <- cbind(DataX, Y = DataY$values)
    names(Data) = c("nbVar", "dates", "X", "Y")
  }



  ## Interpolation over the same dates
  if( Interpolation == TRUE){
    timeDuration <- max(Data$dates)-min(Data$dates)
    stepDuration <- min(length(DataX$dates), length(DataY$dates))
    newDates <- seq(from = min(Data$dates), to = max(Data$dates), by = timeDuration/stepDuration)[-1]
    if(length(Data$dates) > 20){
      degFree <- 20
    } else {
      degFree <- length(Data$dates)-1
    }


    varSplineX <- gam::gam(values~s(dates, df = 20), data = DataX)
    varPredX <- stats::predict(object = varSplineX, newdata = data.frame(dates = newDates), type = "response")

    varSplineY <- gam::gam(values~s(dates, df = 20), data = DataY)
    varPredY <- stats::predict(object = varSplineY, newdata = data.frame(dates = newDates), type = "response")
    Data <- data.frame(dates = newDates, X = varPredX, Y = varPredY)

  }else{
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
  y <- list(title = paste('<b>', varPretty[2,"name"], ' (',varPretty[2,"unity"], ')' , '</b>', sep = ""), color = '#282828',
            tickfont = list(family = 'serif'), gridwidth = 2)
  x <- list(title = paste('<b>', varPretty[1,"name"], ' (',varPretty[1,"unity"], ')' , '</b>', sep = ""), color = '#282828',
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

  p <- plotly::add_markers(p, x = Data$X, y = Data$Y, marker = marker, name = varPretty[1,"method"], yaxis = y$title, hoverlabel = hoverlabel,
                           text = paste(
                             paste( round(Data$Y, digits = 2),varPretty[2,"unity"]),
                             paste(" ~ ", round(Data$X,2),varPretty[1,"unity"] ),
                             sep = "\n"),
                           hoverinfo = 'text')

  if(trend==TRUE){
    if(length(Data$dates) > 20){
      degFree <- 20
    } else {
      degFree <- length(Data$dates)-1
    }
    varSpline <- gam::gam(Y ~ s(X, df = 20), data = Data)
    varPred<- stats::predict(object = varSpline, se.fit = TRUE)

    smoothData <- data.frame(Dates = Data$dates, X = Data$X, Y = varPred$fit)


    p <- plotly::add_lines(p, x = smoothData$X, y = smoothData$Y, line = list(color = as.character(colorVar[i])), yaxis = y$title,
                           name = "smoothed curve)")
  }


  y <- list(title = paste('<b>', varPretty[2, "name"], ' (', varPretty[2, "unity"], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
            gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
  p <- plotly::layout(p, yaxis = y)
  p <- plotly::layout(p, title =paste( "<b>Tendency of ", y$title, " ~ ", x$title, "</br>"))

  htmlwidgets::saveWidget(p, "plotVarRelWidget.html", selfcontained = FALSE)

  # export PlotlySchema and configuration
  jsonlite::write_json(plotly::plotly_data(p), "plotlyData")
  plotlyJson <- plotly::plotly_json(p, pretty = FALSE)
  plot <- jsonlite::fromJSON(plotlyJson)
  jsonlite::write_json(plot, "plotlySchema")
  jsonlite::write_json(Data,"gridData")
}
