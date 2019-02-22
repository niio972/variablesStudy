library(plotly)
library(phisWSClientR)
library(webapp)
library(gam)
initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
aToken <- getToken("guest@opensilex.org","guest")
token <- aToken$data
plotVarRel("precipitation_daily rainfall_millimeter", "wind_hourly maximum wind speed_meter per second", token = token)



plotVarRel <- function(varX, varY, startDate = NULL, endDate = NULL, token, smoothing = TRUE){
  varX = "temperature_hourly instant temperature_degree celsius"
    varY = "precipitation_daily rainfall_millimeter"
    token=token
    startDate = as.POSIXct("2017-05-21", tz = "UTC", format = "%Y-%m-%d" )
  ## Data recuperation
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
      DataX <- data.frame(nbVar = nbVar, date = dates, values = value)
    }else{
      DataY <- data.frame(nbVar = nbVar, date = dates, values = value)
    }

  }
  Data <- rbind(DataX, DataY)

  # Test synchonisation
  # if(max(DataX$date) != max(DataY$date) | min(DataX$date) != min(DataY$date)){
  #   stop("the two variables are not synchronised or avaliable during this period of time")
  # }

  #   # Chosen dates
      if(!is.null(startDate)){
      startDate <- as.POSIXct(startDate, tz = "UTC", format = "%Y-%m-%d")
      if(startDate <= max(Data$date)){
        Data <- Data[which(Data$date >= startDate),]
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
  DataX = Data[which(Data$nbVar==1), ]
  DataY = Data[which(Data$nbVar==2), ]

  ## Interpolation over the same dates
  timeDuration <- max(Data$date)-min(Data$date)
  stepDuration <- min(length(DataX$date), length(DataY$date))
  newDates <- seq(from = min(Data$date), to = max(Data$date), by = timeDuration/stepDuration)[-1]
  if(length(Data$date) > 20){
    df = 20
  } else {
    df <- length(Data$date)-1
  }

  varSplineX <- gam::gam(values~s(date, df = df), data = DataX)
  #varPredX <- stats::predict(varSplineX, se.fit=TRUE)
  varPredX <- stats::predict(varSplineX, newdata = data.frame(date = newDates), type = "terms")

  data(gam.newdata)
  varSplineY <- gam::gam(values~s(date, df = df), data = DataY)
  varPredY <- stats::predict(varSplineY, newDates)
  #varPredY <- stats::predict(varSplineY, se.fit=TRUE)

  predData <- data.frame(newDates, varPredX, varPredY)

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
  # Affichage des variables et leur lissage
  # Lissage non paramétrique par B-spline

  # Mise en forme des traces
  marker <- NULL
  marker$color <- as.character(colorVar[1])
  hoverlabel <- list(bgcolor = colorBgHover, font = list(color = colorText), hoveron = "")
  hoverlabel$bordercolor <- as.character(colorVar[1])

  p <- plotly::add_markers(p, x = predData$fit, y = predData$fit.1, marker = marker, name = varPretty[1,"method"], yaxis = y$title, hoverlabel = hoverlabel,
                           text = paste(
                                  paste( round(predData$fit.1, digits = 2),varPretty[2,"unity"]),
                                  paste(" ~ ", round(predData$fit,2),varPretty[1,"unity"] ),
                                  sep = "\n"),
                           hoverinfo = 'text')


  y <- list(title = paste('<b>', varPretty[2, "name"], ' (', varPretty[2, "unity"], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
            gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
  p <- plotly::layout(p, yaxis = y)
  p <- plotly::layout(p, title =paste( "<b>Tendency of ", x$title, " ~ ", y$title, "</br>"))

  htmlwidgets::saveWidget(p, "relVarPlot.html", selfcontained = FALSE)
}

