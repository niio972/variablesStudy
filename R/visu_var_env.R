##' @title Plot Environmental Data
##'
##' @importFrom magrittr %>%
##' @importFrom gam s
##' @importFrom plotly layout
##' @importFrom plotly plot_ly
##' @importFrom plotly add_trace
##'
##' @param nameVar name of the variable to plot
##' @param startDate date from which to plot
##' @param endDate date to which to plot
##' @param sensor sensor's name that recorded the values
##' @param token a token from getToken function
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
  # variable's informations
  varPrettyTot <- getVarPretty(token = token)
  Data <- NULL
  # Chosen variable
  for (i in 1: length(nameVar)){
    nameString <- toString(nameVar[i])
    varMeth <- strsplit(nameString, split="_")
    methodVar <- varMeth[[1]][2]
    subNameVar <- varMeth[[1]][1]
    # Recuperation of the data from the WS
    enviroData <- getDataVarPretty(nameVar = subNameVar, methodVar = methodVar, varPretty = varPrettyTot, token = token)
    varPretty <- cbind(varPrettyTot, enviroData$varPretty)
    enviroData <- enviroData$enviroData
    # Values
    yVar <- enviroData$value
    if (i == 1){
      xVar <- as.POSIXct(enviroData$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
    }
    Data <- cbind(Data, y = yVar)
  }
  # Chosen sensor
  if(!is.null(sensor)){
    sensorsNames <- enviroData$sensorUri
    for (i in 1:length(sensorsNames)){
      sensorName <- strsplit(sensorsNames[i], split="/")
      sensorsNames[i] <- sensorName[[1]][6]
    }
    Data$sensorsNames <- sensorsNames
    Data <- Data[which(Data$sensorsNames == sensor),]
  }
  # Chosen dates
  if(!is.null(startDate)){
    startDate <- as.POSIXct(startDate, tz = "UTC", format = "%Y-%m-%d")
    Data <- Data[which(Data$x >= startDate),]
  }
  if (!is.null(endDate)){
    endDate <- as.POSIXct(endDate, tz = "UTC", format = "%Y-%m-%d")
    Data <- Data[which(Data$x <= endDate),]
  }

  ## Plotting
  # Labels
  colorVar <- list("#7CB5EC", "#0F528A", "#003152", "#577A003")
  colorBgHover <- "#F8F8F8"
  colorText <- "#525252"
  y <- list(title = paste('<b>', varPretty$name[1], ' (',varPretty$unity[1], ')' , '</b>', sep = ""), color = '#282828',
            tickfont = list(family = 'serif'), gridwidth = 2)
  x <- list(title = '<b>Time</b>', tickfont = list(family = 'serif'), gridwidth = 2)
  title <- list(size = 20, color = '#282828', tickfont = list(family = 'serif'))
  # Plot
  p <- plotly::plot_ly()
  p <- plotly::layout(p, xaxis = x, yaxis = y,
                      titlefont = title,
                      legend = list(x = 0.80, y = -0.2),
                      margin = list(l = 60, r = 70, t = 70, b =  60))
  # Affichage des variables et leur lissage
  for (i in 1:(length(nameVar))){
    # Lissage non paramétrique par B-spline

    # Mise en forme des traces
    nameY <- paste('y', i, sep = "")
    marker <- NULL
    marker$color <- as.character(colorVar[i])
    hoverlabel <- list(bgcolor = colorBgHover, font = list(color = colorText), hoveron = "")
    hoverlabel$bordercolor <- as.character(colorVar[i])
    # Ajout des traces au graphique
    if(smoothing == TRUE){
      line <- NULL
      line$color <- as.character(colorVar[i])
      varSpline <- gam::gam(Data[,i]~s(xVar, df = 20))
      modele <- stats::predict(varSpline, newdata = xVar)
      p <- plotly::add_lines(p, x = xVar, y = modele, line = line, name = paste(varPretty$acronym[i], "(smoothed curve)", sep = " "), yaxis = nameY)
      opacity <- 0.5
    } else {
        opacity <- 1
      }
    p <- plotly::add_markers(p, x = xVar, y = Data[, i], marker = marker, opacity = opacity,name = varPretty$method[i], yaxis = nameY, hoverlabel = hoverlabel,
                             text = ~paste(xVar, '<br>', varPretty$acronym[i], ': <b>', Data[,i], varPretty$unity[i], '</b>'), hoverinfo = 'text')
  }
  if (length(nameVar) == 1){
    p <- plotly::layout(p, title = paste('<b>Tendency of ', varPretty$name[1],'</b><br><i>', varPretty$method[1],'</i>' , sep = ""))
  } else if (i == 2) {
    y <- list(title = paste('<b>', varPretty$name[2], ' (',varPretty$unity[2], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
              gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
    p <- plotly::layout(p, yaxis2 = y)
    p <- plotly::layout(p, title = "<b>Tendency of environmental variables among time</br>")
  } else {
    y <- list(title = paste('<b>', varPretty$name[2], ' (',varPretty$unity[2], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
              gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
    p <- plotly::layout(p, yaxis = y)
    p <- plotly::layout(p, title = "<b>Tendency of environmental variables among time</br>")

  }
  htmlwidgets::saveWidget(p, "test1var.html", selfcontained = FALSE)
}

##' @title Get Variable's Names from WS2 and formate them
##'
##' @importFrom phisWSClientR initializeClientConnection
##' @importFrom phisWSClientR getEnvironmentData
##'
##' @param token a token from getToken function
##'
##' @return WSResponse
##' @export
##'
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##'  aToken <- getToken("guest@opensilex.org","guest")
##'  token <- aToken$data
##'  getVarPretty(token = token)
##' }
getVarPretty <- function(token){
  phisWSClientR::initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
  rawVar <- phisWSClientR::getVariables2(token = token)
  names <- rawVar$data$label
  methods <- rawVar$data$label
  for (i in 1:length(names)){
    names[i] <- strsplit(names[i], split="_")[[1]][1]
    methods[i] <- strsplit(methods[i], split="_")[[1]][2]
  }
  acronyms <- rawVar$data$trait$label
  unitVar <- rawVar$data$unit$comment
  uriVar <- rawVar$data$uri
  varPretty <- data.frame(name = names, method = methods, acronym = acronyms, unity = unitVar, uri = uriVar)
  varPretty <- data.frame(lapply(varPretty, as.character), stringsAsFactors=FALSE)
  return(varPretty)
}

##' @title Get Data from WS2 and formate them
##'
##' @importFrom phisWSClientR initializeClientConnection
##' @importFrom phisWSClientR getVariables2
##'
##' @param nameVar name of the variable to plot
##' @param methodVar name of the method used to collect data
##' @param varPretty from getVarPretty
##' @param token a token from getToken function
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

  nomVar <- paste(toupper(substr(varPretty$name[numVar],1,1)), substr(varPretty$name[numVar],2,nchar(varPretty$name[numVar])), sep = "")
  methodVar <- as.character(varPretty$method[numVar])
  acronymVar <- as.character(varPretty$acronym[numVar])
  unityVar <- as.character(varPretty$unity[numVar])
  varPretty <- list(name = nomVar, method = methodVar, acronym = acronymVar, unity = unityVar)
  return(list(enviroData = enviroData, varPretty = varPretty))
}

