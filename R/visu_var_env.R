###############################################################################
################################### A FAIRE ###################################
###############################################################################
###     Ajouter de l'interactivité:                                         ###
### * recherche d'outliers                                                  ###
### * pouvoir changer les informations de ce point                          ###
###                                                                         ###
###   sources:                                                              ###
### * http://users.auth.gr/~prinosp/downloads/Galiatsatou_PAPER_416.pdf     ###
###############################################################################
###############################################################################
###############################################################################

##' @title plot.var
##'
##' @importFrom magrittr %>%
##' @importFrom plotly layout
##' @importFrom plotly plot_ly
##' @importFrom plotly add_trace
##'
##' @param nomVar (list) name of the variable to plot
##' @param startDate (date format %Y-%m-%d) date from which to plot
##' @param endDate (date format %Y-%m-%d) date to which to plot
##' @param sensor (number) sensor's name that recorded the values
##' @param token (string) a token from getToken function
##'
##' @return
##'
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##'  aToken <- getToken("guest@opensilex.org","guest")
##'  token <- aToken$data
##'  plotVar("temperature", token = token)
##'  plotVar("temperature", token = token, sensor = "s18002")
##'  plotVar("temperature", startDate = "2017-06-26", endDate = "2017-06-28", sensor = "s18002", token = token)
##' }
##'
##' @export
##'
plotVar <- function(nameVar, startDate = NULL, endDate = NULL, sensor = NULL, token){
  startDate = NULL
  endDate = NULL
  ## Data recuperation
  # variable's informations
  varPrettyTot <- getVarPretty(token = token)
  Data <- NULL
  varPretty <- NULL
  # Chosen variable
  for (i in 1: length(nameVar)){
    nameString <- toString(nameVar[i])
    varMeth <- strsplit(nameString, split="_")
    methodVar <- varMeth[[1]][2]
    subNameVar <- varMeth[[1]][1]
    # Recuperation of the data from the WS
    enviroData <- getDataVarPretty(nameVar = subNameVar, methodVar = methodVar, varPretty = varPrettyTot, token = token)
    print(enviroData$varPretty)
    varPretty <- cbind(varPretty, enviroData$varPretty)
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
  y <- list(title = paste('<b>', varPretty[1,1], ' (',varPretty[4,1], ')' , '</b>', sep = ""), color = '#282828',
            tickfont = list(family = 'serif'), gridwidth = 2)
  x <- list(title = '<b>Time</b>', tickfont = list(family = 'serif'), gridwidth = 2)
  title <- list(size = 20, color = '#282828', tickfont = list(family = 'serif'))

  # Plot
  p <- plotly::plot_ly(type = 'scatter', mode = 'lines+markers')
  p <- plotly::layout(p, xaxis = x, yaxis = y,
                      titlefont = title,
                      legend = list(x = 0.80, y = 1.1),
                      margin = list(l = 60, r = 70, t = 70, b =  60))
  # Legend
  for (i in 1:(length(nameVar))){
    nameY <- paste('y', i, sep = "")
    p <- plotly::add_trace(p, x = xVar, y = Data[, i], name = varPretty[1,i], yaxis = nameY,
                           text = ~paste("<b>", varPretty[3,i], ': ', Data[,i], varPretty[4,i], '</b><br>', xVar), hoverinfo = 'text')
  }
  if (length(nameVar) == 1){
    p <- plotly::layout(p, title = paste('<b>Tendency of ', varPretty[1,1],'</b><br><i>', varPretty[2,1],'</i>' , sep = ""))
  } else if (i == 2) {
    y <- list(title = paste('<b>', varPretty[1,2], ' (',varPretty[4,2], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
              gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
    p <- plotly::layout(p, yaxis2 = y)
    p <- plotly::layout(p, title = "<b>Tendency of environmental variables among time</br>")
  } else {
    y <- list(title = paste('<b>', varPretty[1,2], ' (',varPretty[4,2], ')' , '</b>', sep = ""), color = '#282828', showgrid = FALSE,
              gridwidth = 2,  tickfont = list(family = 'serif'), overlaying = "y", side = "right")
    p <- plotly::layout(p, yaxis = y)
    p <- plotly::layout(p, title = "<b>Tendency of environmental variables among time</br>")

  }
  p
  #htmlwidgets::saveWidget(p, "test1var.html", selfcontained = FALSE)
}

##' @title getVarPretty
##'
##' @importFrom phisWSClientR initializeClientConnection
##' @importFrom phisWSClientR getEnvironmentData
##'
##' @param token (string) a token from getToken function
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
  varPretty <- data.frame(name = names, method = methods, acronym = acronyms, unity = unitVar)
  return(varPretty)
}

##' @title getDataVarPretty
##'
##' @importFrom phisWSClientR initializeClientConnection
##' @importFrom phisWSClientR getVariables2
##'
##' @param nameVar (string) name of the variable to plot
##' @param methodVar (string) name of the method used to collect data
##' @param varPretty (data.frame) from getVarPretty
##' @param token (string) a token from getToken function
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
  zero <- rep('0', 3-length(numVar))
  nameUriVar <- paste(paste(zero, collapse=''), numVar, sep  = "")

  # Recuperation of the data from the WS
  myCount <- phisWSClientR::getEnvironmentData(token = token, variable = paste("http://www.phenome-fppn.fr/ues/id/variables/v", nameUriVar, sep = ""))$totalCount
  enviroData <- phisWSClientR::getEnvironmentData(token=token, variable = paste("http://www.phenome-fppn.fr/ues/id/variables/v", nameUriVar, sep = "") , verbose = TRUE, pageSize = myCount)$data

  nomVar <- paste(toupper(substr(levels(droplevels(varPretty$name[numVar])),1,1)), substr(levels(droplevels(varPretty$name[numVar])),2,nchar(levels(droplevels(varPretty$name[numVar])))), sep = "")
  methodVar <- levels(droplevels(varPretty$method[numVar]))
  acronymVar <- levels(droplevels(varPretty$acronym[numVar]))
  unityVar <- levels(droplevels(varPretty$unity[numVar]))


  varPretty <- list(name = nomVar, method = methodVar, acronym = acronymVar, unity = unityVar)
  return(list(enviroData = enviroData, varPretty = varPretty))
}
