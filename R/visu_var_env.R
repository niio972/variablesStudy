# Packages
library(plotly)
library(magrittr)
library(openssl)

###############################################################################
################################### A FAIRE ###################################
###############################################################################
###     Ajouter de l'interactivité:                                         ###
### * récupérer les coordonnées d'un point sur lequel clique l'utilisateur  ###
### * pouvoir changer les informations de ce point                          ###
###                                                                         ###
###   sources:                                                              ###
### * https://plot.ly/r/click-events/                                       ###
###############################################################################
###############################################################################
###############################################################################

##' @title plot.var
##' @importFrom magrittr %>%
##' @param nom_var (string) name of the variable to plot
##' @param startDate (date format %Y-%m-%d) date from which to plot
##' @param endDate (date format %Y-%m-%d) date to which to plot
##' @param sensor (number) sensor's name that recorded the values
##' @param token (string) a token from getToken function
##' @return
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "138.102.159.36:8080/uesAPI/rest/")
##'  aToken <- getToken("guest@phis.fr","guest")
##'  token <- aToken$data
##'  plotVar("temperature", token = aToken$data)
##'  plotVar("temperature", token = aToken$data, sensor = "s18002")
##'  plotVar("temperature", startDate = "2017-06-26", endDate = "2017-06-28", sensor = "s18002", token = aToken$data)
##' }
##' @export
##'
plotVar <- function(nom_var, startDate = NULL, endDate = NULL, sensor = NULL, token){
  # argument mycount<-getEnvironmentData(token=aToken$data,
                            #""  variable = "http://www.phenome-fppn.fr/ues/id/variables/v005")$totalCount
  initializeClientConnection(apiID="ws_private", url = "138.102.159.36:8080/uesAPI/rest/")
  ## Data recuperation
  # variable's informations
  var <- getVariables2(token = token)
  nom_vars <- var$data$label
  methode_vars <- var$data$label
  for (i in 1:length(nom_vars)){
    nom_vars[i] <- strsplit(nom_vars[i], split="_")[[1]][1]
    methode_vars[i] <- strsplit(methode_vars[i], split="_")[[1]][2]

  }
  acronyme_var <- var$data$trait$label

  # Chosen variable
  var_meth <- strsplit(nom_var, split="_")
  if(length(var_meth[[1]])>1){
    num_var <- 1
    while(grepl(var_meth[[1]][2], methode_vars[num_var]) == FALSE && num_var < length(methode_vars)){
      print(num_var)
      num_var <- num_var+1
    }
  } else {
    num_var <- match(nom_var, nom_vars)
  }
  zero <- rep('0', 3-length(num_var))
  nom_uri_var <- paste(paste(zero, collapse=''), num_var, sep  = "")

  # Recuperation of the data from the WS
  envirodata <- getEnvironmentData(token=token, variable = paste("http://www.phenome-fppn.fr/ues/id/variables/v", nom_uri_var, sep = "") , verbose = TRUE)$data

  # Values
  y_var <- envirodata$value
  x_var <- as.POSIXct(envirodata$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  Data <- data.frame(x = x_var, y = y_var)

  # Chosen sensor
  if(!is.null(sensor)){
    sensors_names <- envirodata$sensorUri
    for (i in 1:length(sensors_names)){
      sensor_name <- strsplit(sensors_names[i], split="/")
      sensors_names[i] <- sensor_name[[1]][6]
    }
    Data$sensors_names <- sensors_names
    Data <- Data[which(Data$sensors_names == sensor),]
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

  # Unit
  unit_var <- var$data$unit$comment[num_var]

  ## Plotting
  # Labels
  nom_y <- paste(toupper(substr(nom_vars[num_var],1,1)), substr(nom_vars[num_var],2,nchar(nom_vars[num_var])), sep = "")
  y <- list(title = paste('<b>', nom_y, ' (',unit_var, ')' , '</b>', sep = ""), color = '#282828',
            gridwidth = 2,  tickfont = list(family = 'serif'))
  x <- list(title = '<b>Time</b>', tickfont = list(family = 'serif'), gridwidth = 2)
  title <- list(size = 20, color = '#282828', tickfont = list(family = 'serif'))


  # Plot
  p <- plotly::plot_ly(x = Data$x, y =  Data$y, type = 'scatter', mode = 'lines+markers',
          text = ~paste("<b>", acronyme_var[num_var], ': ', Data$y, unit_var, '</b><br>', Data$x), hoverinfo = 'text')  %>%
    # Legend
    plotly::layout(yaxis = y, xaxis = x,
           title = paste('<b>Tendency of ', nom_vars[num_var],'</b><br><i>', methode_vars[num_var],'</i>' , sep = ""), titlefont = title,
           legend = list(x = 0.80, y = 1.15),
           margin = list(l = 60, r = 50, t = 70, b =  60))

  htmlwidgets::saveWidget(p, "test1var.html", selfcontained = FALSE)
}

##' @title plot2Vars
##'
##' @param nom_var1 (string) name of the first variable to plot
##' @param nom_var2 (string) name of the second variable to plot
##' @param startDate (date format %Y-%m-%d) date from which to plot
##' @param endDate (date format %Y-%m-%d) date to which to plot
##' @param token (string) a token from getToken function
##'
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "138.102.159.36:8080/uesAPI/rest/")
##'  aToken <- getToken("guest@phis.fr","guest")
##'  token <- aToken$data
##'  plot2Vars(nom_var1 = "temperature", nom_var2 = "radiation", token = token)
##'  plot2Vars(nom_var1 = "temperature", nom_var2 = "radiation", startDate = "2017-06-26", endDate = "2017-06-28", token = token)
##' }
##'
##' @return
##' @export
plot2Vars <- function(nom_var1, nom_var2, startDate = NULL, endDate = NULL, token){
  initializeClientConnection(apiID="ws_private", url = "138.102.159.36:8080/uesAPI/rest/")
  ## Data recuperation
  # variable's informations
  var <- getVariables2(token = token)
  nom_vars <- var$data$label
  for (i in 1:length(nom_vars)){
    nom_vars[i] <- strsplit(nom_vars[i], split="_")[[1]][1]
  }
  acronyme_var <- var$data$trait$label

  # Chosen variable
  num_var1 <- match(nom_var1, nom_vars)
  zero <- rep('0', 3-length(num_var1))
  nom_uri_var1 <- paste(paste(zero, collapse=''), num_var1, sep  = "")
  num_var2 <- match(nom_var2, nom_vars)
  zero <- rep('0', 3-length(num_var2))
  nom_uri_var2 <- paste(paste(zero, collapse=''), num_var2, sep  = "")

  # Recuperation of the data from the WS
  envirodata1 <- getEnvironmentData(token=token, variable = paste("http://www.phenome-fppn.fr/ues/id/variables/v", nom_uri_var1, sep = ""))$data
  envirodata2 <- getEnvironmentData(token=token, variable = paste("http://www.phenome-fppn.fr/ues/id/variables/v", nom_uri_var2, sep = ""))$data

  # Values
  y_var1 <- envirodata1$value
  y_var2 <- envirodata2$value
  x_var2 <- as.POSIXct(envirodata1$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  x_var1 <- as.POSIXct(envirodata2$date, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  if(all.equal(x_var1, x_var2)){
    Data <- data.frame(x = x_var1, y1 = y_var1, y2 = y_var2)
  } else {

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
  # Unit
  unit_var1 <- var$data$unit$comment[num_var1]
  unit_var2 <- var$data$unit$comment[num_var2]


  ## Plotting
  # Labels
  nom_y1 <- paste(toupper(substr(nom_vars[num_var1],1,1)), substr(nom_vars[num_var1],2,nchar(nom_vars[num_var1])), sep = "")
  y1 <- list(title = paste('<b>', nom_y1, ' (',unit_var1, ')' , '</b>', sep = ""),showgrid = FALSE,
             tickfont = list(family = 'serif'), side = "left", color = "#FF7733")
  nom_y2 <- paste(toupper(substr(nom_vars[num_var2],1,1)), substr(nom_vars[num_var2],2,nchar(nom_vars[num_var1])), sep = "")
  y2 <- list(title = paste('<b>', nom_y2, ' (',unit_var2, ')' , '</b>', sep = ""), showgrid = FALSE,
             tickfont = list(family = 'serif'), side = "right", color = "#6770f9", overlaying = "y")
  x <- list(title = '<b>Time</b>', tickfont = list(family = 'serif'), gridwidth = 2)
  title <- list(size = 20, color = '#282828', tickfont = list(family = 'serif'))

  # Plot
  p <- plotly::plot_ly(data = Data, type = 'scatter', mode = 'lines+markers') %>%
    # Var 1
    plotly::add_trace(x = ~x, y = ~y1, name = nom_y1, color = I("#FF7733"),
              text = ~paste("<b>", acronyme_var[num_var1], ': ', Data$y1, unit_var1, '</b><br>', Data$x), hoverinfo = 'text')%>%
    # # Var 2
    plotly::add_trace(x = ~x, y = ~y2, name = nom_y2, yaxis = "y2", color = I("#6770f9"),
               text = ~paste("<b>", acronyme_var[num_var2], ': ', Data$y2, unit_var2, '</b><br>', Data$x), hoverinfo = 'text')%>%
    # # Legend
    plotly::layout( xaxis = x, yaxis = y1, yaxis2 = y2,
            title = paste('<b>Tendency of ', nom_vars[num_var1],' and ', nom_vars[num_var2], '</b>', sep = ""), titlefont = title,
            showlegend =FALSE,
            margin = list(l = 60, r = 50, t = 70, b =  60) )
  htmlwidgets::saveWidget(p, "test2vars.html", selfcontained = FALSE)

}

