#-------------------------------------------------------------------------------
# Program: getEnvironmentData.R
# Objective: functions called by the user on the web service Phenomeapi
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update:
#-------------------------------------------------------------------------------

##' @title getEnvironmentData
##'
##' @description Retrieves the environmental data from a variable or a sensor
##' @param token a token from getToken function
##' @param variable Search by the uri of a variable
##' @param startDate Search from start date
##' @param endDate Search to end date
##' @param sensor Search by the uri of a sensor
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param dateSortAsc boolean, sort date in ascending order if TRUE
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  vars <- getEnvironmentData(aToken$data,category="imagery",
##'           variable = "http://www.phenome-fppn.fr/ues/id/variables/v005")
##'  vars$data
##' }
##' @export
getEnvironmentData<-function(token,
                             variable = "",
                             startDate = "",
                             endDate = "",
                             sensor = "",
                             page = NULL,
                             pageSize = NULL,
                             dateSortAsc = TRUE,
                             verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE",configWS)

  attributes = list(pageSize=pageSize,
                    page = page)
  if (variable!="")    attributes = c(attributes, variable = variable)
  if (startDate!="")   attributes = c(attributes, startDate = startDate)
  if (endDate!="")     attributes = c(attributes, endDate = endDate)
  if (sensor!="")      attributes = c(attributes, sensor = sensor)

  variableResponse <- getResponseFromWS2(resource=paste0(get("ENVIRONMENTS", configWS)),
                                         attributes = attributes,
                                         verbose = verbose,
                                         authorization=token)
  return(variableResponse)
}
#-------------------------------------------------------------------------------
# Program: wsFunctions.R
# Objective: functions called by the user on the web service Phenomeapi
# Author: A. Charleroy
# Creation: 12/08/2016
# Update: 29/10/2018 (by I.Sanchez) - 30/10/2016 (by  A. Charleroy)
#-------------------------------------------------------------------------------

##' @title retrieves a user identifier for connexion to the web service
##'
##' @description Retrieves a user identifier for connexion to the WebService (WS)
##' @param login login of the user to create the token
##' @param password password of the user to create the token
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return a session token user identifier in the WS
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##' aToken <- getToken("guestphis@supagro.inra.fr","guestphis")
##' aToken$data
##' }
##' @export
getToken<-function(login,password,verbose=FALSE){
  attributes<-list(username = login, password = password)

  # Try 1 on first web service
  tokenResp1<-getTokenResponseWS(resource = get("TOKEN",configWS), attributes = attributes,verbose=verbose)

  # Try 2 on second web service
  tokenResp2<-getTokenResponseWS2(resource = get("BRAPITOKEN",configWS), attributes = attributes,verbose=verbose)

  # Test which WS is OK
  if (tokenResp1$status_code >= 200 && tokenResp1$status_code < 300 && tokenResp2$status_code >=400){
    json = jsonlite::fromJSON(httr::content(tokenResp1, as = "text", encoding = "UTF-8"))
    response <- list(
      currentPage = json$metadata$pagination$currentPage,
      totalCount = json$metadata$pagination$totalCount,
      totalPages = json$metadata$pagination$totalPages,
      codeHttp = tokenResp1$status_code,
      codeHttpMessage = "Query executed and data recovered - WS1",
      codeStatusMessage = json$metadata$status,
      data = json$session_token,
      ws="WS1")
    print("Query executed and data recovered - WS1")
  } else if (tokenResp2$status_code >= 200 && tokenResp2$status_code < 300 && tokenResp1$status_code >=400){
    json = jsonlite::fromJSON(httr::content(tokenResp2, as = "text", encoding = "UTF-8"))
    response <- list(
      #currentPage = json$metadata$pagination$currentPage,
      #totalCount = json$metadata$pagination$totalCount,
      #totalPages = json$metadata$pagination$totalPages,
      codeHttp = tokenResp2$status_code,
      codeHttpMessage = "Query executed and data recovered - WS2",
      codeStatusMessage = json$metadata$status,
      data = json$access_token,
      ws="WS2")

    print("Query executed and data recovered - WS2")
  } else if(tokenResp1$status_code == 500 || tokenResp2$status_code == 500){
    print("WebService internal error")
  } else if(tokenResp1$status_code == 401 || tokenResp2$status_code == 401){
    print("User not authorized")
  } else if(tokenResp1$status_code == 404 || tokenResp2$status_code == 404){
    print("Not found")
  } else if((tokenResp1$status_code >= 400 && tokenResp1$status_code != 401 &&
             tokenResp1$status_code != 404 && tokenResp1$status_code < 500) &&
            (tokenResp2$status_code >= 400 && tokenResp2$status_code != 401 &&
             tokenResp2$status_code != 404 && tokenResp2$status_code < 500)){
    print("Bad user request")
  }

  if (tokenResp1$status_code > 250 && tokenResp2$status_code > 250){
    print("No web service available! Check your login/password and/or your url...")
  }

  # define class S3 and return the list if exists
  if (exists("response")){
    class(response) <- append(class(response),"WSResponse")
    return(response)
  }
}

##' @title retrieves the list of projects from the web service
##'
##' @description Retrieves the list of projects in the WS
##' @param token a token
##' @param projectName Name of the project to search
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_public")
##'  accesToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  getProjects(accesToken$data)
##'  getProjects(accesToken$data, page = 1)
##'  getProjects(accesToken$data, page = 3, pageSize = 100)
##'  getProjects(accesToken$data, projectName = "PHIS_Publi")
##' }
##' @export
getProjects<-function(token, projectName = "",page=NULL,pageSize=NULL,verbose=FALSE){
  if(is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)

  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (projectName != ""){
    attributes <- c(attributes, projectName = projectName)
  }
  projectResponse<-getResponseFromWS(resource = get("PROJECTS",configWS),attributes=attributes,verbose=verbose)
  return(projectResponse)
}

##' @title getVariablesByCategory
##'
##' @description Retrieves the variable by categories (environment or setpoint...)
##' @param token a token
##' @param category Name of the category to search
##' @param imageryProvider character, provider of the images
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  vars <- getVariablesByCategory(aToken$data,category="imagery",
##'           experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  vars$data
##' }
##' @export
getVariablesByCategory<-function(token,category ="",experimentURI ="",imageryProvider="",
                                 page=NULL,pageSize=NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId=token, page=page, pageSize = pageSize)
  if (category  == ""){
    stop("no category selected")
  } else {
    if (experimentURI != ""){
      attributes <- c(attributes, experimentURI = experimentURI)
    }
    if (imageryProvider != ""){
      attributes <- c(attributes, imageryProvider = imageryProvider)
    }
    variableResponse <- getResponseFromWS(resource=paste0(get("VARIABLES",configWS),"/category/",category),
                                          attributes = attributes,verbose=verbose)
    return(variableResponse)
  }
}


##' @title retrieves the environmental mesures of an experiment from the web service
##'
##' @description Retrieves environmental mesures of an experiment or by dates
##' @param token a token
##' @param variableCategory character, a category of variables
##' @param startDate data > startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param endDate data < startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param variables list of variables for the request (Ex : "wind speed_weather station_meter per second")
##' @param facility place of the experiment (Ex : "http://www.phenome-fppn.fr/m3p/ec3")
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  getEnvironment(aToken$data,page=3,pageSize=100,startDate="2012-02-21",endDate = "2012-03-21")
##'  test<-getEnvironment(aToken$data,
##'        experimentURI="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  test$data
##'  getEnvironment(aToken$data,experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01",
##'  startDate="2012-02-21",endDate="2012-02-15 19:20:30")
##'  getEnvironment(aToken$data,experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01",
##'     facility="http://www.phenome-fppn.fr/m3p/ec3",
##'     variables="wind speed_weather station_meter per second")
##' }
##' @export
getEnvironment <- function(token ,variableCategory ="",startDate = "",endDate = "" ,variables = "",facility = "",
                           experimentURI ="", page = NULL, pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)

  attributes = list(sessionId = token, page = page, pageSize=pageSize)
  if (startDate != ""){
    attributes <- c(attributes, startDate = startDate)
  }
  if (endDate != ""){
    attributes <- c(attributes, endDate = endDate)
  }
  if (facility != ""){
    attributes <- c(attributes, facility = facility)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }
  if (variableCategory != ""){
    attributes <- c(attributes, variableCategory = variableCategory)
  }
  if (variables != ""){
    attributes <- c(attributes, variables = utils::URLencode(variables))
  }
  environmentResponse <- getResponseFromWS(resource = get("ENVIRONMENT",configWS),
                                           attributes = attributes,verbose=verbose)
  return(environmentResponse)
}

##' @title getExperimentById
##'
##' @description retrieves the informations for one experiment
##' @param token a token
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  publicExp<-getExperimentById(aToken$data,
##'         experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  publicExp$data
##' }
##' @keywords internal
getExperimentById <- function(token, experimentURI ="", page = NULL,pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (experimentURI  == ""){
    stop("no experimentURI selected")
  } else {
    # IS 03/11/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    expUrlEncoded<-paste0(utils::URLencode(experimentURI,  reserved = TRUE),"/details")
    experimentResponse<-getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                          paramPath=expUrlEncoded,attributes=attributes,verbose=verbose)
    return(experimentResponse)
  }
}

##' @title retrieves the experiments from the web service
##'
##' @description Retrieves the available experiments and/or linked to a project
##' @param token a token
##' @param projectName  project name
##' @param season character, a year when the experiment was conducted
##' @param sortOrder ordering "ASC" or "DESC"
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")$data
##'  getExperiments(aToken,page=3,pageSize=100,startDate="2012-02-21",endDate="2012-03-21")
##'  getExperiments(aToken,projectName = "PHIS_Publi")
##'  getExperiments(aToken,sortOrder = "ASC")
##'  getExperiments(aToken,season = 2012 )
##' }
##' @export
getExperiments <- function(token, projectName ="", season = "", sortOrder = "DESC" ,
                           page = NULL,pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)

  if (projectName != ""){
    attributes <- c(attributes, projectName = projectName)
  }
  if (season != ""){
    attributes <- c(attributes, season = season)
  }
  if (sortOrder != ""){
    attributes <- c(attributes, sortOrder = sortOrder)
  }
  experimentResponse<-getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                        attributes = attributes,verbose=verbose)
  return(experimentResponse)
}

##' @title retrieves the context of plant linked to an experiment from the web service
##'
##' @description Retrieves context of plant linked to an experiment
##' @param token a token
##' @param plantAlias an alias of plant
##' @param experimentURI URI of the experiment
##' @param germplasmURI filter by genotype
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken<-getToken("guestphis@supagro.inra.fr","guestphis")$data
##'  plantes<-getPlants(aToken,experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##' }
##' @export
getPlants <- function(token, plantAlias ="", experimentURI = "", germplasmURI = "" ,
                      page = NULL,pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)

  if (plantAlias != ""){
    attributes <- c(attributes, plantAlias = plantAlias)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }
  if (germplasmURI != ""){
    attributes <- c(attributes, germplasmURI = germplasmURI)
  }
  plantsResponse<-getResponseFromWS(resource = get("PLANTS",configWS),attributes = attributes,verbose=verbose)
  return(plantsResponse)
}

##' @title getPlantsContextByID
##'
##' @description Retrieves the contect of a plant
##' @param token a token
##' @param plantURI character, a URI identifier of a plant
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' # not run (is an internal function!!!)
##' # aToken<-getToken("guestphis@supagro.inra.fr","guestphis")$data
##' # test<-getPlantsContextByID(aToken,plantURI="http://www.phenome-fppn.fr/m3p/arch/2011/c11005809",
##' #       ,experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##' # test$data
##' @keywords internal
getPlantsContextByID<-function(token, plantURI ="",experimentURI="",page = NULL,
                               pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }

  if (plantURI  == ""){
    stop("no plantURI selected")
  } else {
    # AC 28/10/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    plantURIEncoded = utils::URLencode(plantURI,reserved = TRUE)
    plantByIDResponse<-getResponseFromWS(resource = get("PLANTS",configWS),
                                         paramPath=plantURIEncoded,attributes=attributes,verbose=verbose)
    return(plantByIDResponse)
  }
}


##' @title retrieves the environmental mesures of a plant from the web service
##'
##' @description Retrieves environmental mesures of a plant or by dates
##' @param token a token
##' @param plantURI plant URI
##' @param variableCategory character, a category of variables
##' @param startDate data > startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param endDate data < startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param variables list of variables for the request (Ex: "wind speed_weather station_meter per second")
##' @param facility place of the experiment (Ex: "http://www.phenome-fppn.fr/m3p/ec3")
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @examples
##' # not run (is an internal function!!!)
##' # aToken<-getToken("guestphis@supagro.inra.fr","guestphis")$data
##' # myplant<-getPlantEnvironment(aToken,
##' #       plantURI="http://www.phenome-fppn.fr/m3p/arch/2011/c11005809",
##' #       experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##' # myplant$data
##' @keywords internal
getPlantEnvironment <- function(token,plantURI ="",variableCategory ="",startDate = "",endDate = "",
                                variables = "",facility = "", experimentURI ="",
                                page = NULL,pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (plantURI  == ""){
    stop("no plantURI given")
  } else {
    attributes = list(sessionId = token, page = page, pageSize = pageSize)
    if (startDate != ""){
      attributes <- c(attributes, startDate = startDate)
    }
    if (endDate != ""){
      attributes <- c(attributes, endDate = endDate)
    }
    if (facility != ""){
      attributes <- c(attributes, facility = facility)
    }
    if (experimentURI != ""){
      attributes <- c(attributes, experimentURI = experimentURI)
    }else{
      stop("no experimentURI given")
    }
    if (variableCategory != ""){
      attributes <- c(attributes, variableCategory = variableCategory)
    }
    if (variables != ""){
      attributes <- c(attributes, variables = utils::URLencode(variables))
    }
    # AC 28/10/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    plantURIEncoded =  utils::URLencode(plantURI, reserved = TRUE)
    plantEnvironmentResponse<-getResponseFromWS(resource = get("PLANTS",configWS),
                                                paramPath = paste0(plantURIEncoded,"/environment"),
                                                attributes =  attributes,verbose=verbose)
    return(plantEnvironmentResponse)
  }
}

##' @title retrieves the images analysis data from the web service
##'
##' @description Retrieves data from image analysis
##' @param token a token
##' @param experimentURI URI of the experiment
##' @param variablesName list, variable names of images analysis (ex : "objAreaSum")
##' @param labelView character, label view of an image
##' @param provider character, provider of data
##' @param date character, data for one day (format: YYYY-MM-DD)
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  myImages<-getImagesAnalysis(token = aToken$data,
##'            experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01",
##'            variablesName = list("objAreaSum"),pageSize = 100000,verbose=FALSE)
##'  head(myImages$data)
##' }
##' @export
getImagesAnalysis <- function(token, experimentURI ="", variablesName = list(),
                              labelView ="", provider = "", date = "",
                              page = NULL,pageSize = NULL,verbose=FALSE){

  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (date != ""){
    attributes <- c(attributes, date = date)
  }
  if (is.list(variablesName)){
    if (length(variablesName) != 0){
      attributes <- c(attributes, variablesName = paste(variablesName, collapse = ","))
    }
  } else {
    stop("variablesName is not a list")
  }
  if (labelView != ""){
    attributes <- c(attributes, labelView = labelView)
  }
  if (provider != ""){
    attributes <- c(attributes, provider = provider)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }
  imagesAnalysisResponse <- getResponseFromWS(resource = get("IMAGESANALYSIS",configWS),
                                              attributes = attributes,verbose=verbose)
  return(imagesAnalysisResponse)
}


##' @title retrieves the irrigation data from the web service
##'
##' @description Retrieves irrigation data
##' @param token a token
##' @param experimentURI URI of the experiment
##' @param variablesName list, variable names of images analysis
##' @param provider character, provider of data
##' @param date character, data for one day (format: YYYY-MM-DD)
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  accesToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  mywater<-getWatering(token=accesToken$data,
##'          experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01",
##'          variablesName = list("weightBefore"),pageSize=100000,verbose=FALSE)
##'  head(mywater$data)
##' }
##' @export
getWatering <- function(token, experimentURI ="", variablesName = list(), provider = "", date = "",
                        page = NULL,pageSize = NULL,verbose=FALSE){

  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (date != ""){
    attributes <- c(attributes, date = date)
  }
  if (is.list(variablesName)){
    if (length(variablesName) != 0){
      attributes <- c(attributes, variablesName = paste(variablesName, collapse = ","))
    }
  } else {
    stop("variablesName is not a list")
  }

  if (provider != ""){
    attributes <- c(attributes, provider = provider)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }

  wateringResponse <- getResponseFromWS(resource = get("WATERING",configWS),
                                        attributes = attributes,verbose=verbose)
  return(wateringResponse)
}

# ##' @title postPhenotypes
# ##'
# ##' @description Post phenotypes data
# ##' @details !!!in development!!!
# ##' @param token a token
# ##' @param experimentURI URI of the experiment
# ##' @param data list of dataframe
# ##'  $ data :'data.frame':  1 obs. of  5 variables:
# ##' .. ..$ plantURI      : chr "http://www.phenome-fppn.fr/m3p/arch/2016/c16001681"
# ##' .. ..$ codeVariableId: chr "leafArea_unspecified_square meter"
# ##' .. ..$ date          : chr "2016-05-18 04:30:10"
# ##' .. ..$ confidence    : logi NA
# ##' .. ..$ value         : int 0'
# ##' @seealso http://147.99.7.5:8080/phenomeapi/api-docs/#!/environment/postPhenotypes
# ##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
# ##' @details You have to execute the getToken() function first to have access to the web
# ##' service
# ##' @examples
# ##' # Not run (is an internal function)
# ##' @keywords internal
# postPhenotypes <- function(token, experimentURI = "", data = NULL, reportId = "", verbose=FALSE){
#
#   attributes = list(sessionId = token)
#   if ( is.null(data)){
#     stop("data attribute must be filled")
#   }
#   if (experimentURI == ""){
#     stop("experimentURI must be given")
#   }
#   if (reportId == ""){
#     stop("reportId must be given")
#   }
#
#   configuration<-list(reportId = reportId)
#   phenotypesList<-list(experimentURI = experimentURI,
#                         configuration = configuration,
#                         data = data)
#
#   # str(phenotypesList)
#   postPhenotypeResponse <- postResponseFromWS(resource = get("PHENOTYPES",configWS),
#                                               attributes = attributes, requestBody = list(phenotypesList),
#                                               verbose=verbose)
#   return(postPhenotypeResponse)
# }
#
#
##' @title getLabelViewByExperimentById
##'
##' @description Retrieves LabelViews used in a specific experiment
##' @param token a token
##' @param viewType type of view, top or side
##' @param cameraAngle numeric, angle of the camera
##' @param provider origin of the data
##' @param experimentURI experiment unique identifier
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' # Not run (is an internal function)
##' # aToken = getToken("guestphis@supagro.inra.fr","guestphis")$data
##' # publicLabelView <- getLabelViewByExperimentById(aToken,
##' #      experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##' # publicLabelView$data
##' @keywords internal
getLabelViewByExperimentById <- function(token ,experimentURI="" ,viewType="" ,cameraAngle="",provider="",
                                         page = NULL,pageSize = NULL,verbose=FALSE){

  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)

  if(experimentURI  == ""){
    stop("no experimentURI selected")
  } else {
    if (viewType != ""){
      attributes <- c(attributes, viewType = viewType)
    }
    if (cameraAngle != ""){
      attributes <- c(attributes, cameraAngle = cameraAngle)
    }
    if (provider != ""){
      attributes <- c(attributes, provider = provider)
    }
    # AC 28/10/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    expUrlEncoded<-paste0(utils::URLencode(experimentURI, reserved = TRUE),"/labelViews")
    experimentLabelViewsResponse <- getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                                      paramPath = expUrlEncoded, attributes =  attributes)

    return(experimentLabelViewsResponse)
  }
}
#-------------------------------------------------------------------------------
# Program: wsFunctions.R
# Objective: functions to facilitate requests on web service Phenomeapi
# Author: A. Charleroy
# Creation: 19/03/2018
# Update: 03/07/2018 by I.Sanchez
#-------------------------------------------------------------------------------


##' @title initializeClientConnection
##' @param apiID character, a character name of an API ("ws_public" or "ws_private")
##' @param url character, if apiID is private add the url of the chosen API, containing the IP,
##'            the port and the path of the WS
##'
##' @description load name space and connexion parameters of the webservice.
##' Execute only once at the beginning of the requests.
##' In the case of a WebService change of address or a renaming of services, please edit this list.
##' and execute the function.
##' @export
initializeClientConnection<-function(apiID,url = ""){
  # if apiID is public then we use the public configWS given by the package
  # else if apiID is private, we use the url procided by the user
  if (apiID == "ws_private") {
    if(url != ""){
      # configWS is an environment with specific variables to phenomeapi web service
      assign("BASE_PATH",paste0("http://",url),configWS)
    } else {
      print("Please, you have to give an URL and port address")
    }
  } else if (apiID == "ws_public") {
    assign("BASE_PATH",get("PUBLIC_PATH",configWS),configWS)
  }
}

##' @title getTokenResponseWS
##'
##' @description Create a token to call the webservice for authentication and
##' returns a formatted response of WSResponse class.
##' @param resource character, an resource from the web service api
##' @param attributes a list containing a login and a password
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @details This function is OK for the first version of the web service
##'  (a GET call with a visible request)
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @return responseObject an object HTTP httr
##' @keywords internal
getTokenResponseWS<-function(resource,paramPath=NULL,attributes,type = "application/json",verbose=FALSE){
  webserviceBaseUrl <- get("BASE_PATH",configWS)
  urlParams <- ""
  # create the URL
  for (attribut in names(attributes)) {
    if(urlParams != ""){
      urlParams <- paste0(urlParams,"&")
    }
    if(is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],  reserved = TRUE))
    } else {
      urlParams <- paste0(urlParams,attribut,"=",attributes[[attribut]])
    }
  }
  if(is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
  }

  ptm <- proc.time()
  r <- httr::GET(finalurl)
  if (verbose) {
    print("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  }

  return(r)
}


##' @title getTokenResponseWS2
##'
##' @description Create a token to call the webservice for authentication and
##' returns a formatted response of WSResponse class.
##' @param resource character, an resource from the web service api
##' @param attributes a list containing a login and a password
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @details This function is OK for the second version of the web service
##'  (a POST call with an invisible request using a correct JSON list in a body)
##' @seealso https://brapi.docs.apiary.io/#introduction/structure-of-the-response-object
##' @return responseObject an object HTTP httr
##' @importFrom openssl md5
##' @keywords internal
getTokenResponseWS2<-function(resource,attributes,type = "application/json",verbose=FALSE){
  # create the URL
  #finalurl <- paste0(get("BASE_PATH",configWS),"brapi/v1/token")
  finalurl <- paste0(get("BASE_PATH",configWS),resource)

  # Create the body JSON list with the attributes
  # take care that httr::POST function doesn't allow to md5 object
  # I had to convert the md5 object into a string one with the toString() function
  finalbody<-list(grant_type="password",
                  username= attributes[[1]],
                  password=toString(md5(attributes[[2]])))

  # call
  ptm <- proc.time()
  r <- httr::POST(url=finalurl,body = finalbody,encode="json")
  if (verbose) {
    print("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  }

  # if (r$status_code >= 500){
  #   print("WebService internal error")
  # }
  # if (r$status_code == 401){
  #   print("User not authorized")
  # }
  # if (r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
  #   print("Bad user request")
  # }
  # if (r$status_code >= 200 && r$status_code < 300){
  #   print("Query executed and data recovered")
  # }
  return(r)
}



##' @title getResponseFromWS
##'
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @param responseObject object HTTP httr
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @keywords internal
getResponseFromWS<-function(resource,paramPath = NULL,attributes,type="application/json",verbose=FALSE){
  webserviceBaseUrl <- get("BASE_PATH",configWS)
  urlParams <- ""
  # creation de l'url
  for (attribut in names(attributes)) {
    if (urlParams != ""){
      urlParams <- paste0(urlParams,"&")
    }
    #     chaines de caractere
    if (is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],reserved = TRUE))
      #       nombres
    } else if (is.numeric(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",format(attributes[[attribut]], scientific=FALSE))
      # autres
    } else {
      urlParams <- paste0(urlParams,attribut,"=",attributes[[attribut]])
    }
  }
  if (is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
  }

  ptm <- proc.time()
  r <- httr::GET(finalurl)
  if (verbose) {
    print("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  }

  if(r$status_code >= 500){
    print("WebService internal error")
  }
  if(r$status_code == 401){
    print("User not authorized")
  }
  if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
    print("Bad user request")
  }
  if(r$status_code >= 200 && r$status_code < 300){
    print("Query executed and data recovered")
  }
  return(getDataAndShowStatus(r))
}

#' @title getResponseFromWS2
#'
#' @description Create an URL to call the WS2 and retrun a formatted response of WSResponse class.
#' @param responseObject object HTTP httr
#' @param verbose logical FALSE by default, if TRUE display information about the progress
#' @keywords internal
getResponseFromWS2<-function(resource, paramPath = NULL, attributes, authorization, type="application/json", verbose=FALSE){
  webserviceBaseUrl <- get("BASE_PATH",configWS)
  urlParams <- ""
  # creation de l'url
  for (attribut in names(attributes)) {
    if (urlParams != ""){
      urlParams <- paste0(urlParams,"&")
    }
    #     chaines de caractere
    if (is.character(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],reserved = TRUE))
      #       nombres
    } else if (is.numeric(attributes[[attribut]])){
      urlParams <- paste0(urlParams,attribut,"=",format(attributes[[attribut]], scientific=FALSE))
      # autres
    } else {
      urlParams <- paste0(urlParams,attribut,"=",attributes[[attribut]])
    }
  }
  if (is.null(paramPath)){
    finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
  } else {
    finalurl <- paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
  }

  ptm <- proc.time()
  r <- httr::GET(finalurl, httr::add_headers(Authorization = paste("Bearer",authorization, sep = " ")))
  if (verbose) {
    print("Request Time : " )
    print(proc.time() - ptm)
    print(r)
  }

  if(r$status_code >= 500){
    print("WebService internal error")
  }
  if(r$status_code == 401){
    print("User not authorized")
  }
  if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
    print("Bad user request")
  }
  if(r$status_code >= 200 && r$status_code < 300){
    print("Query executed and data recovered")
  }
  return(getDataAndShowStatus(r))
}

# ##' @title postResponseFromWS
# ##'
# ##' @description Create an URL to call the WS and return a formatted response of WSResponse class.
# ##' @param resource character, the name of the webservice resource
# ##' @param paramPath character, path URL encoded parameter
# ##' @param attributes query parameters
# ##' @param encode character, type of encodage
# ##' @param requestBody body data which will be send
# ##' @param verbose logical FALSE by default, if TRUE display information about the progress
# ##' @return WSResponse WSResponse class instance
# ##' @keywords internal
# postResponseFromWS<-function(resource, paramPath = NULL, attributes,  encode ="json", requestBody, verbose=FALSE){
#   #configWS<-initializeClientConnection()
#   webserviceBaseUrl <- configWS[["BASE_PATH"]]
#   urlParams = ""
#   # create the l'url
#   for (attribut in names(attributes)) {
#     if (urlParams != ""){
#       urlParams = paste0(urlParams,"&")
#     }
#     #     chaines de caractere
#     if (is.character(attributes[[attribut]])){
#       urlParams = paste0(urlParams,attribut,"=",utils::URLencode(attributes[[attribut]],reserved = TRUE))
#       #       nombres
#     } else if (is.numeric(attributes[[attribut]])){
#       urlParams = paste0(urlParams,attribut,"=",format(attributes[[attribut]], scientific=FALSE))
#       # autres
#     } else {
#       urlParams = paste0(urlParams,attribut,"=",attributes[[attribut]])
#     }
#   }
#   if (is.null(paramPath)){
#     finalurl = paste0(webserviceBaseUrl, resource , "?", urlParams)
#   } else {
#     finalurl = paste0(webserviceBaseUrl, resource ,"/",paramPath, "?", urlParams)
#   }
#
#   ptm <- proc.time()
#   r <- httr::POST(finalurl, body = jsonlite::toJSON(requestBody,auto_unbox = TRUE))
#   if (verbose) {
#     print("Request Time : " )
#     print(proc.time() - ptm)
#     print(r)
#   }
#
#   if(r$status_code >= 500){
#     print("WebService internal error")
#   }
#   if(r$status_code == 401){
#     print("User not authorized")
#   }
#   if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
#     print("Bad user request")
#   }
#   if(r$status_code >= 200 && r$status_code < 300){
#     print("Query executed and data recovered")
#   }
#   return(getDataAndShowStatus(r))
# }

##' @title getDataAndShowStatus
##'
##' @description Recupere les status et les informations presentes dans les entetes de reponse HTTP
##'  ainsi que dans la partie metadata de la reponse
##' @param responseObject objet de reponse HTTP httr
##' @keywords internal
getDataAndShowStatus<-function(responseObject){
  status = NULL
  json = jsonlite::fromJSON(httr::content(responseObject, as = "text", encoding = "UTF-8"))
  if (responseObject$status_code >= 400){
    if (!is.null(json$metadata$status) && length(json$metadata$status) > 0){
      print("Additional Request information :")
      print(json$metadata$status)
      status = json$metadata$status
    }
    if(responseObject$status_code >= 500){
      msg = "WebService internal error"
    }
    if(responseObject$status_code == 401){
      msg = "User not authorized"
    }
    if(responseObject$status_code >= 400 && responseObject$status_code != 401 &&  responseObject$status_code < 500){
      msg = "Bad user request"
    }
    response <- list(
      currentPage = NULL,
      totalCount = NULL,
      totalPages = NULL,
      codeHttp = responseObject$status_code,
      codeHttpMessage = msg,
      codeStatusMessage = status,
      data = NULL
    )
  } else {
    if (!is.null(json$metadata$status) && length(json$metadata$status) > 0){
      print("Additional Request information :")
      print(json$metadata$status)
      status = json$metadata$status
    }
    if (responseObject$status_code >= 200 && responseObject$status_code < 300){
      msg = "Query executed and data recovered"
    }
    response <- list(
      currentPage = json$metadata$pagination$currentPage,
      totalCount = json$metadata$pagination$totalCount,
      totalPages = json$metadata$pagination$totalPages,
      codeHttp = responseObject$status_code,
      codeHttpMessage = msg,
      codeStatusMessage = status,
      data = json$result$data
    )
  }
  class(response) <- append(class(response),"WSResponse")
  return(response)
}

##' @title ObjectType
##' @param obj an object
##' @description Returns the type of object received by R Development function
##' @return string
##' @importFrom utils str
##' @keywords internal
ObjectType<-function(obj){
  return(utils::capture.output(str(obj)))
}
#-------------------------------------------------------------------------------
# Program: getResponseFromWS2.R
# Objective: functions called by the user on the web service Phenomeapi
# Authors: Hollebecq Jean-Eudes
#          Chourrout Elise
# Creation: 21/01/2019
# Update:
#-------------------------------------------------------------------------------

##' @title Retrieves the data of a service from the WS2
##'
##' @description Create an URL to call the WS and retrun a formatted response of WSResponse class.
##' @param ressource the name of the service to call
##' @param paramPath the extension of the service to call, default to NULL
##' @param attributes The list of attributes to give to the GET request
##' @param type The type of the output, default to application/json
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @keywords internal
#
# getResponseFromWS2<-function(resource, paramPath = NULL, attributes, type = "application/json", verbose = FALSE){
#   webserviceBaseUrl <- get("BASE_PATH", configWS)
#   urlParams <- ""
#   # url concatenation
#   for (attribut in names(attributes)) {
#     if (urlParams != ""){
#       urlParams <- paste0(urlParams, "&")
#     }
#     #     character arguments
#     if (is.character(attributes[[attribut]])){
#       urlParams <- paste0(urlParams, attribut, "=", utils::URLencode(attributes[[attribut]], reserved = TRUE))
#       #   numeric arguments
#     } else if (is.numeric(attributes[[attribut]])){
#       urlParams <- paste0(urlParams, attribut, "=", format(attributes[[attribut]], scientific = FALSE))
#       #   other arguments
#     } else {
#       urlParams <- paste0(urlParams, attribut, "=", attributes[[attribut]])
#     }
#   }
#   if (is.null(paramPath)){
#     finalurl <- paste0(webserviceBaseUrl, resource , "?", urlParams)
#   } else {
#     finalurl <- paste0(webserviceBaseUrl, resource , "/", paramPath, "?", urlParams)
#   }
#   print(finalurl)
#   ptm <- proc.time()
#   r <- httr::GET(finalurl, config = httr::add_headers(Authorization=paste("Bearer " ,attributes$Authorization, sep = "")))
#   if (verbose) {
#     print("Request Time : " )
#     print(proc.time() - ptm)
#     print(r)
#   }
#
#   if(r$status_code >= 500){
#     print("WebService internal error")
#   }
#   if(r$status_code == 401){
#     print("User not authorized")
#   }
#   if(r$status_code >= 400 && r$status_code != 401 &&  r$status_code < 500){
#     print("Bad user request")
#   }
#   if(r$status_code >= 200 && r$status_code < 300){
#     print("Query executed and data recovered")
#   }
#   return(getDataAndShowStatus(r))
# }
#-------------------------------------------------------------------------------
# Program: getVariables2.R
# Objective: functions called by the user on the web service Phenomeapi
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update:
#-------------------------------------------------------------------------------

##' @title getVariables
##'
##' @description Retrieves the variable descriptions, trait, method and unit covered by the variable
##' @param token a token from getToken function
##' @param uri Search by the uri of an experiment
##' @param label Search by label
##' @param trait Search by trait uri
##' @param method Search by method uri
##' @param unit Search variables by unit uri
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  vars <- getVariablesByCategory(aToken$data,category="imagery",
##'           experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  vars$data
##' }
##' @export
getVariables2<-function(token,
                        uri = "",
                        label = "",
                        trait = "",
                        method = "",
                        unit = "",
                        pageSize = NULL,
                        page = NULL,
                        verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE",configWS)

  attributes = list(pageSize=pageSize,
                    page = page)
  if (uri!="")    attributes = c(attributes, uri = uri)
  if (label!="")  attributes = c(attributes, lavel = label)
  if (trait!="")  attributes = c(attributes, trait = trait)
  if (method!="") attributes = c(attributes, method = method)
  if (unit!="")   attributes = c(attributes, unit = unit)

  variableResponse <- getResponseFromWS2(resource=paste0(get("VARIABLES", configWS)),
                                         attributes = attributes,
                                         verbose = verbose,
                                         authorization = token)
  return(variableResponse)
}
configWS<-new.env(emptyenv())

assign("PUBLIC_PATH","http://147.100.179.156:8080/phenomeapi/resources/", configWS)
assign("BASE_PATH","", configWS)

# WS phis1
assign("TOKEN", "token", configWS)
assign("EXPERIMENT", "experiments", configWS)
assign("VARIABLES", "variables", configWS)
assign("PLANTS", "plants", configWS)
assign("IMAGESANALYSIS", "imagesAnalysis", configWS)
assign("PHENOTYPES", "phenotypes", configWS)
assign("WATERING", "watering", configWS)

# WS phis2
assign("BRAPITOKEN", "brapi/v1/token", configWS)
assign("AGROOBJECTS", "agronomicalObjects", configWS)
assign("DATASETS", "datasets", configWS)
assign("ANNOTATIONS", "annotations", configWS)
assign("ENVIRONMENTS", "environments", configWS)

# commun
assign("VARIABLES", "variables", configWS)
assign("ENVIRONMENT", "environment", configWS)
assign("PROJECTS", "projects", configWS)
assign("DEFAULT_PAGE", 0, configWS)
assign("DEFAULT_PAGESIZE", 100, configWS)
