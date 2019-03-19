# library(plotly)
# library(phisWSClientR)
# library(variablesStudy)
# library(gam)
# initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
# aToken <- getToken("guest@opensilex.org","guest")
# token <- aToken$data
# vars <- variableList(token = token)
# plotVar(vars$uri[1], token = token)
#
# plotVarDemo( list(vars$uri[2],vars$uri[5]), token = token)
# po = getDF(token = token, varURI = vars$uri[2])
