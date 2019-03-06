# library(plotly)
# library(phisWSClientR)
# library(variablesStudy)
# library(gam)
# initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
# aToken <- getToken("guest@opensilex.org","guest")
# token <- aToken$data
# vars <- listVariables(token = token)
# plotVar(vars$value[1], token = token)
#
# plotVarRel( vars$value[1],
#             vars$value[2],
#             token = token,
#             trend = TRUE)
