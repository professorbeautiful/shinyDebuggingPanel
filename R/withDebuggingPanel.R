#' withDebuggingPanel
#'
#' For inclusion in your UI.
#'
#'
withDebuggingPanel = function() {
  list(
    #includeScript(paste0(path.package("shinyDebuggingPanel"),
     #                    "/debugPopup.js")), doesnt work at shinyapps.io
    #includeScript("jquery-ui.js"),  ## automatic if in www/js
    ### note: debugPopup.js is not used.
    uiOutput("debugTools")
  )
}
