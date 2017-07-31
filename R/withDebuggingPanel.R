#' withDebuggingPanel
#'
#' For inclusion in your UI.
#'
#'
withDebuggingPanel = function() {
  list(
    #includeScript(paste0(path.package("shinyDebuggingPanel"),
     #                    "/debugPopup.js")),
    includeScript("jquery-ui.js"),  ## (possibly needed)
    uiOutput("debugTools")
  )
}
