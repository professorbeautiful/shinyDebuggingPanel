#' testDebuggingPanel
#'
#' testDebuggingPanel() to test this package.
#'
testDebuggingPanel = function() {
  require(shiny)
  runApp(system.file(package = "shinyDebuggingPanel",
                     "shinyDebuggingPanel"))
}
