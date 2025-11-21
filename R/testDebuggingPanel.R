#' testDebuggingPanel
#'
#' testDebuggingPanel() to test this package.
#'
testDebuggingPanel = function() {
  require(shiny)
  #run('inst/shinyDebuggingPanel/testDebuggingPanel.Rmd')
  run(system.file(package = "shinyDebuggingPanel", "shinyDebuggingPanel",
                     "testDebuggingPanel.Rmd"))
}
