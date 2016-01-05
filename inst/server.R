
require(shiny)


shinyServerFunction = function(input, output, session) {

  thisSession <<- session
  makeDebuggingPanelOutput(thisSession)
}
shinyServer(shinyServerFunction)
