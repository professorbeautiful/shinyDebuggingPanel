
require(shiny)


shinyServerFunction = function(input, output, session) {

  makeDebuggingPanelOutput(session, toolsInitialState = TRUE)
}
shinyServer(shinyServerFunction)
