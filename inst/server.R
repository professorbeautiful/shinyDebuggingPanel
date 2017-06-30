
require(shiny)


shinyServerFunction = function(input, output, session) {

  makeDebuggingPanelOutput(session)
}
shinyServer(shinyServerFunction)
