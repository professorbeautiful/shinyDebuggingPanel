
require(shiny)


shinyServerFunction = function(input, output, session) {

  makeDebuggingPanelOutput()
}
shinyServer(shinyServerFunction)
