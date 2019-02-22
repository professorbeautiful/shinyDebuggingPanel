
require(shiny)


shinyServerFunction = function(input, output, session) {

  makeDebuggingPanelOutput(session, toolsInitialState = TRUE,
                           condition='true')
  includeScript('www/KeyHandler.js')
}
shinyServer(shinyServerFunction)


