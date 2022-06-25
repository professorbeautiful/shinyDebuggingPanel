
require(shiny)

print('testing ctrlDpressed')
shinyServerFunction = function(input, output, session) {
  observeEvent(input$ctrlDpressed, {}) # just to flush the ctrl-D press.

  shinyDebuggingPanel::makeDebuggingPanelOutput(
    session, toolsInitialState = TRUE,
    condition='ctrlDpressed === true')

  includeScript('www/KeyHandler.js')
}
shinyServer(shinyServerFunction)


