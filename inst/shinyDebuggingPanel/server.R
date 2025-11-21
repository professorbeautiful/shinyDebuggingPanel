
require(shiny)

print('testing ctrlDpressed')
shinyServerFunction = function(input, output, session) {
  includeScript('www/KeyHandler.js')
  observeEvent(input$ctrlDpressed, {}) # just to flush the ctrl-D press.

  shinyDebuggingPanel::makeDebuggingPanelOutput(
    session, toolsInitialState = FALSE,
    condition='ctrlDpressed === true')


}
shinyServer(shinyServerFunction)



