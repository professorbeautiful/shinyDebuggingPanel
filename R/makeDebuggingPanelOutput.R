#' makeDebuggingPanelOutput
#'
#' Place shinyDebuggingPanel::makeDebuggingPanelOutput(session) inside your call to shinyServer(server.R). Place shinyDebuggingPanel::withDebuggingPanel() in your ui.R.
#'
#' @param session  The shiny session; default is  shiny::getDefaultReactiveDomain()s
#'
#' @details  Make sure your server function has the third argument:  function(input, output, session)

## We begin with some convenient assignments and function.

makeDebuggingPanelOutput = function(
  session=NULL,
  toolsInitialState=FALSE,
  condition='true') {
  #thisSession <<- shiny::getDefaultReactiveDomain()
  # either is OK, but the ifelse fails, both with is.null() and  missing().
  toolsInitialState <<- toolsInitialState
  theShowDebuggerCondition <<- condition
  if(is.null(session))
    thisSession <<- shiny::getDefaultReactiveDomain()
  else thisSession <<- session
  debugToolsExpression = expression(
    {
      rValuesDebugging = reactiveValues()
      wasClicked =  function(button) {
        if(exists('input'))
          if(!is.null(button) ) {
            if(button > 0) {
              return(TRUE)
            }
          }
        return(FALSE)
      }
      assign('%&%',  function (a, b) paste(a, b, sep = ''))
      catn = function(...) cat(..., '\n')

      # Here begins the good stuff.
      output$evaluatedOutputR = renderUI({
        if(wasClicked(input$evalButtonR)) {
          # cat('evaluatedOutputR\n')
          evalString = isolate(input$evalStringR)
          capturedOutput =  capture.output(eval(parse(text=evalString)))
          HTML(paste(collapse='<br>', capturedOutput))
          #capturedOutput
          ## You have to isolate input$evalStringR; otherwise each character typed calls this callback.
          ## The following might be useful later for up-arrowing through past expressions.
          #   if(is.null(rValuesDebugging$evalStringHistory))
          #     rValuesDebugging$evalStringHistory = character(0)
          #  rValuesDebugging$evalStringHistory = c(rValuesDebugging$evalStringHistory, evalString)
        }
      })

      outputPreambleJS <<- 'window.Shiny.shinyapp.$bindings.'
      # EXAMPLE:  window.Shiny.shinyapp.$bindings.selTxt.firstChild.nodeValue
      inputPreambleJS <<- 'window.Shiny.shinyapp.$inputValues.'
      wrapperToGetKeys <<- function(x) 'Object.keys(' %&% x %&% ')'
      observerPreambleToggles = observe({
        input$prependInputPreambleToggle
        input$prependOutputPreambleToggle
        try({
          evalString = isolate(input$evalStringJS)
          if(wasClicked(input$prependInputPreambleToggle)) {
            if(substr(evalString, 1, nchar(inputPreambleJS)) != inputPreambleJS)
              evalString = paste0(inputPreambleJS, evalString)
          }
          else ## Remove inputPreambleJS
            evalString = gsub(inputPreambleJS, '', evalString, fixed=TRUE)

          if(wasClicked(input$prependOutputPreambleToggle)) {
            if(substr(evalString, 1, nchar(outputPreambleJS)) != outputPreambleJS)
              evalString = paste0(outputPreambleJS, evalString)
          }
          else ## Remove outputPreambleJS
            evalString = gsub(outputPreambleJS, '', evalString, fixed=TRUE)
          isolate( { rValuesDebugging$evalStringJS = evalString } )
          catn('Responding to preamble toggles, evalString=', evalString)
          updateTextInput(thisSession, 'evalStringJS', label='', value=rValuesDebugging$evalStringJS)
          # You need to specify the label arg too. The default, NULL, doesn't cut it.
        })
      })

      #output$evaluatedOutputJS = renderText({
      #shinyalert('JS output is in a popup alert window, if there was no error. Otherwise nothing happens')
      # }
      # )

      output$JSevaluation = renderUI({
        if(wasClicked(input$evalButtonJS) ) {
          evalString = gsub('"', "'", isolate(input$evalStringJS)) # replace all DQ with SQ.
          div(list(tags$script(
            paste0(
              'alert(eval("', evalString, '"))'       # THIS WORKS!
            )
          )))
        }
        # TRY THIS SOME TIME, to avoid creating an alert window for the JS output:
        #document.getElementById("demo").innerHTML = ... ;
      })

      output$shiny.trace.text = renderText({
        eval(options(shiny.trace=input$traceCheckbox), envir = .GlobalEnv);
        #cat("shiny.trace: ", options("shiny.trace")[[1]], "\n")
        if( options("shiny.trace")[[1]] != input$traceCheckbox)
          cat('Error: options("shiny.trace")[[1]] should equal input$traceCheckbox', "\n");
        paste("shiny trace=",
              ifelse(input$traceCheckbox, 'on', 'off'))
      })   #### OK this works now.


      output$debugTools = renderUI({
        shiny::conditionalPanel(
          condition=theShowDebuggerCondition,
          div(style="background:darkGrey",
              singleton(tags$script(paste(
                "outputPreambleJS = '", outputPreambleJS, "';")))
              ,
              fluidRow( style="color: blue",
                        column(4, checkboxInput(
                          inputId='debugToolsCheckbox', value=toolsInitialState,
                          label=em(strong("Show box of code to evaluate")))
                        ),
                        column(4, radioButtons('id_languageChoice', '',
                                               choices=c('R', 'JS'),
                                               inline=TRUE)
                               # inputId='debugToolsCheckbox_JS', value=toolsInitialState,
                               # label=em(strong("Debug a shiny apps: evaluate R and JS")))
                        ),
                        column(4, checkboxInput(
                          inputId="traceCheckbox",
                          value=FALSE,
                          label=em(strong(textOutput("shiny.trace.text")))
                        )
                        )
              ),
              conditionalPanel(
                'input.debugToolsCheckbox',
                conditionalPanel(
                  'input.id_languageChoice==="R"',
                  fluidRow(
                    column(2, offset = 1,
                           actionButton("evalButtonR",
                                        HTML("<font color='red'> evaluate R</font>"))
                    ),
                    column(9,
                           tagAppendAttributes(
                             style="width:550px; height:150px;",
                             tags$textarea(
                               id = "evalStringR", label="R code",
                               value="1234"))
                    ),
                    tagAppendAttributes(width=800,
                                        style='text-align:"right"; color:green',
                                        bsModal(id = 'evaluateR_popup',
                                                title="Output of R command",
                                                trigger = "evalButtonR",
                                                size="large",
                                                uiOutput(outputId="evaluatedOutputR")))
                  )),
                conditionalPanel(
                  'input.id_languageChoice==="JS"',
                  fluidRow(
                    column(2,
                           actionButton("evalButtonJS",
                                        HTML("<font color='red'> evaluate JS</font>")),
                           numericInput('idJSlineNum', label = '', value = 1)
                           ),
                    column(10, tagAppendAttributes(
                      style="width:550px; height:150px;",
                      tags$textarea(id = "evalStringJS",
                                    value="") )
                    )
                  ),
                  fluidRow(
                    column(6,
                           checkboxInput(inputId="prependOutputPreambleToggle",
                                         value=FALSE,
                                         label="prependOutputPreambleToggle")
                    ),
                    column(6,
                           checkboxInput(inputId="prependInputPreambleToggle",
                                         value=FALSE,
                                         label="prependInputPreambleToggle")
                    )
                  )
                )
              ),
              #list(HTML(paste0(rep("&nbsp;",15), collapse=""))),
              uiOutput(outputId='JSevaluation')
          )
        )
      }) ### end callto renderUI

    })  ### end of call to expression()
parentFrameNumber = 1
#cat('parentFrameNumber = ', parentFrameNumber, '\n')
eval(debugToolsExpression, envir = parent.frame(parentFrameNumber))
#cat('debugToolsExpression eval done ', '\n')
}
