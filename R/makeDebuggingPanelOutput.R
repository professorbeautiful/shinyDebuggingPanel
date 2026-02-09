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
    condition='true',
    initialTraceValue=FALSE,
    includePreambleFeature = TRUE,
    verbose=0) {
  verbose <<- verbose
  # if( ! require('shinyalert')) {
  #   devtools::install_github('daattali/shinyalert', quietly=TRUE, warn.conflicts=FALSE)
  # }
  #thisSession <<- shiny::getDefaultReactiveDomain()
  # either is OK, but the ifelse fails, both with is.null() and  missing().
  toolsInitialState <<- toolsInitialState
  initialTraceValue <<-  initialTraceValue
  theShowDebuggerCondition <<- condition
  includePreambleFeature <<- includePreambleFeature
  if(is.null(session))
    thisSession <<- shiny::getDefaultReactiveDomain()
  else thisSession <<- session
  rValuesDebugging_R <<- reactiveValues(evalStringHistory=list(),
                                        capturedOutput="")
  rValuesDebugging_JS <<- reactiveValues(evalStringHistory=list())

  rV <<- reactiveValues(Rcommand = 'hello')

  if(includePreambleFeature) {
    preamble1checkbox <<-
      checkboxInput(
        inputId="prependOutputPreambleToggle",
        value=FALSE,
        label="prepend/remove Output Preamble")
    preamble2checkbox <<-
      checkboxInput(inputId="prependInputPreambleToggle",
                    value=FALSE,
                    label="prepend/remove Input Preamble")
  } else {
    preamble1checkbox <<- " "
    preamble2checkbox <<- " "
  }



  debugToolsExpression = expression(
    {
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


      observeEvent(input$evalStringR, {
        rV$Rcommand = input$evalStringR
      })

      #bindEvent(   #input$evalButtonR
      observeEvent(input$evalButtonR, {
        evalString = rV$Rcommand

        if(verbose>0) print(paste('evalString', evalString))
        rValuesDebugging_R$evalStringHistory =
          c(rValuesDebugging_R$evalStringHistory, evalString)
        if(verbose>0) cat("length evalStringHistory = ",
                          length(rValuesDebugging_R$evalStringHistory),  '\n')
        rValuesDebugging_R$capturedOutput =
          capture.output(try(eval(parse(text=evalString))))
        rValuesDebugging_R$lineWidths =
          sapply(strsplit(split = '\n',
                          rValuesDebugging_R$capturedOutput), nchar)
        rValuesDebugging_R$maxWidth =
          max(rValuesDebugging_R$lineWidths)
        if(verbose>1) print(paste('capturedOutput ', rValuesDebugging_R$evalStringHistory))
        if(verbose>1) print(paste('rValuesDebugging_R$lineWidths', rValuesDebugging_R$lineWidths))
        observeEvent(input$idCopyToPB, {
          if(wasClicked(input$idCopyToPB))
            write(rValuesDebugging_R$capturedOutput,
                  file=pipe('pbcopy')
            )
        })
        showModal(
          modalDialog(
            title = div(
              fluidRow(
                column(7, style='text-align:left; color:blue',
                       em("To close this popup:"),
                       modalButton('cancel')
                )
                ,
                column(4, style='font-size:x-small;',
                       actionButton('idCopyToPB',
                                    HTML('Click here to "ctrl/cmd C"',
                                         '<br/>the entire result.',
                                         '<br/>(ctrl/cmd A is not helpful.)'))
                )
              ),
              HTML(gsub('\n', br(), evalString) )
            ),
            # easy_close = TRUE,  #doesn't work. you need the cancel button.
            ### so far no solution for scrollbar inside modalDialog.
            # wellPanel(style='text-align:left; color:red',
            #           align = "center",
            #                scroller::use_scroller(animationLength = 2000), # add use_scroller() in the UI
            #                h1("Shiny with scroller"),

            div(style='font-family:monaco',
                HTML(gsub(' ', '&nbsp;', paste(collapse='<br/>',     # note the '/'.
                                               rValuesDebugging_R$capturedOutput)
                )
                )
            )

          ) )

        updateNumericInput(label = ' ', session = thisSession, inputId = 'idRlineNum',
                           value = length(rValuesDebugging_R$evalStringHistory),
                           max = length(rValuesDebugging_R$evalStringHistory))
      })


      outputPreambleJS <<- 'window.Shiny.shinyapp.$bindings.'
      # EXAMPLE:  window.Shiny.shinyapp.$bindings.selTxt.firstChild.nodeValue
      inputPreambleJS <<- 'window.Shiny.shinyapp.$inputValues.'
      wrapperToGetKeys <<- function(x) 'Object.keys(' %&% x %&% ')'

      observeEvent(input$idJSlineNum, {
        #if(verbose>1) print(input$idJSlineNum)
        if(!is.na(input$idJSlineNum) & (input$idJSlineNum > 0) &
           input$idJSlineNum <= length(rValuesDebugging_JS$evalStringHistory)) {
          print(rValuesDebugging_JS$evalStringHistory[[input$idJSlineNum]])
          updateTextAreaInput(label=' ', session = thisSession, inputId = 'evalStringJS',
                              value = rValuesDebugging_JS$evalStringHistory
                              [[input$idJSlineNum]])
        }
      })

      if(includePreambleFeature)
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

            isolate( { rValuesDebugging_JS$evalStringJS = evalString } )
            # This 'isolate' is not the problem.
            catn('Responding to preamble toggles, evalString=', evalString)
            updateTextAreaInput(label=' ', session = thisSession,
                                inputId = 'evalStringJS',
                                value=evalString)
            # This updateTextInput works.
            # This statement works without error when run from the evalStringR box!
            # It also works here!
            # But does it kick off the labelNode error when one of
            # the checkboxes is clicked?
          })
        })

      # output$evaluatedOutputJS = renderText({
      #   #shinyalert('JS output is in a popup alert window, if there was no error. Otherwise nothing happens')
      # }
      # )
      observeEvent('idWidth', {
        if(!is.null(input$idWidth))
          options(width = input$idWidth)
      })
      output$JSevaluation = renderUI({
        if(wasClicked(input$evalButtonJS) ) {
          evalString = gsub('"', "'", isolate(input$evalStringJS))
          # replace all DQ with SQ. May not always be the right fix.
          isolate({
            rValuesDebugging_JS$evalStringHistory =
              c(rValuesDebugging_JS$evalStringHistory, evalString);
            cat('length(rValuesDebugging_JS$evalStringHistory)\n');
            print(length(rValuesDebugging_JS$evalStringHistory));
            updateNumericInput(label=' ', session = thisSession, inputId = 'idJSlineNum',
                               value = length(rValuesDebugging_JS$evalStringHistory),
                               max = length(rValuesDebugging_JS$evalStringHistory))
          })
          div(list(tags$script(
            paste0(
              'alert(eval("', evalString, '"))'       # THIS WORKS!
            )
          )))
          # if(is.null(rValuesDebugging_JS$evalStringHistory))
          #   rValuesDebugging_JS$evalStringHistory = character(0)
          # rValuesDebugging_JS$evalStringHistory = c(rValuesDebugging_JS$evalStringHistory, evalString)

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

      observeEvent(input$idRlineNum, {
        #if(verbose > 0)
        print(paste('input$idRlineNum', input$idRlineNum) )
        if(!is.na(input$idRlineNum) & (input$idRlineNum > 0) &
           input$idRlineNum <= length(rValuesDebugging_R$evalStringHistory)) {
          print(paste('Updating evalStringR with ', 'input$idRlineNum', input$idRlineNum,
                      ':',
                      rValuesDebugging_R$evalStringHistory[[input$idRlineNum]]) )
          updateTextAreaInput(label=' ', session = thisSession, inputId = 'evalStringR',
                              value = rValuesDebugging_R$evalStringHistory
                              [[input$idRlineNum]])
          rV$Rcommand = rValuesDebugging_R$evalStringHistory [[input$idRlineNum]]
        }
      })
      fluidRow_R =
        div(
          fluidRow(
            column(3, offset = 2,
                   br(),
                   actionButton(
                     "evalButtonR",
                     HTML(
                       "<font color='dark red' style='font-weight:bold'> evaluate R</font>")),
            )
            ,  column(2,
                      numericInput('idRlineNum',
                                   label = HTML("<font color='white' style='font-weight:bold'>
                               Command history</font>"),
                                   #width = '150px',
                                   value = 1, min=1, step=1)
            )
            ,  column(3,                      numericInput('idWidth',
                                   label =    HTML("<font color='white' style='font-weight:bold'>
                                Line width for panel</font>"),
                                   value = getOption('width'), min = 30, max=300, step = 1)

            ) ),
          fluidRow(
            column(8, offset=2,
                   tagAppendAttributes(
                     style="width:650px; height:150px;",
                     tags$textarea(
                       id = "evalStringR", label="R code",
                       value="1234")
          )
            ))
        )  ###end of  fluidRow_R

      #   tagAppendAttributes(width=800,
      #                       style='text-align:"right"; color:green',
      #                       bsModal(id = 'evaluateR_popup',
      #                               title="Output of R command",
      #                               trigger = "evalButtonR",
      #                               size="large",
      #                               uiOutput(outputId="evaluatedOutputR")))

      fluidRow_JS =  fluidRow(
        column(2, offset = 1,
               actionButton( inputId = "evalButtonJS",
                             label = HTML(
                               "<font color='dark red' style='font-weight:bold'> evaluate JS</font>")),
               numericInput('idJSlineNum', label = "command\nhistory",
                            width = '100px', value = 1, min=1)
        ),
        column(9, tagAppendAttributes(
          style="width:550px; height:150px;",
          tags$textarea(id = "evalStringJS",
                        value="") )
          ,
          preamble1checkbox,
          preamble2checkbox
        )
      )
      fluidRow_debugToolsCheckbox = fluidRow( style="color: blue",
                                              # column(4, checkboxInput(
                                              #   inputId='debugToolsCheckbox', value=toolsInitialState,
                                              #   label=em(strong("Show box of code to evaluate")))
                                              # ),
                                              column(4, radioButtons('id_languageChoice',
                                                                     choices=c('R', 'JS'),
                                                                     selected = 'R',
                                                                     inline=TRUE, label='language')
                                                     # inputId='debugToolsCheckbox_JS', value=toolsInitialState,
                                                     # label=em(strong("Debug a shiny apps: evaluate R and JS")))
                                              ),
                                              column(4, offset=4, checkboxInput(
                                                inputId="traceCheckbox",
                                                value=initialTraceValue,
                                                label=em(strong(textOutput("shiny.trace.text")))
                                              )
                                              )
      )
      #### output$debugTools ####
      output$debugTools =
        renderUI({
          shiny::conditionalPanel(
            condition=theShowDebuggerCondition,
            div(style="background:darkGrey",
                singleton(tags$script(paste(
                  "outputPreambleJS = '", outputPreambleJS, "';")))
                ,
                fluidRow_debugToolsCheckbox
                ,
                hr(),
                # conditionalPanel(
                #   'input.debugToolsCheckbox',
                conditionalPanel(
                  'input.id_languageChoice=="R"',
                  fluidRow_R)
                ,
                conditionalPanel(
                  'input.id_languageChoice=="JS"',
                  fluidRow_JS),
                # ),
                list(HTML(paste0(rep("&nbsp;",15), collapse=""))),
                uiOutput(outputId='JSevaluation')
            )
          )
        }) ### end callto renderUI

    })  ### end of call to expression()
  parentFrameNumber = 1
  #cat('parentFrameNumber = ', parentFrameNumber, '\n')
  #  OK-   assign('debugToolsExpression.saved', debugToolsExpression, pos=1)
  eval(debugToolsExpression, envir = parent.frame(parentFrameNumber))
  #cat('debugToolsExpression eval done ', '\n')
}
