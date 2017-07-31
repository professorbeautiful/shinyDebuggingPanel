#library(shinyjs)

shinyUI(
  navbarPage(
    title="HELLO",
    div(h1("EXAMPLE"),
        hr(),
        shinyDebuggingPanel::withDebuggingPanel(),
        'NEW debugTools!'
        #                       uiOutput(outputId="debugTools"),
    )
  )
)
