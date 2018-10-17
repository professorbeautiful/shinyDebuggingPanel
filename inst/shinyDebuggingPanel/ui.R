#library(shinyjs)

shinyUI(
  navbarPage(
    title="shinyDebuggingPanel",
    div(h1("A simple example"),
        hr(),
        shinyDebuggingPanel::withDebuggingPanel(),
        h4('To use this facility:'),
        pre(
"
Select R or JS for your language, type an expression in the box,
and click the 'evaluate' button.
Use the counter to retrieve previous commands.
"
        ),
        h4('To add this facility to your app:'),
        pre(
"
- install the package with
    install_github('professorbeautiful/shinyDebuggingPanel')
- insert
    require(shinyDebuggingPanel)
into the global code for your app,
- insert
    withDebuggingPanel()
into your UI,
- insert
      makeDebuggingPanelOutput(session, toolsInitialState = TRUE, condition='true')
into your server code.",
            br(),
            pre(
"Use the 'condition' arg to set up a toggle for visibility of the panel,
for example driven by pressing a shortcut key.")
        )
    )
  )
)
