# shinyDebuggingPanel
shinyDebuggingPanel R package. 

You can include this panel in any shiny project.

Place shinyDebuggingPanel::makeDebuggingPanelOutput(session) 
inside your call to shinyServer(server.R). 

Place shinyDebuggingPanel::withDebuggingPanel() 
in your ui.R.

help('makeDebuggingPanelOutput', package='shinyDebuggingPanel')

The preamble feature for javascript is not that helpful.

2025-10-08
Big changes, because damn, bsModal no longer showing the output,
and shinyalert was full of problems too, like not being able to close the window.

So, now using showModal(modalDialog(...)).
That allows HTML, and the cancel button works.
(But easy_close = TRUE  does not work.)

2018-05-05
Cosmetic improvements.
