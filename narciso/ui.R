library(shiny)
library(visNetwork)

#shiny::runApp(system.file("shiny", package = "visNetwork"))
# Define UI for app that draws a histogram ----
fluidPage(style="padding-top: 0px;",
    visNetworkOutput("network",height="1000"),
  absolutePanel(
    top = 20, left = 20, width = 200,
    draggable = TRUE,
    wellPanel(
      selectInput(inputId="select", label = h4("Select stanza"), 
          choices = list("Stanza 1" = 1, "Stanza 2" = 2, "Stanza 3" = 3, "Stanza 4" = 4,
                        "Stanza 5" = 5, "Stanza 6" = 6, "Stanza 7" = 7,"Stanza 8" = 8,
                        "Stanza 9" = 9, "Stanza 10" = 10, "Stanza 11" = 11, "Stanza 12" = 12,
                        "Stanza 13" = 13,"Stanza 14" = 14, "Stanza 15" = 15, "Stanza 16" = 16, "Whole poem" = 0),
          selected = 1),
      checkboxInput(inputId="nodetype","All Nodes", value = TRUE),
      checkboxInput(inputId="soundon","With Sound", value = FALSE)
    ),
    style = "opacity: 0.92"
  )
)
