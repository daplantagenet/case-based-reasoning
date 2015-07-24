cbrDistViz <- function(cbrObject) {
  require(shiny)
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(sliderInput("n", "Bins", 5, 100, 20)),
        mainPanel(plotOutput("hist"))
      )
    ),
    server = function(input, output, session) {
      
    }
  )
}
