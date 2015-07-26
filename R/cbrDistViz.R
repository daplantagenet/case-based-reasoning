cbrDistViz <- function(cbrObject) {
  require(shiny)
  vClass <- unlist(lapply(cbrObject$learning, class))
  vars <- which(vClass != "numeric")
  
  shinyApp(
    ui = fluidPage(
      titlePanel("Manifold Learning in Case-Based Reasoning for survival data"),
      br(),
      sidebarLayout(
        sidebarPanel(width = 2,
                     selectInput(inputId = "method", label = "Method", choices = c("tSNE", "mds"), selected = "tSNE"),
                     hr(),
                      conditionalPanel("input.method == 'tSNE'",
                                       tagList(
                                         numericInput("tSNEDim", "Dimension", value = 2, min = 2),
                                         numericInput("tSNEPerp", "Perplexity", value=30, min=1),
                                         numericInput("tSNETheta", "Theta", value=0.5, min=0, max=1)))),
        mainPanel(
          uiOutput("tSNEUI"),
          plotOutput("plottSNE")
          )
      )
    ),
    server = function(input, output, session) {
      output$tSNEUI <- renderUI({
        ret <- tagList()
        ret <- c(ret, tagList(fluidRow(column(3, selectInput("tSNEColumn", "n columns:", choices = 1:10, selected = 2)),
                              column(3, selectInput("tSNECol", "Colour:", choices = vars, selected = vars[1])))))
        wellPanel(ret)
      })
      
      tSNE <- reactive({
        if (is.null(input))
          return()
        
        Rtsne(X = cbrObject$distMat, initial_dims = 5, dims = input$tSNEDim, perplexity = input$tSNEPerp, input$tSNETheta, is_distance=TRUE, check_duplicates=FALSE)$Y
      })
      output$plottSNE <- renderPlot({
        y <- tSNE()
        if (is.null(y))
          return()
        
        nColumn <- as.numeric(input$tSNEColumn)
        if (is.null(nColumn))
          return()
        
        
        n <- input$tSNEDim
        cc <- combn(n, 2)
        m <- ncol(cc)
        ggList <- list()
        for (i in 1:m) {
          df <- data.frame(x1=y[, cc[1, i]], x2=y[, cc[2, i]], col=cbrObject$learning[, as.numeric(input$tSNECol)])
          q <- ggplot(df) + 
            geom_point(aes(x=x1, y=x2, colour=col), size=3) +
            scale_color_brewer(name=names(cbrObject$learning)[as.numeric(input$tSNECol)], type = "qual") +
            theme_cowplot(font_size = 18) +
            theme(legend.position="top") +
            xlab("") + ylab("")
          ggList[[i]] <- q
        }
        lab <- paste0("Dimension: ", apply(cc, 2, function(x) paste(x, collapse=" - ")))
        plot_grid(plotlist = ggList, ncol = nColumn, labels = lab)
      })
    }
  )
}
