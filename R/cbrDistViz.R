#' Shiny Application for Manifold Learning 
#'
#' @param cbrObject
#'
#' @export cbrDistViz
cbrDistViz <- function(cbrObject) {
  require(shiny)
  vClass <- unlist(lapply(cbrObject$learning, class))
  vars <- which(vClass != "numeric")
  
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Manifold Learning in Case-Based Reasoning for survival data"),
      br(),
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 2,
                            shiny::selectInput(inputId = "method", label = "Method", choices = c("tSNE"), selected = "tSNE"),
                     hr(),
                     helpText("Changing following parameters will restart algorithm. This may need some time for large data sets."),
                     hr(),
                     shiny::conditionalPanel("input.method == 'tSNE'",
                                       tagList(
                                         shiny::numericInput("tSNEDim", "Dimension", value = 2, min = 2),
                                         shiny::numericInput("tSNEPerp", "Perplexity", value=30, min=1),
                                         shiny::numericInput("tSNETheta", "Theta", value=0.5, min=0, max=1))),
                     hr(),
                     shiny::checkboxInput("summGraph", "All in one Graph", value = F),
                     shiny::checkboxInput("showTable", "Data Table", value = F),
                     shiny::numericInput("gHeight", label = "Graphic Height", value = 400, min = 100, max = 1000)),
        shiny::mainPanel(
          shiny::uiOutput("tSNEUI"),
          shiny::plotOutput("plottSNE")
          )
      )
    ),
    server = function(input, output, session) {
      grHeight <- shiny::reactive({
        grHeight <- input$gHeight
        if (is.null(grHeight))
          grHeight <- 400
        
        grHeight
      })
      
      shiny::output$tSNEUI <- renderUI({
        ret <- tagList()
        
        if(input$summGraph) {
          ret <- c(ret, shiny::tagList(shiny::fluidRow(column(3, selectInput("tSNEColumn", "n columns:", choices = 1:10, selected = 2)),
                                         column(3, selectInput("tSNECol", "Colour:", choices = vars, selected = vars[1])))))
        } else {
          n <- input$tSNEDim
          x <- 1:n
          names(x) <- paste0("x", 1:n)
          ret <- c(ret, shiny::tagList(shiny::fluidRow(column(3, selectInput("tSNEx", "x:", choices = x, selected = x[1])),
                                         column(3, selectInput("tSNEy", "y:", choices = x, selected = x[2])),
                                         column(3, selectInput("tSNECol", "Colour:", choices = vars, selected = vars[1])))))
        }
        wellPanel(ret)
      })
      
      tSNE <- shiny::reactive({
        if (is.null(input))
          return()
        
        Rtsne::Rtsne(X                = cbrObject$distMat, 
              initial_dims     = 5, 
              dims             = input$tSNEDim, 
              perplexity       = input$tSNEPerp, 
              theta            = input$tSNETheta, 
              is_distance      = TRUE, 
              check_duplicates = FALSE)$Y
      })
      
      output$plottSNE <- shiny::renderPlot({
        y <- tSNE()
        if (is.null(y))
          return()
        
        nColumn <- as.numeric(input$tSNEColumn)
        if (is.null(nColumn))
          nColumn <- 1
        
        n <- input$tSNEDim
        if(input$summGraph) {
          cc <- combn(n, 2)
          m <- ncol(cc)
          ggList <- list()
          for (i in 1:m) {
            df <- data.frame(x1  = y[, cc[1, i]], 
                             x2  = y[, cc[2, i]], 
                             col = cbrObject$learning[, as.numeric(input$tSNECol)])
            q <- ggplot2::ggplot(df) + 
              ggplot2::geom_point(aes(x=x1, y=x2, colour=col), size=3) +
              ggplot2::scale_color_brewer(name=names(cbrObject$learning)[as.numeric(input$tSNECol)], type = "qual") +
              cowplot::theme_cowplot(font_size = 18) +
              ggplot2::theme(legend.position="top") +
              ggplot2::xlab("") + 
              ggplot2::ylab("")
            ggList[[i]] <- q
          }
          lab <- paste0("Dimension: ", apply(cc, 2, function(x) paste(x, collapse=" - ")))
          cowplot::plot_grid(plotlist = ggList, ncol = nColumn, labels = lab)
        } else {
          nColumn <- 1
          x1 <- as.numeric(input$tSNEx)
          x2 <- as.numeric(input$tSNEy)
          df <- data.frame(x1=y[, x1], x2=y[, x2], col=cbrObject$learning[, as.numeric(input$tSNECol)])
          q <- ggplot2::ggplot(df) + 
            ggplot2::geom_point(aes(x=x1, y=x2, colour=col), size=3) +
            ggplot2::scale_color_brewer(name=names(cbrObject$learning)[as.numeric(input$tSNECol)], type = "qual") +
            cowplot::theme_cowplot(font_size = 18) +
            ggplot2::theme(legend.position="top") +
            ggplot2::xlab("") + 
            ggplot2::ylab("")
          lab <- paste0("Dimension: ", paste(c(x1, x2), collapse=" - "))
          cowplot::plot_grid(q, ncol = 1, labels = lab)
        }
      }, width = 'auto', height = grHeight, units="px") # renderPlot
    }
  )
}
