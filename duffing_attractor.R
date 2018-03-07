library(shiny)
library(pracma)

ui <- fluidPage(
  titlePanel("Duffing attractor"),
  
  fluidRow(
    column(2, align = "center",
      sliderInput("damping",
                  "DAMPING:",
                  min = 0.0,
                  max = 10.0,
                  value = 0.25,
                  step = 0.05)),
    column(2, align = "center",   
      sliderInput("amplitude",
                   "AMPLITUDE:",
                   min = 0.0,
                   max = 100.0,
                   value = 0.3,
                   step = 0.05)),
    column(2, align = "center",
       sliderInput("angularFreq",
                   "ANGULAR FREQUENCY:",
                   min = 0.0,
                   max = 100.0,
                   value = 1.0,
                   step = 0.05)),
    column(2,
       sliderInput("time",
                   "TIME:",
                   min = 0.0,
                   max = 60.0,
                   value = 10.0,
                   step = 1.0)),
    column(2,
       sliderInput("startX",
                   "START X:",
                   min = -30.0,
                   max = 30.0,
                   value = 1.0,
                   step = 0.5)),
    column(2,
       sliderInput("startY",
                   "START Y:",
                   min = -30.0,
                   max = 30.0,
                   value = 1.0,
                   step = 0.5))
    ),
  
  fluidRow(
    column(6, align = "center",
      fluidRow(
        column(12,
          plotOutput("xt"),
          plotOutput("yt"))
        )
      ),
    column(6, align = "center",          
      plotOutput("xt_yt", height = 800))
   )
)

duffingEq <- function(t, x, damping, amplitude, angularFreq) {
  return(as.matrix(c(x[2], -damping*x[2] - x[1] - x[1]^3 + amplitude*cos(angularFreq*t))))
}

solve <- function(t, startX, startY, damping, amplitude, angularFreq) {
  f <- function(t, x)
    duffingEq(t, x, damping, amplitude, angularFreq)
  
  x <- as.matrix(c(startX, startY))
  
  solution <- ode23(f, 0, t, x)
  return(solution)
}

minAreaSize <- function(solution) {
  return(abs(max(abs(unlist(solution$y)))))
}

renderXTPlot <- function(solution) {
  return(renderPlot({
    plot(c(0, 60), c(-minAreaSize(solution), minAreaSize(solution)), type = "n",
         xlab = "Time", ylab = "x", main = "x(t)")
    
    lines(solution$t, solution$y[, 1], col = "blue")
    grid()
  }))
}


server <- function(input, output) {
  solution <- reactive({
    solve(input$time, input$startX, input$startY, input$damping, input$amplitude, input$angularFreq)
  })
  
  areaSize <- reactive({
    minAreaSize(solution())
  })

  output$xt <- renderPlot({
    plot(c(0, 60), c(-areaSize(), areaSize()), type = "n",
         xlab = "Time", ylab = "x", main = "x(t)")
    
    lines(solution()$t, solution()$y[, 1], col = "blue")
    grid()
  })
  
  output$yt <- renderPlot({
    plot(c(0, 60), c(-areaSize(), areaSize()), type = "n",
         xlab = "Time", ylab = "y", main = "y(t)")
    
    lines(solution()$t, solution()$y[, 2], col = "blue")
    grid()
  })
  
  output$xt_yt <- renderPlot({
    axisSize <- c(-areaSize(), areaSize())
    plot(axisSize, axisSize, type = "n",
         xlab = "x", ylab = "y", main = "f(x(t),y(t))")
    
    lines(solution()$y[, 1], solution()$y[, 2], col = "blue")
    grid()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

