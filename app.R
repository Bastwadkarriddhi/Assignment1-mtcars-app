library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Load dataset
data("mtcars")

# UI Layout
ui <- fluidPage(
  titlePanel("MTCARS Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X-axis Variable:", choices = names(mtcars)),
      selectInput("yvar", "Select Y-axis Variable:", choices = names(mtcars)),
      sliderInput("cylFilter", "Filter by Cylinders:", 
                  min = min(mtcars$cyl), max = max(mtcars$cyl), 
                  value = c(min(mtcars$cyl), max(mtcars$cyl))),
      checkboxInput("regression", "Include Regression Line", value = FALSE)
    ),
    
    mainPanel(
      plotlyOutput("scatterPlot"),
      tableOutput("summaryTable"),
      downloadButton("downloadData", "Download CSV")
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Reactive data filtering
  filteredData <- reactive({
    mtcars %>% filter(cyl >= input$cylFilter[1], cyl <= input$cylFilter[2])
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(aes(color = factor(cyl)), size = 3) +
      theme_minimal()
    
    if (input$regression) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
    }
    
    ggplotly(p)
  })
  
  # Summary Table
  output$summaryTable <- renderTable({
    filteredData()
  })
  
  # CSV Download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

