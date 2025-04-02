library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(colourpicker)

# Shiny App for visualizing MTCARS dataset with various features

ui <- fluidPage(
  useShinyjs(),
  titlePanel("MTCARS Shiny App"),
  sidebarLayout(
    sidebarPanel(
      # 1 Feature: Select plot type (Scatter, Line, Bar)
      selectInput("plotType", "Select Plot Type:", choices = c("Scatter", "Line", "Bar")),
      
      # Feature: Select variables for X and Y axes
      selectInput("xvar", "X-axis Variable:", choices = names(mtcars)),
      selectInput("yvar", "Y-axis Variable:", choices = names(mtcars)),
      
      # 2 Feature: Customize point size and color
      sliderInput("pointSize", "Point Size:", min = 1, max = 5, value = 2),
      colourpicker::colourInput("pointColor", "Point Color:", value = "blue"),
      
      # 3. Feature: Apply data transformations
      selectInput("transformation", "Apply Transformation:", choices = c("None", "Log", "Square Root")),
      
      # 4.Feature: Filter data by cylinder count and horsepower range
      checkboxGroupInput("cylFilter", "Select Cylinders:", choices = unique(mtcars$cyl), selected = unique(mtcars$cyl)),
      sliderInput("hpRange", "Horsepower Range:", min = min(mtcars$hp), max = max(mtcars$hp), value = range(mtcars$hp)),
      
      # Feature: Filter by transmission type
      selectInput("trans", "Transmission Type:", choices = c("All", "Automatic", "Manual")),
      
      # 5. Feature: Perform statistical test (t-test on MPG vs Transmission)
      actionButton("runTest", "Run t-test (MPG vs Transmission)"),
      
      # 8. Feature: Download plots as PNG or PDF
      downloadButton("downloadPlotPNG", "Download PNG"),
      downloadButton("downloadPlotPDF", "Download PDF"),
      
      # 7. Feature: User authentication for restricted access
      passwordInput("password", "Enter Password:"),
      actionButton("login", "Login")
    ),
    mainPanel(
      plotOutput("plot", hover = hoverOpts(id = "plot_hover")),
      verbatimTextOutput("statsTest"),
      dataTableOutput("filteredData"),
      verbatimTextOutput("hover_info")
    )
  )
)

server <- function(input, output, session) {
  # 6. Authentication feature
  observeEvent(input$login, {
    if (input$password != "abcd") {
      hide("plot")
      hide("statsTest")
      hide("filteredData")
      showNotification("Access Denied! Incorrect Password.", type = "error")
    } else {
      show("plot")
      show("statsTest")
      show("filteredData")
      showNotification("Access Granted!", type = "message")
    }
  })
  
  # Feature: Data filtering based on user inputs
  filteredData <- reactive({
    data <- mtcars %>%
      filter(cyl %in% input$cylFilter, hp >= input$hpRange[1], hp <= input$hpRange[2])
    if (input$trans != "All") {
      data <- data %>% filter(am == ifelse(input$trans == "Automatic", 0, 1))
    }
    return(data)
  })
  
  # Feature: Plot generation
  output$plot <- renderPlot({
    data <- filteredData()
    
    x <- data[[input$xvar]]
    y <- data[[input$yvar]]
    
    if (input$transformation == "Log") {
      x <- log1p(x)
      y <- log1p(y)
    } else if (input$transformation == "Square Root") {
      x <- sqrt(abs(x))
      y <- sqrt(abs(y))
    }
    
    p <- ggplot(data, aes(x = x, y = y)) +
      geom_point(size = input$pointSize, color = input$pointColor)
    
    if (input$plotType == "Line") {
      p <- p + geom_line()
    } else if (input$plotType == "Bar") {
      p <- ggplot(data, aes(x = factor(x), y = y, fill = factor(x))) +
        geom_bar(stat = "identity")
    }
    
    print(p)
  })
  
  # Feature: Statistical test (t-test for MPG vs Transmission)
  output$statsTest <- renderPrint({
    req(input$runTest)
    data <- filteredData()
    
    # Ensure 'am' has exactly two levels before running t-test
    if (length(unique(data$am)) == 2) {
      test <- t.test(mpg ~ am, data = data)
      print(test)
    } else {
      print("Error: Not enough data for t-test. Ensure both transmission types are included.")
    }
  })
  
  # Feature: Display hover information
  output$hover_info <- renderPrint({
    req(input$plot_hover)
    paste("Hovering at: X =", input$plot_hover$x, "Y =", input$plot_hover$y)
  })
  
  # Feature: Render filtered data table
  output$filteredData <- renderDataTable({
    datatable(filteredData())
  })
  
  # Feature: Download plot as PNG
  output$downloadPlotPNG <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      png(file)
      print(output$plot())
      dev.off()
    }
  )
  
  # Feature: Download plot as PDF
  output$downloadPlotPDF <- downloadHandler(
    filename = "plot.pdf",
    content = function(file) {
      pdf(file)
      print(output$plot())
      dev.off()
    }
  )
}

shinyApp(ui, server)
