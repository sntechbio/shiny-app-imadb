library(shiny)
library(corrplot)

ui <- fluidPage(
  
  tags$head(
    tags$style(
      type = "text/css",
      "#correlation_plot_container { 
        display: flex; 
        justify-content: center; 
         
        height: 100vh; 
      }"
    )
  ),
  
  titlePanel(strong("Imadb App")),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Analysis of the database or external files"),
      
      fileInput(inputId = "input_data_ext", label = "Input Data",
                accept = ".csv",
                buttonLabel = "Browse..."),
      
      helpText("Clustering order of correlations"),
      
      selectInput("order", label = "Order", 
                  choices = c("original", "AOE", "FPC", "hclust", "alphabet"), 
                  selected = "hclust"),
      
      textInput(inputId = "title_correlation_plot", label = "Graph Title"),
      
      sliderInput(inputId = "font_size", label = "Font Size", value = 1.5, min = 0.5, max = 3, step = 0.7, width = "300px"),
      
      sliderInput(inputId = "plot_size", label = "Plot Size", value = 600, min = 400, max = 900, step = 150, width = "300px"),
      
      actionButton(inputId = "analyze_data_button", label = "Analisys Data")
      
      ),
    
    mainPanel(div(id = "correlation_plot_container",
                  plotOutput("correlationPlot", width = "600px", height = "600px")))
    
  )
  
)

server <- function(input, output) {
  
  data_input <- reactive({
    req(input$input_data_ext)
    data_frame <- read.csv(input$input_data_ext$datapath)
    data_frame <- replace(data_frame, is.na(data_frame), 0)
  })
  
  
  
  observeEvent(input$analyze_data_button, {
    
    output$correlationPlot <- renderPlot({
      
      req(data_input())
      correlationMatrix <- cor(data_input())
      
      corrplot(correlationMatrix,
               order = input$order,
               title = input$title_correlation_plot,
               mar = c(0,0,2,0),
               tl.col = "black",
               cex.main = input$font_size)
      
    }, height = input$plot_size, width = input$plot_size)
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
