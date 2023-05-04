library(shiny)
library(corrplot)
library(ggradar)
library(scales)
library(dplyr)

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
  
  titlePanel("imadb data viz"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Analysis of the database or external files"),
      
      tabsetPanel(
        
        tabPanel("correlation", 
                 
                 
                 h3("Correlation Analisys"),
                 fileInput(inputId = "input_data_corr", label = "Input Data",
                           accept = ".csv",
                           buttonLabel = "Browse..."),
                 helpText("Clustering order of correlations"),
                 selectInput("order", label = "Order", 
                             choices = c("original", "AOE", "FPC", "hclust", "alphabet"), 
                             selected = "hclust"),
                 textInput(inputId = "title_correlation_plot", label = "Graph Title"),
                 sliderInput(inputId = "font_size", label = "Font Size", value = 1.5, min = 0.5, max = 3, step = 0.7, width = "300px"),
                 sliderInput(inputId = "plot_size", label = "Plot Size", value = 600, min = 400, max = 900, step = 100, width = "300px"),
                 actionButton(inputId = "analyze_data_button", label = "Analisys Data")
                 
                 ),
        
        tabPanel("radar",
                 
                 h3("Radar Chart"),
                 fileInput(inputId = "input_data_radar", label = "Input Data",
                           accept = ".csv",
                           buttonLabel = "Browse..."),
                 
                 selectInput("radarVariables", choices = NULL,
                             label = "variables", selected = NULL)
                 
                 
                 )
        
            ),
      
      ),
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Correlation Plot", div(id = "correlation_plot_container",
                                              plotOutput("correlationPlot", width = "600px", height = "650px"))
                           ),
                  
                  tabPanel("Radar Chart", plotOutput("radarPlot")),
                  
                  tabPanel("Table", DT::dataTableOutput("tableData"))
                  
                  ),
      
      
      
      
      )
    
  )
  
)

server <- function(input, output, session) {
  
  # correlations -----------------------
  
  data_input <- reactive({
    req(input$input_data_corr)
    data_frame <- read.csv(input$input_data_corr$datapath)
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
    
    output$tableData <- DT::renderDataTable({data_input()})
    
  })
  
  # radar --------------------------------------------------->
  
  observeEvent(input$input_data_radar, {
    
    df <- read.csv(input$input_data_radar$datapath)
    df <- replace(df, is.na(df), 0)
    
    updateSelectInput(
      session,
      "radarVariables",
      choices = names(df),
      selected = names(df)[1]
    )
    
    df <- df %>% mutate_all(as.numeric)
    
    output$radarPlot <- renderPlot({
      req(df)
      ggradar(scale(df), aes_string(x = input$radarVariables, y = input$radarVariables))
    })
    
  })
  
  # end -------------------------------------------------------->
  

}

shinyApp(ui = ui, server = server)
