# Load required packages
library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Random Whole Number Generator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input for minimum range
      numericInput("min", "Minimum:", 1),
      
      # Input for maximum range
      numericInput("max", "Maximum:", 100),
      
      # Button to generate random number
      actionButton("generate", "Generate")
    ),
    
    mainPanel(
      # Output: Display random number
      textOutput("randomNumber"),
      
      # Output: Display count plot
      plotOutput("countPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store counts
  counts <- reactiveValues(values = NULL)
  
  # Initialize counts based on input range
  observe({
    if (!is.null(input$min) && !is.null(input$max) && input$min <= input$max) {
      counts$values <- setNames(rep(0, input$max - input$min + 1), input$min:input$max)
    }
  })
  
  # Reactive expression to generate random number
  randomNumber <- eventReactive(input$generate, {
    sample(input$min:input$max, 1)
  })
  
  # Update counts whenever a new number is generated
  observeEvent(input$generate, {
    num <- randomNumber()
    counts$values[as.character(num)] <- counts$values[as.character(num)] + 1
  })
  
  # Output random number
  output$randomNumber <- renderText({
    req(input$generate)
    paste("Generated Random Number:", randomNumber())
  })
  
  # Output count plot
  output$countPlot <- renderPlot({
    req(counts$values)
    data <- data.frame(Number = as.numeric(names(counts$values)), Count = counts$values)
    ggplot(data, aes(x = Number, y = Count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Count of Each Number Selected", x = "Number", y = "Count") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
