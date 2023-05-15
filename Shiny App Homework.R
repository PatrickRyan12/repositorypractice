#1) Produce the shiny app shown below
library(tidyverse)
library(shiny)

diamonds %>%
  select(carat, price, x, y, z) -> diamonds1
diamonds1

ui <- fluidPage(
  titlePanel("Frequency Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "DVvar", label = "Diamond Variables", choices = names(diamonds1), selected = names(diamonds1)[1])),
    mainPanel(plotOutput("plot"))))

server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(diamonds, mapping = aes(x = .data[[input$DVvar]], color = cut)) +
      geom_freqpoly(binwidth = 0.1) +
      ggtitle("Frequency Polygons") +
      labs(x = input$DVvar, y = "Count")
  })
}
shinyApp(ui = ui, server = server)


#2) 
library(tidyverse)
library(shiny)
library(DT)

diamonds %>%
  select(carat, price, x, y, z) -> diamonds1
diamonds1

ui <- fluidPage(
  titlePanel("Diamond Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chartType", label = "Chart Type", choices = c("Frequency Polygon", "Boxplot"), selected = "Frequency Polygon"),
      selectInput(inputId = "variable", label = "Variable", choices = names(diamonds1), selected = names(diamonds1)[1])
    ),
    mainPanel(
      plotOutput("plot"),
      DTOutput("table")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$chartType == "Frequency Polygon") {
      ggplot(diamonds, mapping = aes(x = .data[[input$variable]], color = cut)) +
        geom_freqpoly(binwidth = 0.1) +
        ggtitle("Frequency Polygons") +
        labs(x = input$variable, y = "Count")
    } else if (input$chartType == "Boxplot") {
      ggplot(diamonds, aes(x = cut, y = .data[[input$variable]], fill = cut)) +
        geom_boxplot() +
        ggtitle("Boxplots of Variable against Cut") +
        xlab("Cut") +
        ylab(input$variable)
    }
  })
  
  output$table <- renderDT({
    diamonds1
  })
}

shinyApp(ui = ui, server = server)




#3
diamonds %>%
  select(carat, price, x, y, z) -> diamonds1
diamonds1

ui <- fluidPage(
  titlePanel("Diamond Analysis"),
  tags$head(tags$style(HTML("
    body {
      background-color: #f2f2f2;
    }
    .nav-pills .nav-link.active {
      background-color: #333333;
      color: #ffffff;
    }
  "))),
  tabsetPanel(
    id = "tabset",
    tabPanel("Frequency Polygon", 
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "variable1", label = "Variable", choices = names(diamonds1), selected = names(diamonds1)[1])),
               mainPanel(plotOutput("plot1")))),
    tabPanel("Boxplot",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "variable2", label = "Variable", choices = names(diamonds1), selected = names(diamonds1)[1])),
               mainPanel(plotOutput("plot2")))),
    tabPanel("Data Table", fluidRow(column(width = 12, DTOutput("table"))))))

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(diamonds, mapping = aes(x = .data[[input$variable1]], color = cut)) +
      geom_freqpoly(binwidth = 0.1) +
      ggtitle("Frequency Polygons") +
      labs(x = input$variable1, y = "Count")})
  
  output$plot2 <- renderPlot({
    ggplot(diamonds, aes(x = cut, y = .data[[input$variable2]], fill = cut)) +
      geom_boxplot() +
      ggtitle("Boxplots of Variable against Cut") +
      xlab("Cut") +
      ylab(input$variable2)})

  output$table <- renderDT({diamonds1
  })
}

shinyApp(ui = ui, server = server)

