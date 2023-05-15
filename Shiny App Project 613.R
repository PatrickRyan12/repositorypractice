library(shiny)
library(ggplot2)
library(dplyr)
data(CO2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: green;
      }
    "))
  ),
  titlePanel("CO2 Uptake Analysis By Patrick Ryan"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("treatments", "Select Treatments", choices = unique(CO2$Treatment)),
      sliderInput("timeRange", "Time Range", min = 1, max = nlevels(as.factor(CO2$Plant)), value = c(1, nlevels(as.factor(CO2$Plant)))),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Description", 
                 tags$div(
                   style = "padding: 20px;",
                   h3("CO2 Dataset Description"),
                   p("The CO2 dataset has the following variables:"),
                   tags$ul(
                     tags$li("Plant: A factor variable representing the plant identification number."),
                     tags$li("Type: A factor variable indicating the type of treatment applied to the plants."),
                     tags$li("Treatment: A factor variable indicating the specific treatment condition."),
                     tags$li("conc: The concentration of CO2 in parts per million (ppm)."),
                     tags$li("uptake: The CO2 uptake rate measured in units of mg/m^2/sec.")
                   ),
                   p("The dataset contains observations collected at different time points for various combinations of treatment and concentration levels. The CO2 uptake rate (uptake) was measured for each combination of plant, treatment, and concentration. the Time Range selection represents different calendar year quarters beginning with Q1 2013 (1) and Q4 2015 (12)")
                 )
        ),
        tabPanel("Average CO2 Uptake by Treatment", 
                 tags$p("Use the checkboxes above to select specific treatments and see the average CO2 uptake for each treatment:"),
                 plotOutput("plot1")
        ),       
        tabPanel("CO2 Uptake by Concentration", plotOutput("plot3")),
        tabPanel("Boxplot of CO2 Uptake by Treatment",
                 tags$p("Use the checkboxes above to select specific treatments and see the boxplot of CO2 uptake for each treatment:"),
                 plotOutput("plot4")),
        tabPanel("CO2 Uptake Comparison Between Plants", plotOutput("plot5"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    filtered_data <- CO2 %>% filter(Treatment %in% input$treatments)
    avg_uptake <- filtered_data %>% group_by(Treatment) %>% summarize(Average_Uptake = mean(uptake))
    ggplot(avg_uptake, aes(x = Treatment, y = Average_Uptake)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Treatment", y = "Average CO2 Uptake", title = "Average CO2 Uptake by Treatment")
  })
  
  output$plot3 <- renderPlot({
    ggplot(CO2, aes(x = conc, y = uptake)) +
      geom_point() +
      labs(x = "CO2 Concentration", y = "CO2 Uptake", title = "CO2 Uptake by Concentration")
  })
  
  output$plot4 <- renderPlot({
    filtered_data <- CO2 %>% filter(Treatment %in% input$treatments)
    if (nrow(filtered_data) > 0) {
      ggplot(filtered_data, aes(x = Treatment, y = uptake)) +
        geom_boxplot(fill = "steelblue") +
        labs(x = "Treatment", y = "CO2 Uptake", title = "Boxplot of CO2 Uptake by Treatment")
    } else {
      ggplot() +
        labs(title = "No data available")
    }
  })
  
  
  output$plot5 <- renderPlot({
    filtered_data <- CO2 %>% filter(as.integer(as.factor(Plant)) >= input$timeRange[1] & as.integer(as.factor(Plant)) <= input$timeRange[2])
    ggplot(filtered_data, aes(x = Plant, y = uptake, fill = Treatment)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Plant", y = "CO2 Uptake", title = "CO2 Uptake Comparison Between Plants")
  })
}

shinyApp(ui, server)