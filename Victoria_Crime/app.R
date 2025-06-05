library(shiny)
library(dplyr)
library(bslib)
library(ggplot2)
library(readxl) 
library(tidyr)

crime_data <- read_excel("Offences_recorded_and_rate_per_100,000_population_by_offence_type.xlsx")

offence_divisions <- crime_data %>% select(`Offence Division`) %>% unique()

visualisation_data <- crime_data %>% group_by(`Year`) %>% summarise(year_total = sum(`Offence Count`))

ui <- page_sidebar(
  titlePanel("The Victoria Crime Dive"),
  sidebar = sidebar(
    selectInput(
      "offence_division",
      label = "Choose a offence division",
      choices = c("-Empty-", offence_divisions),
      ),
    selectInput(
      "offence_subdivision",
      label = "Choose a offence subdivision",
      choices = c("-Empty-"),
    ),
    selectInput(
      "offence_subgroup",
      label = "Choose a offence subgroup",
      choices = c("-Empty-"),
    ),
  ),
  mainPanel(
    plotOutput("ggplot", height = "545px", width = "100%")
  )
)

server <- function(input, output, session) {
  
  #Display offence subdivision options after offence division selection
  observeEvent(input$offence_division, {
    filtered <- crime_data %>% filter(`Offence Division` == input$offence_division) %>% select(`Offence Subdivision`) %>% unique()
    updateSelectInput(session, "offence_subdivision", choices = c("-Empty-", filtered))
    
    visualisation_data <- crime_data %>% group_by(`Year`) %>% filter(`Offence Division` == input$offence_division) %>% summarise(year_total = sum(`Offence Count`))
  })
  
  #Display offence subgroup options after offence subdivision selection
  observeEvent(input$offence_subdivision, {
    filtered <- crime_data %>% filter(`Offence Subdivision` == input$offence_subdivision) %>% select(`Offence Subgroup`) %>% unique()
    updateSelectInput(session, "offence_subgroup", choices = c("-Empty-", filtered))
    
    visualisation_data <- crime_data %>% group_by(`Year`) %>% filter(`Offence Subdivision` == input$offence_subdivision) %>% summarise(year_total = sum(`Offence Count`))
  })
  
  observeEvent(input$offence_subgroup, {
    visualisation_data <- crime_data %>% group_by(`Year`) %>% filter(`Offence Subgroup` == input$offence_subgroup) %>% summarise(year_total = sum(`Offence Count`))
  })
  
  #output$ggplot <- renderPlot({
  #  ggplot(visualisation_data, aes(x=Year, y=year_total)) +
  #    geom_col()
  #})
}

shinyApp(ui = ui, server = server)
