library(shiny)
library(dplyr)
library(bslib)
library(ggplot2)
library(readxl) 
library(tidyr)
library(plotly)

crime_data <- read_excel("Offences_recorded_and_rate_per_100,000_population_by_offence_type.xlsx")

offence_divisions <- crime_data %>% select(`Offence Division`) %>% unique()
options(scipen=4000)

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
    actionButton("reset", "Reset")
    
  ),
  navset_card_tab( 
    nav_panel("Broad Dive", 
        plotlyOutput("heatmap", height = "545px", width = "100%")
    ),
    
    nav_panel("Sources", 
      fluidPage(
        tags$p(
          tags$strong("Title: "), 
          "Recorded offences visualisation year ending december 2024"
        ),
          tags$p(
            tags$strong("Organisation: "), 
            "Crime Statistics Agency"
        ),
          tags$p(
            tags$strong("Link: "),
              tags$a(href = "https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data", 
              "View the original article", target = "_blank")
        )
      )
    )
    
  )
)

server <- function(input, output, session) {
  
  #Reset drop downs
  observeEvent(input$reset, {
    updateSelectInput(session, "offence_division", selected = "-Empty-")
    updateSelectInput(session, "offence_subdivision", selected = "-Empty-")
  })
  
  #Display offence subdivision options
  observeEvent(input$offence_division, {
    filtered <- crime_data %>% filter(`Offence Division` == input$offence_division) %>% select(`Offence Subdivision`) %>% unique()
    
    updateSelectInput(session, "offence_subdivision", choices = c("-Empty-", filtered))
  })
  
  #Display offence subgroup options
  observeEvent(input$offence_subdivision, {
    filtered <- crime_data %>% filter(`Offence Subdivision` == input$offence_subdivision) %>% select(`Offence Subgroup`) %>% unique()
    updateSelectInput(session, "offence_subgroup", choices = c("-Empty-", filtered))
  })
  
  visualisation_data <- reactive({
    df <- NULL
    
    #Display subgroup data
    if (input$offence_subdivision != "-Empty-") {
      
      df <- crime_data %>% 
        group_by(Year, `Offence Subgroup`) %>%
        filter(`Offence Subdivision` == input$offence_subdivision) %>%
        summarise(year_total = sum(`Offence Count`)) %>%
        rename(Offence = `Offence Subgroup`)
      
    }
    #Display subdivision data
    else if (input$offence_division != "-Empty-") {
      
      df <- crime_data %>% 
        group_by(Year, `Offence Subdivision`) %>%
        filter(`Offence Division` == input$offence_division) %>%
        summarise(year_total = sum(`Offence Count`)) %>%
        rename(Offence = `Offence Subdivision`)
      
    }
    #Display division data
    else{
      
      df <- crime_data %>%
        group_by(Year, `Offence Division`) %>%
        summarise(year_total = sum(`Offence Count`)) %>%
        rename(Offence = `Offence Division`)
      
    }
  })
  
  #Heatmap
  output$heatmap <- renderPlotly({
    p <- ggplot(visualisation_data(), aes(x = Year, y = Offence)) + 
      geom_tile(aes(fill = year_total)) + 
      scale_fill_distiller(palette = "YlGnBu", direction = 1, name="Total") +
      scale_x_continuous(breaks = seq(2015, 2024, 1)) +
      theme_minimal() +
      theme(legend.key.width = unit(1, "cm"),
            panel.grid = element_blank()) + 
      coord_equal()
    
    ggplotly(p) %>%
      layout(dragmode = FALSE) %>% # disables drag
      config(displayModeBar = FALSE) # hides the mode bar
  })
}

shinyApp(ui = ui, server = server)
