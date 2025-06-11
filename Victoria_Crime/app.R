library(shiny)
library(dplyr)
library(bslib)
library(ggplot2)
library(readxl) 
library(tidyr)
library(plotly)
library(glue)

crime_data <- read_excel("Offences_recorded_and_rate_per_100,000_population_by_offence_type.xlsx")

offence_divisions <- crime_data %>% select(`Offence Division`) %>% unique()
options(scipen=4000)

ui <- page_sidebar(
  titlePanel("The Victoria Crime Dive Application"),
  
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
    actionButton("reset", "Reset"),
    hr(),
    actionButton("about", "About")
  ),
  navset_card_tab( 
    nav_panel("Heatmap", 
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
  
  observe({ 
    showModal( 
      modalDialog( 
        title = "About the Victorian Crime Dive", 
        easy_close = TRUE, 
        "In order to improve living standards for a community, changes must be made 
        to minimise harmful variables, for example crime. However due to limited 
        resources, governments, homeowners and firms must make calculated decisions 
        on where they should allocate their time, money and labour to maximise positives. 
        The Victorian Crime Dive aims to assist in this goal by providing data that 
        can help audiences to make more informed decisions. If we as Victorians want to 
        minimise crime, we must first figure out what crimes are being commited, in order 
        to find the best correct solution to minimise that exact crime. In return, reducing 
        crime in Victoria." 
      ) 
    ) 
  }) |> 
    bindEvent(input$about) 
  
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
    title_text <- if (input$offence_subdivision != "-Empty-") {
      glue("Crime Trends in Subdivision: {input$offence_subdivision}")
    } 
    else if (input$offence_division != "-Empty-") {
      glue("Crime Trends in Division: {input$offence_division}")
    } 
    else {
      "Crime Trends Across All Divisions"
    }
    
    p <- ggplot(visualisation_data(), aes(x = Year, y = Offence)) + 
      geom_tile(aes(fill = year_total)) + 
      scale_fill_distiller(palette = "YlGnBu", direction = 1, name="Total") +
      labs(title = title_text,
           y = NULL) +
      scale_x_continuous(breaks = seq(2015, 2024, 1)) +
      theme_minimal() +
      theme(legend.key.width = unit(1, "cm"),
            panel.grid = element_blank(),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
      coord_equal()
    
    ggplotly(p) %>%
      layout(dragmode = FALSE) %>% # disables drag
      config(displayModeBar = FALSE) # hides the mode bar
  })
}

shinyApp(ui = ui, server = server)