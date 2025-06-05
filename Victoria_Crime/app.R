library(shiny)

ui <- fluidPage(
  titlePanel("My App"),
  sidebarLayout(
    sidebarPanel(
      # Inputs
    ),
    mainPanel(
      # Outputs
    )
  )
)

server <- function(input, output, session) {
  # Server logic
}

shinyApp(ui = ui, server = server)
