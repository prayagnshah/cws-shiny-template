library(shiny)

densities <- read.csv("../data/densities.csv")

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti", version = 5),
  titlePanel("Shiny application template"),
  sidebarLayout(
    
    # Menu 
    sidebarPanel(
      h4("Table filtering"),
      selectInput("species", label = "Species", choices = sort(unique(densities$Group)), multiple = TRUE, selected = sort(unique(densities$Group))[1]),
      selectInput("period", label = "Period", choices = sort(unique(densities$Month)), multiple = TRUE, selected = sort(unique(densities$Month))[1]),
      width = 3
    ),
     
    # Main panel of application 
    mainPanel(
      dataTableOutput("table"),
      width = 9
    )
  )  
)

server <- function(input, output, session) {
  densities_filter <- reactive({
    dplyr::filter(
      densities, 
      Group %in% input$species,
      Month %in% input$period
    )
  })

 
  output$table <- renderDataTable(
    densities_filter(),  
    options = list(pageLength = 10)
  )
}

shinyApp(ui, server)