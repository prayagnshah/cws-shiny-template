ui <- fluidPage(
  #theme = bslib::bs_theme(bootswatch = "yeti", version = 5),
  titlePanel("Shiny application template"),
  sidebarLayout(

    # Menu
    sidebarPanel(
      h4("Table filtering"),
      selectInput("species", label = "Species", choices = sort(unique(densities$Group)), multiple = TRUE, selected = sort(unique(densities$Group))[1]),
      selectInput("period", label = "Period", choices = sort(unique(densities$Month)), multiple = TRUE, selected = sort(unique(densities$Month))[1]),
      br(),
      h4("Spatial filtering"),
      sliderInput("lon", label = "Longitude", value = c(-93, -18), min = -93, max = -18),
      sliderInput("lat", label = "Latitude", value = c(36, 76), min = 36, max = 76),
      width = 3
    ),

    # Main panel of application
    mainPanel(
      tabsetPanel(
        # Panel 1
        tabPanel(
          "Species table",
          dataTableOutput("table")
        ),

        # Panel 2
        tabPanel(
          "Map",
          leafletOutput("map", width = "100%", height = "85vh")
        ),
        # Panel 3
        tabPanel(
          "Summary",
          fluidRow(
            column(
              4,
              h4("Number of species"),
              textOutput("nspecies")
            ),
            column(
              4,
              h4("Number of periods"),
              textOutput("nperiods")
            ),
            column(
              4,
              h4("Mean density"),
              textOutput("mn_density")
            )
          ),
          br(), br(), hr(),
          tableOutput("summary")
        )
      ),
      width = 9
    )
  )
)
