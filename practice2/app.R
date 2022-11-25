library(shiny)
library(DT)
library(leaflet)

densities <- read.csv("../data/densities.csv")
geo <- sf::st_read("../data/AtlasGrid-GrilleAtlas.gdb", layer = "AtlasGrid_GrilleAtlas") |>
       sf::st_transform(crs = 4326)


pal <- leaflet::colorBin("YlOrRd", domain = geo$density)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti", version = 5),
  titlePanel("Shiny application template"),
  sidebarLayout(
    
    # Menu 
    sidebarPanel(
      h4("Table filtering"),
      selectInput(
        "species", 
        label = "Species", 
        choices = sort(unique(densities$Group)), 
        multiple = TRUE, 
        selected = sort(unique(densities$Group))[1])
        ,
      selectInput(
        "period", 
        label = "Period", 
        choices = sort(unique(densities$Month)), 
        multiple = TRUE, 
        selected = sort(unique(densities$Month))[1]
      ),
      br(),
      h4("Spatial filtering"),
      sliderInput(
        "lon", 
        label = "Longitude", 
        value = c(-93, -18), 
        min = -93, 
        max = -18
      ),
      sliderInput(
        "lat", 
        label = "Latitude", 
        value = c(36, 76), 
        min = 36, 
        max = 76
      ),
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
        tabPanel("Map",
          leafletOutput("map", width = "100%", height = "85vh")
        )
      ),
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
  
  geo_filter <- reactive({
    # Create bounding box
    bbox <- c(
      xmin = input$lon[1], ymin = input$lat[1], 
      xmax = input$lon[2], ymax = input$lat[2]
    ) |>
    sf::st_bbox(crs = sf::st_crs(4326)) |>
    sf::st_as_sfc()
    
    # Intersect with atlas grid
    geo[bbox, ]
  })
  
  # Density of all birds selected during all seasons selected in specified bbox
  geo_data <- reactive({
    dat <- dplyr::group_by(
      densities_filter(),
      Stratum
    ) |>
    dplyr::summarize(Density = sum(Density, na.rm = TRUE))
    
    # Join with spatial data 
    dplyr::left_join(geo_filter(), dat, by = c("id" = "Stratum")) |>
    dplyr::select(Density)
  })
 
  output$table <- renderDataTable(
    densities_filter(),  
    rownames= FALSE, 
    options = list(pageLength = 10)
  )
  
  output$map <- renderLeaflet({
    # Color palette
    rgeo <- range(geo_data()$Density, na.rm = TRUE)
    pal <- leaflet::colorNumeric(
      viridis::viridis_pal(option = "D")(100), 
      domain = rgeo
    )
    
    # Map
    leaflet(geo_data()) |> 
    setView(lng = -55.5, lat = 60, zoom = 4) |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      opacity = 1,
      weight = 1, 
      color = ~pal(geo_data()$Density)) |>
    addLegend(
      position = "bottomright",
      pal = pal,
      values = seq(rgeo[1], rgeo[2], length.out = 5),
      opacity = 1,
      title = "Bird density"
      )
  })

}

shinyApp(ui, server)