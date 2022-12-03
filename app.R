library(shiny)
library(sf)
library(leaflet)


geo <- sf::st_read("data/AtlasGrid-GrilleAtlas.gdb", layer = "AtlasGrid_GrilleAtlas") |>
  sf::st_transform(crs = 4326)

#Global environment

densities <- read.csv("data/densities.csv")
head(densities)

species <- read.csv("data/species.csv")
head(species)
tail(species)



ui <- fluidPage (
  
  ##Title of the application
  titlePanel("Welcome to the world of species"),
  
  ##Trying to make more pleasing using the themes
  theme = bslib::bs_theme(bootswatch = "yeti", version = 5),
  
  sidebarLayout(
    
    
    #Menu
    sidebarPanel(
      
      ##sorting the filtered data into alphabetical order
      selectInput("Group", label = "Species", choices = sort(unique(densities$Group))),
      selectInput("Month", label = "Season", choices = sort(unique(densities$Month))),
      
      ##showing check box of show row names where we can avoid duplicates by having value false
      checkboxInput("showrow", "Show row names", value = FALSE),
      
      br(), ##Adding a new line
      
      h4("Spatial Filtering"),
      
      ##Adding the slider input
      sliderInput("lon", label = "Longitude", value = c(-93,-18),min = -93, max = -18),
      sliderInput("lat", label = "Latitude", value = c(36,76),min = 36, max = 76),
      
      width = 3
      
    ),
    ##Main panel for outputs 
    mainPanel(
      tabsetPanel(
        
        #Panel 1
        tabPanel(
          "Table", 
          dataTableOutput("table")
        ),
        
        #Panel 2
        tabPanel(
          "Map", 
          leafletOutput("map", width = "100%")
        )
      ),
      width = 9
    )
  )
)







##Server

server <- function(input, output, session) {
  
  ## Reactive to filter density table
  densities_filter <- reactive({
    
    ##using library to filter it on the website when user selects the group
    ##this means whole table of densities will print and group and month values are changed so table values will change
    dplyr::filter(
      densities,
      Group %in% densities$Group,
      Month %in% densities$Month
    )
    
  })
  
  geo_filter <- reactive({
    bbox <- c(
      xmin = input$lon[1], ymin = input$lat[1],
      xmax = input$lon[2],ymax = input$lat[2]
    ) |>
      sf::st_bbox(crs = sf::st_crs(4326)) |>
      sf::st_as_sfc()
    
    ##Intersect grid with bbox
    geo[bbox, ]
    
  })
  
  ##Need to understand densities_filter
  ##using na.rm = True means excluding NA values if any 
  ##|> means that LHS will work first and then RHS
  ##x %>% f(y) converted into f(x, y)
  
  geo_data <- reactive({
    dat <- dplyr::group_by(
      densities_filter(),
      Stratum
    ) |>
      dplyr::summarize(Density = sum(Density, na.rm = TRUE))
    
    
    ## Join to spatial data
    ##need to understand the geo_filter
    ##select() means it will select those particular coloumns
    dplyr::left_join(geo_filter(), dat, by = c("id" = "Stratum")) |>
    dplyr::select(Density)
  })
  
  
  ##Table output 
  output$table <- renderDataTable(
    densities_filter(),
    options = list(pageLength = 10)
  )
  
  output$map <- renderLeaflet({
    # Color palette
    ##range function will take geo_data as the argument and na.rm will eliminate NA values
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

##Call to shiny
shinyApp(ui, server) 

