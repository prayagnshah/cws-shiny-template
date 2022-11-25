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

  summary_table <- reactive({
    dplyr::group_by(densities_filter(), Group, Month) |>
      dplyr::summarise(Density = round(sum(Density, na.rm = TRUE), 2)) |>
      tidyr::pivot_wider(names_from = Month, values_from = Density)
  })

  output$table <- renderDataTable(
    densities_filter(),
    rownames = FALSE,
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
        color = ~ pal(geo_data()$Density)
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = seq(rgeo[1], rgeo[2], length.out = 5),
        opacity = 1,
        title = "Bird density"
      )
  })

  output$nspecies <- renderText(length(input$species))
  output$nperiods <- renderText(length(input$period))
  output$mn_density <- renderText(round(mean(geo_data()$Density, na.rm = TRUE), 2))
  output$summary <- renderTable(
    summary_table(),
    rownames = FALSE,
    options = list(pageLength = 10)
  )
}
