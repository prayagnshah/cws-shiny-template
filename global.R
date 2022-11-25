library(shiny)
library(DT)
library(leaflet)

densities <- read.csv("data/densities.csv")
species <- read.csv("data/species.csv")
geo <- sf::st_read("data/AtlasGrid-GrilleAtlas.gdb", layer = "AtlasGrid_GrilleAtlas") |>
  sf::st_transform(crs = 4326)

pal <- leaflet::colorBin("YlOrRd", domain = geo$density)
