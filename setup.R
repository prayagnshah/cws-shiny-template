# Get data used for the shiny application
# Atlas of Seabirds at Sea in Eastern Canada 2006-2016
# https://open.canada.ca/data/en/dataset/f612e2b4-5c67-46dc-9a84-1154c649ab4e

# Declare resources
resources <- list(
  gdb = list(
    file = "AtlasGrid-GrilleAtlas.gdb.zip",
    location = "http://data.ec.gc.ca/data/species/assess/atlas-of-seabirds-at-sea-in-eastern-canada-2006-2016/AtlasGrid-GrilleAtlas.gdb.zip",
    outputdir = "data"
  ),
  data = list(
    file = "DensityData-DonneesDeDensite.xlsx",
    location = "http://data.ec.gc.ca/data/species/assess/atlas-of-seabirds-at-sea-in-eastern-canada-2006-2016/DensityData-DonneesDeDensite.xlsx",
    outputdir = "data"
  ),
  dictionnary = list(
    file = "DataDictionary-DictionnaireDeDonnees.xlsx",
    location = "http://data.ec.gc.ca/data/species/assess/atlas-of-seabirds-at-sea-in-eastern-canada-2006-2016/DataDictionary-DictionnaireDeDonnees.xlsx",
    outputdir = "data"
  )
)

for (resource in resources) {
  if (!file.exists(resource$outputdir)) dir.create(resource$outputdir)
  if (!file.exists(paste(resource$outputdir, resource$file, sep = "/"))) {
    download.file(resource$location, destfile = paste(resource$outputdir, resource$file, sep = "/"))
  }
  if (grepl("zip", resource$file)) {
    utils::unzip(paste(resource$outputdir, resource$file, sep = "/"), exdir = resource$outputdir)
  }
}

# Import species dictionnary and simplify
species <- readxl::read_excel(paste(resources$dictionnary$outputdir, resources$dictionnary$file, sep = "/"), "Species-Espèces") |>
  dplyr::select(Species_ID, English_Name, Scientific_Name, Family_Sci)

# Import density data and simplify
densities <- readxl::read_excel(paste(resources$data$outputdir, resources$data$file, sep = "/"), "Densities") |>
  dplyr::select(Group, Month, Stratum, Density) |>
  dplyr::mutate(
    Month = replace(Month, Month == "04050607", "April-July"),
    Month = replace(Month, Month == "08091011", "August-November"),
    Month = replace(Month, Month == "12010203", "December-March")
  ) |>
  dplyr::filter(Group %in% species$Species_ID)

# Import effort data and simplify
effort <- readxl::read_excel(paste(resources$data$outputdir, resources$data$file, sep = "/"), "Effort") |>
  dplyr::select(Stratum, Month, nbspecies, nbobs, nbind, nbsamples, nbkm, nbcruiseID, nbdays) |>
  dplyr::mutate(
    Month = replace(Month, Month == "04050607", "April-July"),
    Month = replace(Month, Month == "08091011", "August-November"),
    Month = replace(Month, Month == "12010203", "December-March")
  ) |>
  tidyr::pivot_longer(cols = nbspecies:nbdays)


# Export as csv
write.csv(densities, "data/densities.csv", row.names = FALSE)
write.csv(species, "data/species.csv", row.names = FALSE)
write.csv(effort, "data/effort.csv", row.names = FALSE)
