library(sf)
library(leaflet)
library(dplyr)

# Step 1: Read the GeoPackage file
setwd("C:/Users/Ramya Sri/Documents/MSCHDS/RESEARCH_PROJECT")
gpkg_file <- "Merged_data.gpkg"
geo_data <- st_read(gpkg_file)

# Step 2: Transform to longlat (WGS84)
geo_data <- st_transform(geo_data, crs = 4326)

# Step 3: (Optional) Simplify the geometry to improve performance
geo_data_simplified <- st_simplify(geo_data, dTolerance = 100)

# Step 4: Define a color palette based on a numeric column (e.g., vaccination percentage)
color_palette <- colorNumeric(
  palette = "PuRd",  # Change to pinks and purples
  domain = geo_data_simplified$Pr.C.C....
)

# Step 5: Create a leaflet map
leaflet(geo_data_simplified) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(
    fillColor = ~color_palette(Pr.C.C....),  # Apply color palette
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste0(CSO_LEA, ": ", Pr.C.C...., "%"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = color_palette, 
    values = ~Pr.C.C...., 
    opacity = 0.7, 
    title = "Vaccination Percentage",
    position = "bottomright"
  )
