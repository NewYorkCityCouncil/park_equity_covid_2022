# Park Equity Covid Mapped

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

ct_grouped <- st_read("data/processed/modzcta_facre.geojson") 

################################################################################################################################################
### Demographic Maps

# 25%, 50%, 75%, 90%, 100%

# Median Household Income
pal = colorBin(
  # five red (positive)
  palette = "Greens",
  bins = c(0,48000,67000,87000,113000,260000),
  domain = modzcta_facre$MedInc, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=modzcta_facre,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal(MedInc),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$MedInc,
            title =  "Median Household Income")


