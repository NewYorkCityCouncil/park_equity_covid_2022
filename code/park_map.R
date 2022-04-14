# Park Equity Demographics Mapped

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

ct_grouped <- st_read("data/processed/ct_grouped.geojson") 

################################################################################################################################################
### Demographic Maps

# 25%, 50%, 75%, 90%, 100%

# Median Household Income
pal = colorBin(
  # five red (positive)
  palette = "Greens",
  bins = c(0,48000,67000,87000,113000,260000),
  domain = ct_grouped$med_income, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_grouped,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal(med_income),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = ct_grouped$med_income,
            title =  "Median Household Income")


# Percent Born in USA
pal2 = colorBin(
  palette = "Greens",
  bins = c(15,50,65,75,80,100),
  domain = 100-ct_grouped$perc_foreign, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_grouped,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal2(100-perc_foreign),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal2, 
            opacity = 0.9,
            values = 100-ct_grouped$perc_foreign,
            title =  "Percent Born in USA")


# Percent Non-Hispanic White
pal3 = colorBin(
  # five red (positive)
  palette = "Greens",
  bins = c(0,5,25,60,75,100),
  domain = ct_grouped$perc_nhwhite, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_grouped,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal3(perc_nhwhite),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal3, 
            opacity = 0.9,
            values = ct_grouped$perc_nhwhite,
            title =  "Percent Non-Hispanic White")

################################################################################################################################################
### Functional Acreage Maps


# Functional Acreage
pal = colorBin(
  # five red (positive)
  palette = "Greens",
  bins = c(0,4,13,60,165,370),
  domain = ct_grouped$f_acre_sum, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_grouped,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal(f_acre_sum),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = ct_grouped$f_acre_sum,
            title =  "Functional Acres of</br>Parks Properties")


# Functional Acreage Per 1000 Residents 
pal2 = colorBin(
  # five red (positive)
  palette = "Greens",
  bins = c(0,1,4,18,45,1320),
  domain = (ct_grouped$f_acre_pc)*1000, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_grouped,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal2(f_acre_pc * 1000),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal2, 
            opacity = 0.9,
            values = ct_grouped$f_acre_pc * 1000,
            title =  "Functional Acres of Parks</br>Properties Per 1000 Residents")


# Average Hours Per Functional Acreage
pal3 = colorBin(
  # five red (positive)
  palette = "Greens",
  bins = c(0,10,25,50,75,820),
  domain = ct_grouped$hrs_per_facre, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_grouped,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal3(hrs_per_facre),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal3, 
            opacity = 0.9,
            values = ct_grouped$hrs_per_facre,
            title =  "Total Hours Per Functional</br>Acreage of Parks Properties")


##################################################################################################################################

# Isochrones and census tract centers 
leaflet(ct_acs %>% 
          mutate(center = st_centroid(geometry, of_largest_polygon = TRUE)) %>% 
          st_drop_geometry() %>%
          st_as_sf()) %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(color = "red", radius = 1) %>%
  addPolygons(data= iso_pm %>% filter(parkname == "Prospect Park"), 
              fill = TRUE, 
              weight = 1,
              opacity = 0.1,
              popup = as.character(iso_pm$gispropnum))

