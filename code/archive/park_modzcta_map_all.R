# Park Equity Covid MODZCTA Mapped

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

modzcta_facre <- st_read("data/processed/modzcta_facre.geojson") 

################################################################################################################################################
### Demographic Maps

# labels for all the maps
labels_facre <- paste("<h3>","Neighborhood: ", modzcta_facre$NEIGHBORHOOD_NAME, "</h3>",
                      "<p>","(Modified) Zip Code: ", modzcta_facre$MODZCTA, "</p>",
                      "<p>","Functional Acres: ", round(modzcta_facre$facre, 1),"</p>", 
                      "<p>","Functional Acres Per 10,000 Residents: ", round(modzcta_facre$facre_pc * 100000, 1),"</p>",
                      "<p>",paste0("Hours Worked Per Functional Acre: ", round(modzcta_facre$hrs_per_facre, 1)),"</p>",
                      "<p>",paste0("Population: ", round(modzcta_facre$Pop_Add_MODZCTA, 0)),"</p>", 
                      "<p>",paste0("Median Income: ", round(modzcta_facre$MedInc, 0)),"</p>", 
                      "<p>",paste0("Born in USA (%): ", round(100-modzcta_facre$ForeignBorn, 1)),"</p>", 
                      "<p>",paste0("Non-Hispanic White (%): ", round(modzcta_facre$NH_White, 1)),"</p>"
                      
)

# each pal uses: ~ 25%, 50%, 75%, 90%, 100%
# ex: quantile(modzcta_facre$MedInc, seq(0,1,0.05), na.rm = TRUE)
# ex: quantile(modzcta_facre$facre_pc * 100000, seq(0,1,0.05), na.rm = TRUE)

### Functional Acres Per 100,000 Residents ------------------------------ (SHOW)
pal = colorBin(
  # five green (positive)
  palette = "Greens",
  bins = c(0,25,80,150,450,4500),
  domain = modzcta_facre$facre_pc * 100000, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=modzcta_facre %>% mutate(bottom25 = ifelse(facre_pc * 100000 < 25, 1, 0)),
              weight = 1,
              color = "red",
              stroke = (modzcta_facre%>% mutate(bottom25 = ifelse(facre_pc * 100000 < 25, 1, 0)))$bottom25,
              fillColor = ~pal(facre_pc * 100000),
              fillOpacity = 0.9, 
              popup = lapply(labels_facre,HTML)) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$facre_pc * 100000,
            title =  "Functional Acres Per 100,000 Residents")


### Median Household Income ------------------------------
pal = colorBin(
  # five green (positive)
  palette = "Greens",
  bins = c(0,55000,70000,95000,130000,260000),
  domain = modzcta_facre$MedInc, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=modzcta_facre %>% mutate(bottom25 = ifelse(facre_pc * 100000 < 25, 1, 0)),
              weight = 1,
              color = "red",
              stroke = (modzcta_facre%>% mutate(bottom25 = ifelse(facre_pc * 100000 < 25, 1, 0)))$bottom25,
              fillColor = ~pal(MedInc),
              fillOpacity = 0.9, 
              popup = lapply(labels_facre,HTML)) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$MedInc,
            title =  "Median Household Income")


### Born in USA ------------------------------
pal = colorBin(
  # five green (positive)
  palette = "Greens",
  bins = c(29,55,65,75,80,97),
  domain = 100-modzcta_facre$ForeignBorn, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=modzcta_facre,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal(100-ForeignBorn),
              fillOpacity = 0.9, 
              popup = lapply(labels_facre,HTML)) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = 100-modzcta_facre$ForeignBorn,
            title =  "Born in USA")


### Non-Hispanic White ------------------------------
pal = colorBin(
  # five green (positive)
  palette = "Greens",
  bins = c(0,15,35,60,70,92),
  domain = modzcta_facre$NH_White, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=modzcta_facre,
              weight = 1,
              color = "grey",
              stroke = TRUE,
              fillColor = ~pal(NH_White),
              fillOpacity = 0.9, 
              popup = lapply(labels_facre,HTML)) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$NH_White,
            title =  "Non-Hispanic White")

### Covid Death Rate Per 100,000 Residents

pal = colorBin(
  # five green (positive)
  palette = "Greens",
  bins = c(10,350,450,550,650,1500),
  domain = modzcta_facre$COVID_DEATH_RATE, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=modzcta_facre %>% mutate(bottom25 = ifelse(facre_pc * 100000 < 25, 1, 0)),
              weight = 1,
              color = "red",
              stroke = (modzcta_facre %>% mutate(bottom25 = ifelse(facre_pc * 100000 < 25, 1, 0)))$bottom25,
              fillColor = ~pal(COVID_DEATH_RATE),
              fillOpacity = 0.9, 
              popup = lapply(labels_facre,HTML)) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$COVID_DEATH_RATE,
            title =  "Covid Death Rate Per 100,000 Residents")

### Hours Worked Per Functional Acre

pal = colorBin(
  # five green (positive)
  palette = "Greens",
  bins = c(0,15,25,45,70,200),
  domain = modzcta_facre$hrs_per_facre, 
  na.color = "Grey"
)

leaflet() %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=modzcta_facre,
              weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal(hrs_per_facre),
              fillOpacity = 0.9, 
              popup = lapply(labels_facre,HTML)) %>% 
  addLegend(position ="bottomright", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$hrs_per_facre,
            title =  "Hours Worked Per</br>Functional Acre (2021)")

