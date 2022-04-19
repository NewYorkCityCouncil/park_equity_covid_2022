# Park Equity park_accessMODZCTA Mapped

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
# map options defined

boro <- read_sf("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  st_simplify(dTolerance = .00001)

# color palette
pal = colorBin(
  # five green (positive)
  palette = c('#eaf5ef', '#b6d4bd', '#83b48d', '#4f9560', '#007534'),
  bins = c(0,25,80,150,450,4500),
  domain = modzcta_facre$facre_pc * 100000, 
  na.color = "#CACACA"
)
# park_access outline
recode <- modzcta_facre %>% 
  mutate(bottom25 = ifelse(facre_pc * 100000 < 25, 1, 0))

# source control
rr <- HTML('<small> Source: Census ACS Table, NYC Parks, NYC DOHMH </small>')

# park_accesslegend  
park_access<- HTML('<div style="color: #8744BC;"> <strong>Bottom 25%</strong> <br>  Park Access</div> <div><small>(Ten Minute Walk)</div>')

### leaflet map

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  # htmlwidgets::onRender("function(el, x) {
  #       L.control.zoom({ position: 'topright' }).addTo(this)
  #   }") %>%
  setView(-73.984865,40.710542,10.5) %>%
  setMapWidgetStyle(list(background= "white")) %>% 
  addPolygons(data = modzcta_facre, weight = 1, opacity = 0.5,
              color = ~pal(facre_pc * 100000), 
              fillColor = ~pal(facre_pc * 100000),
              fillOpacity = 0.9, popup = lapply(labels_facre,HTML)) %>% 
  addPolygons(data = recode, weight = 2, fill=F, opacity = 1,
              color = "#8744BC", stroke = recode$bottom25) %>% 
  addPolygons(data=boro, stroke = T, fill=F, color = "#666666", weight = 1) %>%
  addLegend(position ="topleft", pal = pal, 
            opacity = 0.9, values = modzcta_facre$facre_pc * 100000,
            title =  "<strong>Park Acreage:<strong><hr><small>Functional Acres <br> Per 100,000 Residents</small>") %>% 
  addControl(rr, position = "bottomright") %>% 
  addControl(park_access, position = "topleft") %>% 
  
  mapshot(file = "figures/acreage.png", 
          vwidth = 900, vheight = 870)


### Median Household Income ------------------------------

# park_access legend  
park_access<- HTML('<div style="color: #800000;"> <strong>Bottom 25%</strong> <br> Park Access</div> <div><small>(Ten Minute Walk)</div>')

pal = colorBin(
  palette = c('#d5dded', '#afb9db', '#8996ca', '#6175b8', '#2f56a6'),
  bins = c(0,55000,70000,95000,130000,260000),
  domain = modzcta_facre$MedInc, 
  na.color = "#CACACA"
)

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  # htmlwidgets::onRender("function(el, x) {
  #       L.control.zoom({ position: 'topright' }).addTo(this)
  #   }") %>%
  setView(-73.984865,40.710542,10.5) %>%
  setMapWidgetStyle(list(background= "white")) %>% 
  addPolygons(data = modzcta_facre, weight = 1, opacity = 0.5,
              color = ~pal(MedInc), 
              fillColor = ~pal(MedInc),
              fillOpacity = 0.9, popup = lapply(labels_facre,HTML)) %>% 
  addPolygons(data = recode, weight = 2, fill=F, opacity = 1,
              color = "#800000", stroke = recode$bottom25) %>% 
  addPolygons(data=boro, stroke = T, fill=F, color = "#666666", weight = 1) %>%
  addLegend(position ="topleft", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$MedInc,
            title =  "<strong>Median Household Income<hr>",
            labFormat = labelFormat(prefix = "$", big.mark = ",", between = ' &ndash;  $')) %>% 
  addControl(rr, position = "bottomright") %>% 
  addControl(park_access,position = "topleft") %>% 
  
  mapshot(file = "figures/median_income.png", 
          vwidth = 900, vheight = 870)



### Non-Hispanic White ------------------------------

park_access<- HTML('<div style="color: #8744BC;"> <strong>Bottom 25%</strong> <br> Park Access</div> <div><small>(As of April 2022)</div>')

pal = colorBin(
  # five green (positive)
  palette = c('#e6dfd3', '#cfbea5', '#b79e7a', '#9e7f4f', '#846126'),
  bins = c(0,15,35,60,70,92),
  domain = modzcta_facre$NH_White, 
  na.color = "#F9F9F9"
)


leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  # htmlwidgets::onRender("function(el, x) {
  #       L.control.zoom({ position: 'topright' }).addTo(this)
  #   }") %>%
  setView(-73.984865,40.710542,10.5) %>%
  setMapWidgetStyle(list(background= "white")) %>% 
  addPolygons(data = modzcta_facre, weight = 1, opacity = 0.5,
              color = ~pal(NH_White), 
              fillColor = ~pal(NH_White),
              fillOpacity = 0.9, popup = lapply(labels_facre,HTML)) %>% 
  addPolygons(data = recode, weight = 2, fill=F, opacity = 1,
              color = "#8744BC", stroke = recode$bottom25) %>% 
  addPolygons(data=boro, stroke = T, fill=F, color = "#666666", weight = 1) %>%
  addLegend(position ="topleft", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$NH_White,
            title =  "<strong>Non-Hispanic White<hr>",
            labFormat = labelFormat(suffix = "%",  between = '% &ndash; ')) %>%
  addControl(rr, position = "bottomright") %>% 
  addControl(park_access,position = "topleft") %>% 
  
  mapshot(file = "figures/NH_White.png", 
          vwidth = 900, vheight = 870)


### Covid Death Rate Per 100,000 Residents ----
park_access<- HTML('<div style="color: #800000;"> <strong>Bottom 25%</strong> <br> Park Access</div> <div><small>(Ten Minute Walk)</div>')

pal = colorBin(
  # five green (positive)
  palette = c('#d1dff6', '#afbdef', '#8b9ce7', '#617ddf', '#1d5fd6'),
  bins = c(10,350,450,550,650,1500),
  domain = modzcta_facre$COVID_DEATH_RATE, 
  na.color = "#CACACA"
)

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  # htmlwidgets::onRender("function(el, x) {
  #       L.control.zoom({ position: 'topright' }).addTo(this)
  #   }") %>%
  setView(-73.984865,40.710542,10.5) %>%
  setMapWidgetStyle(list(background= "white")) %>% 
  addPolygons(data = modzcta_facre, weight = 1, opacity = 0.5,
              color = ~pal(COVID_DEATH_RATE), 
              fillColor = ~pal(COVID_DEATH_RATE),
              fillOpacity = 0.9, popup = lapply(labels_facre,HTML)) %>% 
  addPolygons(data = recode, weight = 2, fill=F, opacity = 1,
              color = "#800000", stroke = recode$bottom25) %>% 
  addPolygons(data=boro, stroke = T, fill=F, color = "#666666", weight = 1) %>%
  addLegend(position ="topleft", 
            pal = pal, 
            opacity = 0.9,
            values = modzcta_facre$COVID_DEATH_RATE,
        title =  "<strong>COVID Death Rate</strong></br>Per 100,000 Residents<hr>") %>%
  addControl(rr, position = "bottomright") %>% 
  addControl(park_access, position = "topleft") %>% 
  
  mapshot(file = "figures/COVID_deaths.png", 
          vwidth = 900, vheight = 870)


