# Park Equity Demographics MODZCTA

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load crosswalks ------------------------------

# Crosswalk to get from census tract to zcta -------------
# https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-zcta-record-layout.html#par_textimage_3
ZNYC <- read.csv("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt?#") %>%
  # Only keep NYC
  filter(
    STATE=="36" & (COUNTY=="05" | COUNTY=="5" | COUNTY=="47" | COUNTY=="81" | COUNTY=="85" | COUNTY=="61")
  ) %>%
  mutate(GEO_ID = paste0("1400000US", GEOID))

# Crosswalk to get from zip to modzcta -------------
# add missing zip to crosswalk
modzcta_cross <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/ZCTA-to-MODZCTA.csv", header = TRUE) %>% 
  add_row(ZCTA = 11249, MODZCTA = 11211) %>%
  rename(ZCTA5 = ZCTA) %>%
  mutate(ZCTA5 = as.character(ZCTA5), 
         MODZCTA = as.character(MODZCTA))

################################################################################################################################################
### Load census and covid data 

### Load processed census tract acs and park maintenance data
ct_grouped <- st_read("data/processed/ct_grouped.geojson") 

### Load Covid data from NYC Health
url <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/data-by-modzcta.csv"
covid <- read.csv(url) %>%
  mutate(MODZCTA = as.character(MODIFIED_ZCTA))

# mod_zcta shapefile from DOHMH covid github ---------
mod_zcta <- read_sf("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/MODZCTA_2010_WGS1984.geo.json") %>% 
  st_transform("+proj=longlat +datum=WGS84")

covid_sf <- mod_zcta %>%
  left_join(covid, by = "MODZCTA")

################################################################################################################################################
### crosswalk from census tract to modzcta 

### census tract to zcta
Z_facre <- ZNYC %>%
  left_join(ct_grouped, by = "GEO_ID")

# ZCTA sqft is weighted average of sqft in each nested census tract 
for (i in unique(Z_facre$ZCTA5)){
  Z_facre[Z_facre$ZCTA5==i,"Z_facre"] <- sum(Z_facre[Z_facre$ZCTA5==i,"f_acre_sum"] * 
                                              Z_facre[Z_facre$ZCTA5==i,"ZPOPPCT"]/
                                            (sum(Z_facre[which(!is.na(Z_facre$f_acre_sum) & Z_facre$ZCTA5==i),"ZPOPPCT"]))
                                          , na.rm=TRUE)
  Z_facre[Z_facre$ZCTA5==i,"Z_facre"] <- ifelse(Z_facre[Z_facre$ZCTA5==i,"Z_facre"]==0, 
                                             Z_facre[Z_facre$ZCTA5==i,"f_acre_sum"], 
                                             Z_facre[Z_facre$ZCTA5==i,"Z_facre"])
  Z_facre[Z_facre$ZCTA5==i,"Pop_Add"] <- sum(Z_facre[Z_facre$ZCTA5==i,"POPPT"])
}



