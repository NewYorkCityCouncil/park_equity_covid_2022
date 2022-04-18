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
  mutate(GEO_ID = paste0("1400000US", GEOID)) %>%
  mutate(ZCTA5 = as.character(ZCTA5))

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

# ZCTA facre is weighted average of facre in each nested census tract 
for (i in unique(Z_facre$ZCTA5)){
  Z_facre[Z_facre$ZCTA5==i,"Z_facre"] <- sum(Z_facre[Z_facre$ZCTA5==i,"f_acre_sum"] * 
                                              Z_facre[Z_facre$ZCTA5==i,"ZPOPPCT"]/
                                            (sum(Z_facre[which(!is.na(Z_facre$f_acre_sum) & Z_facre$ZCTA5==i),"ZPOPPCT"]))
                                          , na.rm=TRUE)
  Z_facre[Z_facre$ZCTA5==i,"Z_facre"] <- ifelse(Z_facre[Z_facre$ZCTA5==i,"Z_facre"]==0, 
                                             Z_facre[Z_facre$ZCTA5==i,"f_acre_sum"], 
                                             Z_facre[Z_facre$ZCTA5==i,"Z_facre"]) 
  Z_facre[Z_facre$ZCTA5==i,"Z_hrs_per_facre"] <- sum(Z_facre[Z_facre$ZCTA5==i,"hrs_per_facre"] * 
                                               Z_facre[Z_facre$ZCTA5==i,"ZPOPPCT"]/
                                               (sum(Z_facre[which(!is.na(Z_facre$hrs_per_facre) & Z_facre$ZCTA5==i),"ZPOPPCT"]))
                                             , na.rm=TRUE)
  Z_facre[Z_facre$ZCTA5==i,"Pop_Add"] <- sum(Z_facre[Z_facre$ZCTA5==i,"POPPT"])
}

# Merge ZCTA to MODZCTA crosswalk with Z_facre data
MZtoZ_facre <- modzcta_cross %>%
  left_join(Z_facre[,c("ZCTA5", "Pop_Add", "Z_facre", "Z_hrs_per_facre")], by="ZCTA5") %>%
  left_join(zcta_acs[,c("ZCTA5", "S1901_C01_012E", "DP02_0093E", "DP02_0087E", "DP05_0077PE")], by="ZCTA5") %>%
  mutate(
    S1901_C01_012E = ifelse(S1901_C01_012E<=0, NA, S1901_C01_012E), 
    Foreign = (DP02_0093E / DP02_0087E) * 100
    ) %>%
  unique()

# MODZCTA facre is weighted average of facre in each nested ZCTA
for (j in MZtoZ_facre$MODZCTA){
  MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"facre"] <- sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Z_facre"] * 
                                                      MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]/
                                                  (sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]))
                                                , na.rm=TRUE)
  MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"hrs_per_facre"] <- sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Z_hrs_per_facre"] * 
                                                       MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]/
                                                       (sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]))
                                                     , na.rm=TRUE)
  MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add_MODZCTA"] <- sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"])
  MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"MedInc"] <- sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"S1901_C01_012E"] * 
                                                        MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]/
                                                    (sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]))
                                                  , na.rm=TRUE)
  MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"ForeignBorn"] <- sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Foreign"] * 
                                                        MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]/
                                                        (sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]))
                                                      , na.rm=TRUE)
  MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"NH_White"] <- sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"DP05_0077PE"] * 
                                                             MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]/
                                                             (sum(MZtoZ_facre[MZtoZ_facre$MODZCTA==j,"Pop_Add"]))
                                                           , na.rm=TRUE)
}

# Merge MZtoZ_facre data with MODZCTA shapefile
modzcta_facre <- covid_sf %>%
  left_join(MZtoZ_facre, by = "MODZCTA") %>%
  # create functional acres per capita if there is a population
  mutate(facre_pc = ifelse(Pop_Add_MODZCTA!=0, 
                            facre / Pop_Add_MODZCTA, 
                            NA), 
         # for the following, input NA if there is no population in the modzcta
         facre = ifelse(Pop_Add_MODZCTA!=0, 
                        facre, 
                        NA), 
         hrs_per_facre = ifelse(Pop_Add_MODZCTA!=0, 
                                hrs_per_facre, 
                                NA), 
         Pop_Add_MODZCTA = ifelse(Pop_Add_MODZCTA!=0, 
                                  Pop_Add_MODZCTA, 
                                  NA), 
         MedInc = ifelse(MedInc!=0, 
                         MedInc, 
                         NA), 
         ForeignBorn = ifelse(ForeignBorn!=0, 
                              ForeignBorn, 
                              NA), 
         NH_White = ifelse(NH_White!=0, 
                           NH_White, 
                           NA)
  ) %>%
  select(c("MODZCTA", "NEIGHBORHOOD_NAME", "BOROUGH_GROUP", "COVID_DEATH_RATE", "facre", "hrs_per_facre", "Pop_Add_MODZCTA", "MedInc", "ForeignBorn", "NH_White", "facre_pc")) %>%
  unique()

st_write(modzcta_facre, "data/processed/modzcta_facre.geojson",  
         driver='GeoJSON', delete_dsn=TRUE)


