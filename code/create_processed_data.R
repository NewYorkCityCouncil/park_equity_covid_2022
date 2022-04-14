# RUN ONLY IF UPDATING FILES OR CREATING TABLES FOR THE FIRST TIME
# ________________________________________________________________

################################################################################################################################################
### Import and Clean ACS Census Tract Data

# 2010 Census Tracts (Clipped to Shoreline)
# Use 2010 since data is from 2019, prior to new 2020 census tract boundaries 
# https://www1.nyc.gov/site/planning/data-maps/open-data/census-download-metadata.page
url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyct2010_22a.zip"
ct_shp <- sf::read_sf(unzip_sf(url)) %>%
  mutate(
    # match county numbers with those from the acs data below
    county = case_when(
      BoroCode == "1" ~ "061", 
      BoroCode == "2" ~ "005", 
      BoroCode == "3" ~ "047", 
      BoroCode == "4" ~ "081", 
      BoroCode == "5" ~ "085" 
    ), 
    # create GEO_ID to match acs data below
    GEO_ID = paste0("1400000US36", county, CT2010)
  )

### Demographics 
# Note: you must get a census API key to use getCensus functions

# Income data to pull from ACS
# https://api.census.gov/data/2019/acs/acs5/subject/variables.html
# Household median income: S1901_C01_012E
inc_pop_col <- c("NAME", "GEO_ID", "S0101_C01_001E", paste0("S1901_C01_0",formatC(1:13, width = 2, flag = "0"), "E")) 

# Income data at census tract level
ct_inc_pop <- getCensus(
  name = "acs/acs5/subject",
  vintage = 2019,
  vars = inc_pop_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

# Place of birth data to pull from ACS
# https://api.census.gov/data/2019/acs/acs5/profile/variables.html
# Foreign born: DP02_0093E
foreign_col <- c("NAME", "GEO_ID", "DP02_0087E", "DP02_0093E") 

# Foreign-born data at census tract level
ct_foreign <- getCensus(
  name = "acs/acs5/profile",
  vintage = 2019,
  vars = foreign_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")


# Race/eth data to pull from ACS
# https://api.census.gov/data/2019/acs/acs5/profile/groups/DP05.html
# Percent non-hispanic white: DP05_0077PE
race_col <- c("NAME", "GEO_ID", "DP05_0070E", "DP05_0077E", "DP05_0077PE") 

# Race/eth data at census tract level
ct_race <- getCensus(
  name = "acs/acs5/profile",
  vintage = 2019,
  vars = race_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

# Combine the census tract data with the census tract shapefile 
ct_acs <- ct_inc_pop %>%
  left_join(ct_foreign %>% select(!c("state", "county", "tract", "NAME")), by = "GEO_ID") %>%
  left_join(ct_race %>% select(!c("state", "county", "tract", "NAME")), by = "GEO_ID") %>%
  left_join(ct_shp %>% select(!c("county", "BoroCT2010", "BoroCode")), by = "GEO_ID") 
# Input NA for annotated values in numeric columns 
# https://www.census.gov/data/developers/data-sets/acs-1year/notes-on-acs-estimate-and-annotation-values.html
ct_acs[,6:24][ct_acs[,6:24] < 0] <- NA
ct_acs <- ct_acs %>% st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 

st_write(ct_acs, "data/processed/ct_acs.geojson",  
         driver='GeoJSON', delete_dsn=TRUE)

################################################################################################################################################
### Import and Clean Park Maintenance Data

# Data downloaded from Parks website (Local Law 98 of 2015)
# https://www.nycgovparks.org/news/archive
dl <- read_xlsx("data/raw/Fiscal-Year-2021-Park-Maintenance-Data__61e6f474469e3.xlsx") %>%
  clean_names()

pm <- dl %>%
  mutate(property_number = str_extract(property_number, "\\w+"), 
         fixedpost_2021q1 = as.factor(fixedpost_2021q1), 
         fixedpost_2021q2 = as.factor(fixedpost_2021q2), 
         fixedpost_2021q3 = as.factor(fixedpost_2021q3), 
         fixedpost_2021q4 = as.factor(fixedpost_2021q4))

write.csv(pm, "data/processed/park_maintenance.csv", row.names = FALSE)

################################################################################################################################################


