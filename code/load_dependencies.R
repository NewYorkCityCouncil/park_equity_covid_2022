## LIBRARIES -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor", "lubridate", "sf", "ggplot2", 
                      "leaflet", "leaflet.extras", "htmlwidgets", "RSocrata", 
                      "vroom", "mapview", "classInt", "openxlsx", "mapboxapi",
                      "skimr", "DBI", "geoclient", "censusapi", "readxl")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)

# returns TRUE if package was loaded successfully

## FUNCTIONS -----------------------------------------

# this function unzips & stores .shp files only
unzip_sf <- function(zip_url) {
  temp <- tempfile()
  temp2 <- tempfile()
  #download the zip folder from the internet save to 'temp' 
  download.file(zip_url, temp)
  #unzip the contents in 'temp' and save unzipped content in 'temp2'
  unzip(zipfile = temp, exdir = temp2)
  #if returns "character(0), then .shp may be nested within the folder
  your_SHP_file <- ifelse(!identical(list.files(temp2, pattern = ".shp$",full.names=TRUE), character(0)), 
    list.files(temp2, pattern = ".shp$",full.names=TRUE), 
    list.files(list.files(temp2, full.names=TRUE), pattern = ".shp$", full.names = TRUE))
  unlist(temp)
  unlist(temp2)
  return(your_SHP_file)
}

# how to use
#  x <- sf::read_sf(unzip_sf("https://data.cityofnewyork.us/download/i8iw-xf4u/application%2Fzip"))


## LOAD IN COMMON DATASETS ------------------------------

# Crosswalk to get from zip to modzcta -------------
# add missing zip to crosswalk
crosswalk <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/ZCTA-to-MODZCTA.csv", header = TRUE) %>% 
  add_row(ZCTA = 11249, MODZCTA = 11211)

# mod_zcta shapefile from DOHMH covid github ---------
mod_zcta <- read_sf("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/Geography-resources/MODZCTA_2010_WGS1984.geo.json")

