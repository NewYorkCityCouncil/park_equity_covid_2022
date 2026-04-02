
## LIBRARIES -----------------------------------------------
list.of.packages <- c("tidyverse", "janitor", "lubridate", "sf", "ggplot2", 
                      "leaflet", "leaflet.extras", "htmlwidgets", "RSocrata", 
                      "vroom", "mapview", "classInt", "openxlsx", "mapboxapi",
                      "skimr", "DBI", "geoclient", "censusapi", "readxl", "htmltools", 
                      "ggiraph", "gt", "gtExtras", "remotes", "showtext", "ggiraph"
)

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# load standard packages
lapply(list.of.packages, require, character.only = TRUE)

# install and load custom councilcount package if not present
if (!require(councilcount)) {
  remotes::install_github("newyorkcitycouncil/councilcount")
  library(councilcount)
}


## FUNCTIONS -----------------------------------------

# this function unzips & stores .shp files only
unzip_sf <- function(zip_url) {
  temp <- tempfile()
  temp2 <- tempfile()
  download.file(zip_url, temp)
  unzip(zipfile = temp, exdir = temp2)
  your_SHP_file <- ifelse(!identical(list.files(temp2, pattern = ".shp$",full.names=TRUE), character(0)), 
                          list.files(temp2, pattern = ".shp$",full.names=TRUE), 
                          list.files(list.files(temp2, full.names=TRUE), pattern = ".shp$", full.names = TRUE))
  unlist(temp)
  unlist(temp2)
  return(your_SHP_file)
}
