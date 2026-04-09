
## LIBRARIES -----------------------------------------------
list.of.packages <- c("tidyverse", "janitor", "lubridate", "sf", "ggplot2", 
                      "leaflet", "leaflet.extras", "htmlwidgets", "RSocrata", 
                      "vroom", "mapview", "classInt", "openxlsx", "mapboxapi",
                      "skimr", "DBI", "censusapi", "readxl", "htmltools", 
                      "ggiraph", "gt", "gtExtras", "showtext", "reticulate", "dotenv"
)

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# load standard packages
lapply(list.of.packages, require, character.only = TRUE)


# setup Python councilcount via reticulate 
env_name <- "councilcount_env"

# if the virtual environment doesn't exist, create it and install dependencies
if (!virtualenv_exists(env_name)) {
  message("Creating Python virtual environment...")
  
  # try to create the environment; if Python 3.9 is missing, download it first
  tryCatch({
    virtualenv_create(env_name, version = "3.9")
  }, error = function(e) {
    message("Suitable Python not found. Downloading Python 3.9 (this may take a minute)...")
    reticulate::install_python(version = "3.9")
    virtualenv_create(env_name, version = "3.9")
  })
  
  message("Installing Python packages...")
  virtualenv_install(env_name, "pandas")
  virtualenv_install(env_name, "geopandas")
  virtualenv_install(env_name, "dotenv")
  virtualenv_install(env_name, "shapely")
  virtualenv_install(env_name, "councilcount")
}

# force R to use this specific environment
use_virtualenv(env_name, required = TRUE)

# import the package
cc <- import("councilcount")


# setup R councilcount
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
