# Park Equity Covid 2022

source("code/load_dependencies.R")

#token for api is needed

mb_access_token(my_token, install = TRUE)

################################################################################################################################################

# https://data.cityofnewyork.us/Recreation/Walk-to-a-Park-Service-area/5vb5-y6cv
access <- sf::read_sf(unzip_sf("https://data.cityofnewyork.us/api/geospatial/5vb5-y6cv?method=export&format=Shapefile")) %>%
  st_transform("+proj=longlat +datum=WGS84")

# https://data.cityofnewyork.us/Recreation/Parks-Properties/enfh-gkve
parks_prop <- sf::read_sf(unzip_sf("https://data.cityofnewyork.us/api/geospatial/enfh-gkve?method=export&format=Shapefile")) %>%
  st_transform("+proj=longlat +datum=WGS84")

sf_use_s2(FALSE)
access_os <- st_join(access, parks_prop, join = nngeo::st_nn, maxdist = 15.24)

access_os <- access_os %>%
  mutate(gispropnum = ifelse(is.na(gispropnum.x), gispropnum.y, gispropnum.x)) %>%
  select(gispropnum, parkname, type, geometry)

st_write(access_os, "data/processed/access_os.geojson",  
         driver='GeoJSON', delete_dsn=TRUE)

################################################################################################################################################

    
isos=list()
system.time(
for(i in 1:nrow(access_os)){
  Sys.sleep(0.5)
  isos[[i]] <- mb_isochrone(location = c(st_coordinates(access_os[i,])[1], 
                                       st_coordinates(access_os[i,])[2]),
                            profile = "walking",
                            time = 10)
  print(i)
}
)

df <- rbind.fill(isos)
df <- cbind(df,access_os %>% st_set_geometry(NULL)) %>%  st_as_sf()

st_write(df, "data/processed/isochrones_10min_accesspts.geojson",  
         driver='GeoJSON', delete_dsn=TRUE)


