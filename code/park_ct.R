# Park Equity Demographics

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

ct_acs <- st_read("data/processed/ct_acs.geojson") 
pm <- read.csv("data/processed/park_maintenance.csv", header = TRUE) %>%
  rename(pm_combine = property_number) %>%
  mutate(
    # 7,000,000 ft^2 = 160.6979 acres
    f_acre_cap = ifelse(functional_acreage > 160.6979, 160.6979, functional_acreage), 
    t_acre_cap = ifelse(total_acreage > 160.6979, 160.6979, total_acreage)
  )
cross_park <- read.csv("data/raw/pm_property_number_crosswalk.csv", header = TRUE)

# Import 10-Min Walk buffers made by isochrones.R and join is cross_park
iso <- st_read("data/processed/isochrones_10min_accesspts.geojson") %>%
  left_join(cross_park, by = c("gispropnum" = "property_number")) %>%
  # use gispropnum as ID unless it was combined in cross_park, then use pm_combine
  mutate(pm_combine = ifelse(is.na(pm_combine), gispropnum, pm_combine))

iso_pm <- iso %>%
  left_join(pm, by = "pm_combine") 

################################################################################################################################################

# 
ct_access <- ct_acs %>% 
  mutate(center = st_centroid(geometry, of_largest_polygon = TRUE)) %>% 
  st_drop_geometry() %>%
  st_as_sf() %>%
  st_join(iso_pm) %>%
  select(GEO_ID, pm_combine) %>%
  st_drop_geometry() %>%
  # some access points with no associated park
#  drop_na() %>%
  unique()

ct_full <- ct_access %>%
  left_join(pm, by = "pm_combine") %>%
  left_join(ct_acs, by = "GEO_ID")

ct_grouped <- ct_full %>%
  group_by(GEO_ID) %>%
  summarise(f_acre_sum = sum(f_acre_cap, na.rm = TRUE),
            f_acre_sum_uncap = sum(functional_acreage, na.rm = TRUE),
            f_acre_pc = f_acre_sum / S0101_C01_001E, 
            t_acre_sum = sum(t_acre_cap, na.rm = TRUE), 
            t_acre_sum_uncap = sum(total_acreage, na.rm = TRUE),
            t_acre_pc = t_acre_sum / S0101_C01_001E, 
            population = S0101_C01_001E, 
            avg_hrs_sum = sum(c(avghours_2021q1, avghours_2021q2, avghours_2021q3, avghours_2021q4), na.rm = TRUE),
            hrs_per_facre = avg_hrs_sum / sum(functional_acreage, na.rm = TRUE), 
            hrs_per_tacre = avg_hrs_sum / sum(total_acreage, na.rm = TRUE), 
            hrs_per_pc = avg_hrs_sum / S0101_C01_001E,
            med_income = S1901_C01_012E, 
            perc_foreign = (DP02_0093E / DP02_0087E) * 100, 
            perc_nhwhite = DP05_0077PE, 
            BoroName = BoroName
            )  %>%
  unique() %>%
  # 100 is just a guess at the appropriate population cutoff
  filter(population > 100) %>%
  right_join(ct_acs %>% select(GEO_ID, geometry), by = "GEO_ID") %>%
  st_as_sf() 

ct_grouped$hrs_per_facre <- ifelse(ct_grouped$hrs_per_facre == Inf, 
                           ct_grouped[is.finite(ct_grouped$hrs_per_facre), "hrs_per_facre"] %>% st_drop_geometry() %>% max(na.rm=TRUE) + 10, 
                           ct_grouped$hrs_per_facre)
  

st_write(ct_grouped, "data/processed/ct_grouped.geojson",  
         driver='GeoJSON', delete_dsn=TRUE)
