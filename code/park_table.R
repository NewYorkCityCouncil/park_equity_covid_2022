# Park Equity Covid MODZCTA Tables

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

modzcta_facre <- st_read("data/processed/modzcta_facre.geojson")

##################################################################################################################################
### Table

quantile(modzcta_facre$facre_pc * 100000, seq(0,1,0.05), na.rm = TRUE)
quantile(modzcta_facre$COVID_DEATH_RATE, seq(0,1,0.05), na.rm = TRUE)

# sorted by covid-19 death rates
modzcta_facre %>% 
  filter(facre_pc <= 0.00022 & COVID_DEATH_RATE >= 500) %>% 
  st_drop_geometry() %>% 
  mutate(
    Neighborhood = NEIGHBORHOOD_NAME, 
    Borough = BOROUGH_GROUP, 
    'Covid-19 Deaths Per 100,000' = round(COVID_DEATH_RATE, 0),
    'Acres Per 100,000' = round(facre_pc * 100000, 1)
  ) %>%
  select(
    Neighborhood, Borough, 'Covid-19 Deaths Per 100,000', 'Acres Per 100,000'
  ) %>%
  arrange(desc(`Covid-19 Deaths Per 100,000`)) %>% 
  gt() %>%
  tab_header(
    title = "Park Equity & COVID-19 ",
    subtitle = "Neighborhoods with High COVID-19 Death Rates and Low Park Access Per Capita"
  ) %>%
#  tab_source_note(source_note = "") %>%
  gt_theme_nytimes() %>% 
  
  gtsave("figures/table_covid_500.pdf")


# sorted by park access
sort_park <- modzcta_facre %>% 
  filter(facre_pc <= 0.00022 & COVID_DEATH_RATE >= 500) %>% 
  st_drop_geometry() %>% 
  mutate(
    Neighborhood = NEIGHBORHOOD_NAME, 
    Borough = BOROUGH_GROUP, 
    'Covid-19 Deaths Per 100,000' = round(COVID_DEATH_RATE, 0),
    'Acres Per 100,000' = round(facre_pc * 100000, 1)
  ) %>%
  select(
    Neighborhood, Borough, 'Acres Per 100,000', 'Covid-19 Deaths Per 100,000' 
  ) %>%
  arrange(`Acres Per 100,000`) %>% 
  gt() %>%
  tab_header(
    title = "Park Equity & COVID-19 ",
    subtitle = "Neighborhoods with Low Park Access Per Capita and High COVID-19 Death Rates"
  ) %>%
#  tab_source_note(source_note = "") %>%
  gt_theme_nytimes() %>%
  
  gtsave("figures/table_acres_500.pdf")
  

