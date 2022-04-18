# Park Equity Covid MODZCTA Correlations

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

modzcta_facre <- st_read("data/processed/modzcta_facre.geojson") 

##################################################################################################################################
### Correlation Plots: Functional Acreage Per Capita

# MODZCTA Park Access and Median Household Income
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(facre_pc), y=MedInc)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Median Household Income",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Median Household Income", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# Notes:
# For Brooklyn, positive relationship until 2 with the most facre_pc: East New York; Brighton Beach/Coney Island/Seagate

# MODZCTA Park Access and Born in USA
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(facre_pc), y=100-ForeignBorn)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Born in USA",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Born in USA (%)", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# MODZCTA Park Access and Non-Hispanic White
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(facre_pc), y=NH_White)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Non-Hispanic White",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Non-Hispanic White (%)", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

##################################################################################################################################
### Correlation Plots: Total Hours Per Functional Acreage

# MODZCTA Hours Worked Per Functional Acre and Median Household Income
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(hrs_per_facre), y=MedInc)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Hours Worked Per Functional Acre and Median Household Income",
    x = "Least to Most Hours Worked Per Functional Acre (Rank)",
    y = "Median Household Income", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# MODZCTA Hours Worked Per Functional Acre and Born in USA
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(hrs_per_facre), y=1-ForeignBorn)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Hours Worked Per Functional Acre and Born in USA",
    x = "Least to Most Hours Worked Per Functional Acre (Rank)",
    y = "Born in USA", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# MODZCTA Hours Worked Per Functional Acre and Non-Hispanic White
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(hrs_per_facre), y=NH_White)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Hours Worked Per Functional Acre and Non-Hispanic White",
    x = "Least to Most Hours Worked Per Functional Acre (Rank)",
    y = "Non-Hispanic White", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

##################################################################################################################################
### Correlation Plots: COVID

# MODZCTA Park Access and Covid Death Rate
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(facre_pc), y=COVID_DEATH_RATE)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Covid Death Rate",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Covid Death Rate", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
    geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 




