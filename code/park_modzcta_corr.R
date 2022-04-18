# Park Equity Covid MODZCTA Correlations

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

modzcta_facre <- st_read("data/processed/modzcta_facre.geojson") %>%
  # Jackson Heights and Elmhurst
  mutate(Krishnan = ifelse(MODZCTA == "11372" | MODZCTA == "11373", 1, 0))

##################################################################################################################################
### Correlation Plots: Functional Acreage Per Capita

# MODZCTA Park Access and Median Household Income (SHOW)
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(facre_pc), y=MedInc, color=Krishnan)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Median Household Income",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Median Household Income", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  #facet_wrap(~BOROUGH_GROUP) + 
  #geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# add vertical for 150 rank, 150000k income

# Notes:
# For Brooklyn, positive relationship until 2 with the most facre_pc: East New York; Brighton Beach/Coney Island/Seagate

# MODZCTA Park Access and Born in USA
modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(facre_pc), y=100-ForeignBorn, color=Krishnan)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Born in USA",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Born in USA (%)", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE, color = "blue") +
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
ggplot(aes(x=rank(facre_pc), y=NH_White, color=Krishnan)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Non-Hispanic White",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Non-Hispanic White (%)", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE, color = "blue") +
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
ggplot(aes(x=rank(hrs_per_facre), y=MedInc, color=Krishnan)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Hours Worked Per Functional Acre and Median Household Income",
    x = "Least to Most Hours Worked Per Functional Acre (Rank)",
    y = "Median Household Income", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE, color = "blue") +
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
ggplot(aes(x=rank(hrs_per_facre), y=100-ForeignBorn, color=Krishnan)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Hours Worked Per Functional Acre and Born in USA",
    x = "Least to Most Hours Worked Per Functional Acre (Rank)",
    y = "Born in USA", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE, color = "blue") +
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
ggplot(aes(x=rank(hrs_per_facre), y=NH_White, color=Krishnan)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Hours Worked Per Functional Acre and Non-Hispanic White",
    x = "Least to Most Hours Worked Per Functional Acre (Rank)",
    y = "Non-Hispanic White", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE, color = "blue") +
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
ggplot(aes(x=rank(facre_pc), y=COVID_DEATH_RATE, color=Krishnan)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Covid Death Rate",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Covid Death Rate (Per 100,000)", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BOROUGH_GROUP) + 
  geom_smooth(se = FALSE, color = "blue") +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# MODZCTA Median Income and Covid Death Rate (SHOW)

modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
  ggplot(aes(x=MedInc, y=COVID_DEATH_RATE, color=Krishnan)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access and Covid Death Rate",
    x = "Median Income",
    y = "Covid Death Rate (Per 100,000)", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  #facet_wrap(~BOROUGH_GROUP) + 
  #geom_smooth(se = FALSE, color = "blue") +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 


# MODZCTA Park Access and Covid Death Rate and Median Income (SHOW)
mid<-median(modzcta_facre$MedInc, na.rm=TRUE)


modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
  ggplot(aes(x=rank(facre_pc), y=COVID_DEATH_RATE, color=MedInc)) + 
  geom_point() + 
  labs(
    title = "Park Equity: MODZCTA Park Access, Covid Death Rate, and Median Income",
    x = "Functional Acreage Per Capita (Rank)",
    y = "Covid Death Rate (Per 100,000)", 
    color = "Median Income",
    caption = expression(paste(italic("Source: ACS; NYC DOHMH; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  scale_color_steps2(midpoint=mid, low="red", mid="white",
                        high="blue", space ="Lab" )+
  #facet_wrap(~BOROUGH_GROUP) + 
  #geom_smooth(se = FALSE) +
  geom_vline(xintercept = 89, linetype = "dashed") +
  geom_hline(yintercept = median(modzcta_facre$COVID_DEATH_RATE, na.rm=TRUE), linetype = "dashed") +
  annotate("text", x = 25, y = 1350, label = "Low Park Access\nHigh Covid") +
  annotate("text", x = 150, y = 1350, label = "High Park Access\nHigh Covid") +
  annotate("text", x = 25, y = 50, label = "Low Park Access\nLow Covid") +
  annotate("text", x = 150, y = 50, label = "High Park Access\nLow Covid") +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("right"),
    x.text.angle = 0
  ) 

