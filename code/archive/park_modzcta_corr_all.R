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
    title = "Park Equity: Park Access and Median Household Income (MODZCTA)",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Median Household Income ($)", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  #facet_wrap(~BOROUGH_GROUP) + 
  #geom_smooth(se = FALSE) +
  geom_vline(xintercept = 150, linetype = "dashed") + 
  geom_hline(yintercept = 155000, linetype = "dashed") + 
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
    title = "Park Equity: Median Income and Covid Death Rate (MODZCTA)",
    x = "Median Income ($)",
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
  geom_point() + ggtitle("Park Equity & COVID", "Comparing Park Access, Covid Death Rates, and Median Income by Zipcode") +
  labs(
    x = "Park Access: Functional Acreage Per Capita (Rank)",
    y = "Covid Death Rate (Per 100,000)",
    color = "Median Income",
    caption = expression(paste(italic("Source: Census ACS; NYC DOHMH; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  scale_color_gradientn(
    labels = scales::dollar_format(),
    colours = c('#800000','#DD6C54','#E6E6E6','#AFB3D1','#7683BC','#2F56A6'),
    values = scales::rescale(c(50000,70000,100000, 150000, 200000, 250000) ) )+
  geom_vline(xintercept = 89, linetype = "dashed", color ="#666666") +
  geom_hline(yintercept = median(modzcta_facre$COVID_DEATH_RATE, na.rm=TRUE), linetype = "dashed", color ="#666666") +
  annotate("text", x = 25, y = 1350, label = "Low Park Access\nHigh Covid") +
  annotate("text", x = 125, y = 1350, label = "High Park Access\nHigh Covid") +
  annotate("text", x = 25, y = 50, label = "Low Park Access\nLow Covid") +
  annotate("text", x = 125, y = 50, label = "High Park Access\nLow Covid") +
  geom_smooth(se = FALSE, method = lm) +
  ggpubr::theme_pubr(
    base_size = 11,
    #base_family = "Open Sans",
    border = TRUE,
    margin = TRUE,
    legend = c("right"),
    x.text.angle = 0
  )

