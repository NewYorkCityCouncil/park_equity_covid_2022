# Park Equity Demographics Correlations

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

ct_grouped <- st_read("data/processed/ct_grouped.geojson") 

##################################################################################################################################
### Correlation Plots: Functional Acreage Per Capita

# Census Tract Park Access and Median Household Income
ggplot(ct_grouped, aes(x=rank(f_acre_pc), y=med_income)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Census Tract Park Access and Median Household Income",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Median Household Income", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
#  facet_wrap(~BoroName) + 
#  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# Census Tract Park Access and Foreign Born
ggplot(ct_grouped, aes(x=rank(f_acre_pc), y=perc_foreign)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Census Tract Park Access and Foreign Born",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Foreign Born", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BoroName) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# Census Tract Park Access and Non-Hispanic White
ggplot(ct_grouped, aes(x=rank(f_acre_pc), y=perc_nhwhite)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Census Tract Park Access and Non-Hispanic White",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Non-Hispanic White", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BoroName) + 
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

# Census Tract Park Access and Median Household Income
ggplot(ct_grouped, aes(x=rank(hrs_per_facre), y=med_income)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Census Tract Park Access and Median Household Income",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Median Household Income", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BoroName) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# Census Tract Park Access and Foreign Born
ggplot(ct_grouped, aes(x=rank(hrs_per_facre), y=perc_foreign)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Census Tract Park Access and Foreign Born",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Foreign Born", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BoroName) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# Census Tract Park Access and Non-Hispanic White
ggplot(ct_grouped, aes(x=rank(hrs_per_facre), y=perc_nhwhite)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Census Tract Park Access and Non-Hispanic White",
    x = "Least to Most Functional Acreage Per Capita (Rank)",
    y = "Non-Hispanic White", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  facet_wrap(~BoroName) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

# Census Tract Park Access and Hours Worked Per Capita
ggplot(ct_grouped, aes(x=f_acre_sum, y=avg_hrs_sum)) + 
  geom_point() + 
  labs(
    title = "Park Equity: Census Tract Park Access and Hours Worked Per Capita",
    x = "Functional Acreage",
    y = "Hours Worked", 
    caption = expression(paste(italic("Source: ACS; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
#  facet_wrap(~BoroName) + 
  geom_smooth(se = FALSE) +
  ggpubr::theme_pubr(
    base_size = 12,
    base_family = "",
    border = TRUE,
    margin = TRUE,
    legend = c("none"),
    x.text.angle = 0
  ) 

