# Park Equity Covid MODZCTA Correlations

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

modzcta_facre <- st_read("data/processed/modzcta_facre.geojson") %>%
  # Jackson Heights and Elmhurst
  mutate(Krishnan = ifelse(MODZCTA == "11372" | MODZCTA == "11373", 1, 0))

##################################################################################################################################
### Correlation Plots

# MODZCTA Park Access and Non-Hispanic White --------
plot <- modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
ggplot(aes(x=rank(facre_pc), y=NH_White)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::percent_format(scale=1)) +
  ggtitle("Park Equity in NYC", "Comparing Park Acreage and Race for Every Zipcode") +
  labs(
    x = "Park Acreage\n (Functional Acreage Per Capita) \n Ranked from Least to Greatest",
    y = "Non-Hispanic White", 
    caption = expression(paste(italic("Source: Census ACS; NYC Parks: Walk-to-a-Park Service Area"))) ) +
 # geom_smooth(se = FALSE, color = "grey") +
  theme(legend.position="right", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
        text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(size = 11, 
          margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11, 
                     margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 11, 
                      margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, 
          margin = margin(t = 10, r = 0, b = 0, l = 0))) 

ggsave(plot, filename = "figures/acreage_race_scatterplot.png", 
       units = c("in"), width= 10, height= 6)

# MODZCTA Median Income and Covid Death Rate (SHOW) ----------
plot <- modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0) %>%
  ggplot(aes(x=MedInc, y=COVID_DEATH_RATE)) + 
  geom_point() + 
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_continuous(label = scales::dollar_format()) +
  ggtitle("Median Income and COVID Death Rate for Every Zipcode") +
  labs(x = "Median Income", y = "Covid Death Rate (Per 100,000)", 
    caption = expression(paste(italic("Source: Census ACS; DOHMH"))) ) +

  theme(legend.position="right", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
        text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(size = 11, 
                       margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11, 
                       margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 11, 
                         margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, 
                            margin = margin(t = 10, r = 0, b = 0, l = 0))) 

ggsave(plot, filename = "figures/income_covid_scatterplot.png", 
       units = c("in"), width= 10, height= 6)



# MODZCTA Park Access and Covid Death Rate and Median Income (SHOW) ---------
mid<-median(modzcta_facre$MedInc, na.rm=TRUE)

m <- modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0)

plot <- 
  ggplot(data = m, 
         aes(x=rank(facre_pc), y=COVID_DEATH_RATE, color=MedInc)) +
  geom_point() + 
  geom_vline(xintercept = median(rank(modzcta_facre$facre_pc), na.rm=TRUE),
             color ="#666666",linetype = "dashed") +
  geom_hline(yintercept = as.numeric(median(modzcta_facre$COVID_DEATH_RATE, na.rm=TRUE)),
             color ="#666666",linetype = "dashed") +
  geom_hline(aes(yintercept = as.numeric(median(modzcta_facre$COVID_DEATH_RATE, na.rm=TRUE)), linetype = "Median"), 
             colour = "#666666") + 
  scale_linetype_manual(name = NULL, values = 4) +
  scale_y_continuous(label = scales::comma_format()) +
  ggtitle("Park Equity in NYC", "Comparing Park Acreage, Covid Death Rates, and Median Income for Every Zipcode") +
  labs(
    x = "Park Acreage\n (Functional Acreage Per Capita) \n Ranked from Least to Greatest",
    y = "Covid Death Rate (Per 100,000)",
    color = "Median Income",
    caption = expression(paste(italic("Source: Census ACS; NYC DOHMH; NYC Parks: Walk-to-a-Park Service Area")))
  ) +
  scale_color_gradientn(
    labels = scales::dollar_format(),
    colours = c('#800000','#DD6C54','#E6E6E6','#AFB3D1','#7683BC','#2F56A6'),
    values = scales::rescale(c(50000,70000,100000, 150000, 200000, 250000) ) )+

  annotate("text", x = 25, y = 1350, label = "Low Park Access\nHigh Covid") +
  annotate("text", x = 125, y = 1350, label = "High Park Access\nHigh Covid") +
  annotate("text", x = 25, y = 50, label = "Low Park Access\nLow Covid") +
  annotate("text", x = 125, y = 50, label = "High Park Access\nLow Covid") +
  
  theme(legend.position="right", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
        text = element_text(family = "Open Sans"),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(size = 11, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0))) 

ggsave(plot, filename = "figures/acreage_income_covid_scatterplot.png", 
       units = c("in"), width= 10, height= 6)


