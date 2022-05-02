# Park Equity Covid MODZCTA Interactive Correlations

################################################################################################################################################

source("code/load_dependencies.R")

################################################################################################################################################
### Load processed acs and park maintenance data

modzcta_facre <- st_read("data/processed/modzcta_facre.geojson") %>%
  # Jackson Heights and Elmhurst
  mutate(Krishnan = ifelse(MODZCTA == "11372" | MODZCTA == "11373", 1, 0))

##################################################################################################################################
### Correlation Plots

library(ggiraph)

mid<-median(modzcta_facre$MedInc, na.rm=TRUE)

m <- modzcta_facre %>%
  filter(Pop_Add_MODZCTA !=0)

plot <- 
  ggplot(data = m, 
         aes(x=rank(facre_pc), y=COVID_DEATH_RATE, color=MedInc)) +
  geom_point_interactive(
    tooltip = paste0(
      "Neighborhood (modzcta): ", m$NEIGHBORHOOD_NAME, " (", m$MODZCTA, ")", 
      "\n", 
      "Borough: ", m$BOROUGH_GROUP, 
      "\n", 
      "Park Access (Functional Acres Per 100,000 Residents): ", round(m$facre_pc * 100000, 1), 
      "\n", 
      "COVID-19 Death Rate (Per 100,000): ", round(m$COVID_DEATH_RATE, 0), 
      "\n", 
      "Median Income ($): ", scales::comma(round(m$MedInc, 0))
      )
    ) + 
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

plot_interactive <- ggiraph(ggobj = plot,   width_svg = 9,
        height_svg = 5,)

htmltools::save_html(plot_interactive, "figures/plot_interactive.html")

