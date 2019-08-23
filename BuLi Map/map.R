library(shiny)
library(ggmap)
library(mapproj)
library(rgdal)
library(tidyverse)
library(ggthemr)
library(ggpubr)
library(ggsci)
library(rsconnect)

germany <- readOGR(dsn = "~/Documents/GitHub Directory/ftw",
                   layer = "Germanypolygon21")

germany_data <- as_tibble(map_data(germany))

ggthemr('greyscale', layout = "scientific", text_size = 25)

ggplot(germany_data, aes(long, lat, group = group)) +
  geom_polygon(color = "white") + theme(axis.line = element_blank(), 
                                        axis.ticks = element_blank(),
                                        panel.grid = element_blank(), 
                                        axis.title = element_blank(),
                                        axis.text = element_blank(),
                                        legend.position = "right")

        
