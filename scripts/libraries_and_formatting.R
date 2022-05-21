# Libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(wesanderson)
library(dichromat)
library(RColorBrewer)
library(patchwork)
library(ggpubr)

# Formatting
standard_theme <- theme(axis.title.x = element_text(size = 24),
                        axis.title.y = element_text(size = 24),
                        legend.title = element_text(size = 24, 
                                                    face = "bold"),
                        legend.key.width = unit(3, 'cm'),
                        legend.key.height = unit(1.8, 'cm'),
                        plot.title = element_text(size = 24,
                                                  face = "bold"),
                        plot.subtitle = element_text(size = 20),
                        legend.position = "right",
                        legend.direction = "vertical")





