library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(wesanderson)
library(dichromat)
library(RColorBrewer)

high_SLR_base <- read_csv("data/scenario_1_baseline/2021_Baseline_High_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, Years) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`)

high_SLR_base_2 <- high_SLR_base %>%
  pivot_longer(cols = `Total Marsh`:`Low Marsh`,
               names_to = "Habitat Type",
               values_to = "areal_extent")

ggplot(data = high_SLR_base_2, mapping = aes(x = time,
                                             y = areal_extent,
                                             color = `Habitat Type`)) +
  geom_line(size = 6,
            aes(linetype = `Habitat Type`)) +
  scale_y_continuous(limits = c(0, 50000)) +
  labs(x = "Years",
       y = "Marsh Extent\n(Square Meters)") +
  theme_dark(base_size = 40,
           base_family = "Arial") +
  scale_linetype_manual(values = c("dotted", "twodash", "solid")) +
  scale_color_brewer(palette = "Greens") +
  theme(axis.title.x = element_text(size = 42, 
                                    face = "bold"),
        axis.title.y = element_text(size = 42, 
                                    face = "bold"),
        legend.title = element_text(size = 42, 
                                    face = "bold"),
        legend.key.size = unit(3.5, 'cm')) +
  labs(title = "Plum Island Estuary Salt Marsh Extent",
       subtitle = "Marsh Area by Habitat Type at\nHigh Sea Level Rise Without Crabs or Mitigation Strategies") +
  theme(plot.title = element_text(size = 42, 
                                  face = "bold"),
        plot.subtitle = element_text(size = 38,
                                     face = "bold"),
        legend.position = "bottom") +
  ggsave(filename = "figures/high_SLR_baseline.png",
         width = 22,
         height = 15)
  
  
  
  
  
  
  