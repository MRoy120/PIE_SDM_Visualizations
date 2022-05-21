source("scripts/libraries_and_formatting.R")

low_SLR_combined_mit <- read_csv("data/scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, Years, `Fiddler Crabs`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_SLR_combined_mit_2 <- low_SLR_combined_mit %>%
  pivot_longer(cols = `Total Marsh`:`Low Marsh`,
               names_to = "Habitat Type",
               values_to = "areal_extent")

ggplot(data = low_SLR_combined_mit_2, 
       mapping = aes(x = time,
                     y = areal_extent,
                     color = `Habitat Type`,
                     linetype = `Habitat Type`)) +
  geom_line(size = 6,
            aes(linetype = `Habitat Type`)) +
  scale_y_continuous(limits = c(0, 60000)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  scale_linetype_manual(values = c("Total Marsh" = "solid",
                                   "Low Marsh" = "dashed",
                                   "High Marsh" = "dotted")) +
  labs(x = "Year",
       #y = "Marsh Extent\n(square meters)",
       title = "Low Sea Level Rise Rate",
       subtitle = "Crab Erosion, Erosion Reduction and Tidal Flood Mitigation") +
  ylab(expression(Marsh~Extent~(m^{"2"}))) +
  standard_theme +
  scale_color_manual(values = c("Total Marsh" = "darkgreen", 
                                "Low Marsh" = "forestgreen",
                                "High Marsh" = "limegreen"))

ggsave(filename = "figures/low_SLR_combined_mit.png",
       width = 22,
       height = 15)

ggplot(data = low_SLR_erosion_2, mapping = aes(x = time,
                                               y = `Crab Density`)) +
  geom_line(size = 4) +
  #           aes(linetype = `Habitat Type`)) +
  scale_y_continuous(limits = c(0, 300)) +
  labs(x = "Years",
       y = "Fiddler Crab\nDensity (crabs/square meter)") +
  theme_dark(base_size = 40,
             base_family = "Arial") +
  #scale_linetype_manual(values = c("dotted", "twodash", "solid")) +
  #scale_color_brewer(palette = "Spectral") +
  scale_color_continuous(type = "steelblue4") +
  theme(axis.title.x = element_text(size = 42, 
                                    face = "bold"),
        axis.title.y = element_text(size = 42, 
                                    face = "bold")#,
        # legend.title = element_text(size = 42, 
        #                             face = "bold"),
        # legend.key.size = unit(1.3, 'cm')
  ) +
  labs(title = "Fiddler Crab Density",
       subtitle = "Density of Minuca pugnax Influencing\nSalt Marsh Erosion at Low Sea Level Rise") +
  theme(plot.title = element_text(size = 42, 
                                  face = "bold"),
        plot.subtitle = element_text(size = 38,
                                     face = "bold")) +
  ggsave(filename = "scripts/simulation_visualization/figures/crab_density_erosion_low.png",
         width = 22,
         height = 15)




