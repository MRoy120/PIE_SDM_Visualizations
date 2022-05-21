source("scripts/libraries_and_formatting.R")

moderate_SLR_erosion <- read_csv("data/scenario_2_crab_erosion/2021_Crab_Erosion_Moderate_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, Years, `Fiddler Crabs`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

moderate_SLR_erosion_2 <- moderate_SLR_erosion %>%
  pivot_longer(cols = `Total Marsh`:`Low Marsh`,
               names_to = "Habitat Type",
               values_to = "areal_extent")

ggplot(data = moderate_SLR_erosion_2, 
       mapping = aes(x = time,
                     y = areal_extent,
                     color = `Habitat Type`)) +
  geom_line(size = 6,
            aes(linetype = `Habitat Type`)) +
  scale_y_continuous(limits = c(0, 100000)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  scale_linetype_manual(values = c("dotted", "twodash", "solid")) +
  labs(x = "Year",
       #y = "Marsh Extent\n(square meters)",
       title = "Moderate Sea Level Rise Rate",
       subtitle = "Crab Erosion, No Mitigation") +
  ylab(expression(Marsh~Extent~(m^{"2"}))) +
  standard_theme +
  scale_color_manual("Habitat Type", values = c("Total Marsh" = "darkgreen", 
                                                "Low Marsh" = "forestgreen",
                                                "High Marsh" = "limegreen")) +
  ggsave(filename = "figures/moderate_SLR_crab_erosion.png",
         width = 22,
         height = 15)
