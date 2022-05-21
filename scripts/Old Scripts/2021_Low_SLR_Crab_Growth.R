source("scripts/libraries_and_formatting.R")

low_SLR_crab_growth <- read_csv("data/scenario_6_crab_growth/2021_Low_SLR_Crab_Growth.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, Years, `Fiddler Crabs`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_SLR_crab_growth_2 <- low_SLR_crab_growth %>%
  pivot_longer(cols = `Total Marsh`:`Low Marsh`,
               names_to = "Habitat Type",
               values_to = "areal_extent")

ggplot(data = low_SLR_crab_growth_2, 
            mapping = aes(x = time,
                          y = areal_extent,
                          color = `Habitat Type`,
                          linetype = `Habitat Type`
            )) +
  #geom_point(show.legend = FALSE) +
  geom_line(size = 6) +
  scale_y_continuous(limits = c(0, 60000)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  labs(x = "Year",
       #y = "Marsh Extent\n(square meters)",
       title = "Low Sea Level Rise Rate",
       subtitle = "Crab Growth, No Mitigation") +
  ylab(expression(Marsh~Extent~(m^{"2"}))) +
  standard_theme +
  scale_linetype_manual(values = c("Total Marsh" = "solid",
                                   "Low Marsh" = "dashed",
                                   "High Marsh" = "dotted")) +
  scale_color_manual(values = c("Total Marsh" = "darkgreen",
                                "Low Marsh" = "forestgreen",
                                "High Marsh" = "limegreen")) 

ggsave(filename = "figures/low_SLR_crab_growth.png",
       width = 22,
       height = 15)


# (growth + guides(color = "none", fill = "none") + ylab("") + labs(title = "Low Sea Level Rise Rate")) + 
#   plot_annotation(tag_levels = 'A') + 
#   plot_layout(guides = 'collect') +
#   ggsave("figures/low_SLR_crab_growth.png",  width = 22, height = 15)
# #dev.off()
