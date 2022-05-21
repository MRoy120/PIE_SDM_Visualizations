source("scripts/libraries_and_formatting.R")

high_SLR_rec_base <- read_csv("data/scenario_1_baseline/2021_Baseline_High_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Baseline` = `Mean Days Recreating`)

high_mean_rec_base <- select(high_SLR_rec_base, c(time, `Baseline`))

high_SLR_rec_erosion <- read_csv("data/scenario_2_crab_erosion/2021_Crab_Erosion_High_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Crab Mediated\nMarsh Erosion` = `Mean Days Recreating`)

high_mean_rec_erosion <- select(high_SLR_rec_erosion, c(time, `Crab Mediated\nMarsh Erosion`))

high_SLR_rec_mitigation <- read_csv("data/scenario_5_combined_mitigation/2021_High_SLR_Combined_Mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Tidal Flood and\nErosion Control\nwith Crab Erosion` = `Mean Days Recreating`)

high_mean_rec_mitigation <- select(high_SLR_rec_mitigation, 
                                   c(time, `Tidal Flood and\nErosion Control\nwith Crab Erosion`))

high_SLR_rec_growth <- read_csv("data/scenario_6_crab_growth/2021_High_SLR_Crab_Growth.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Crab Mediated\nMarsh Growth` = `Mean Days Recreating`)

high_mean_rec_growth <- select(high_SLR_rec_growth, c(time, `Crab Mediated\nMarsh Growth`))

high_rec_join <- full_join(high_mean_rec_base, high_mean_rec_erosion)
high_rec_join <- full_join(high_rec_join, high_mean_rec_mitigation)
high_rec_join <- full_join(high_rec_join, high_mean_rec_growth)

high_rec_join_2 <- high_rec_join %>%
  pivot_longer(cols = `Baseline`:`Crab Mediated\nMarsh Growth`,
               names_to = "Scenario",
               values_to = "time_recreating")

ggplot(data = high_rec_join_2, 
       mapping = aes(x = time,
                     y = time_recreating,
                     color = `Scenario`)) +
  geom_line(size = 6,
            aes(linetype = `Scenario`)) +
  scale_y_continuous(limits = c(0, 50)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  scale_linetype_manual(values = c("solid", "dotted", "dashed","twodash")) +
  scale_color_manual("Scenario", values = c("Baseline" = "gray25", 
                                            "Crab Mediated\nMarsh Erosion" = "dodgerblue4",
                                            "Tidal Flood and\nErosion Control\nwith Crab Erosion" = 
                                              "deepskyblue2",
                                            "Crab Mediated\nMarsh Growth" = "darkred")) +
  labs(x = "Year",
       # y = "Average Time Recreating\n(days/year)",
       title = "High Sea Level Rise Rate" #,
       # subtitle = "Recreation: Baseline, and Fiddler Crab Mediated Erosion and Growth"
  ) +
  standard_theme +
  ylab(expression(Average~Time~Recreating~(days~year^{"-1"}))) +
  theme(legend.key.height = unit(3, 'cm')) +
  ggsave(filename = "figures/high_SLR_rec.png",
         width = 22,
         height = 15)
