source("scripts/libraries_and_formatting.R")

low_SLR_base_1 <- read_csv("data/scenario_1_baseline/2021_Baseline_Low_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "Baseline Low SLR") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_mean_base_2 <- select(low_SLR_base_1, c(time, `Total Marsh`, `Mean Days Recreating`, Scenario))

low_SLR_erosion_control <- read_csv("data/scenario_1_baseline/low_slr_erosion_control/2022_low_slr_erosion_mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "Low SLR with\nErosion Mitigation") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_mean_erosion_control <- select(low_SLR_erosion_control, c(time, `Total Marsh`, `Mean Days Recreating`, Scenario))

low_SLR_tidal_flood <- read_csv("data/scenario_1_baseline/low_slr_erosion_control/2022_low_slr_tidal_flood_mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "Low SLR with\nTidal Flood Mitigation") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_mean_tidal_flood <- select(low_SLR_tidal_flood, c(time, `Total Marsh`, `Mean Days Recreating`, Scenario))

low_SLR_combined <- read_csv("data/scenario_1_baseline/low_slr_erosion_control/2022_low_slr_combined_mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "Low SLR with\nBoth Mitigation Policies") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_mean_combined <- select(low_SLR_combined, c(time, `Total Marsh`, `Mean Days Recreating`, Scenario))

low_join_2 <- full_join(low_mean_base_2, low_mean_erosion_control)
low_join_2 <- full_join(low_join_2, low_mean_tidal_flood)
low_join_2 <- full_join(low_join_2, low_mean_combined)

ggplot(data = low_join_2, 
       mapping = aes(x = time,
                     y = `Total Marsh`,
                     color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario)) +
  scale_y_continuous(limits = c(0, 80000)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  scale_linetype_manual(values = c("Baseline Low SLR" = "solid",
                                   "Low SLR with\nErosion Mitigation" = "dotted",
                                   "Low SLR with\nTidal Flood Mitigation" = "twodash",
                                   "Low SLR with\nBoth Mitigation Policies" = "dashed")) +
  scale_color_manual(values = c("Baseline Low SLR" = "darkred", 
                                "Low SLR with\nErosion Mitigation" = "dodgerblue4",
                                "Low SLR with\nTidal Flood Mitigation" = "skyblue3",
                                "Low SLR with\nBoth Mitigation Policies" = "gray25")) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       title = "Low Sea Level Rise Rate\nand Mitigation Policies"
  ) +
  standard_theme +
  ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  theme(axis.title.y = element_text(size = 42, 
                                    face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))

ggsave(filename = "figures/low_SLR_only_mits.png",
       width = 22,
       height = 15)




