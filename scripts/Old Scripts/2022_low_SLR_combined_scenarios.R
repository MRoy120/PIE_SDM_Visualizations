source("scripts/libraries_and_formatting.R")

low_SLR_base_1 <- read_csv("data/scenario_1_baseline/2021_Baseline_Low_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "Baseline:\nSea Level Rise Only") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_mean_base_2 <- select(low_SLR_base_1, c(time, `Total Marsh`, `Mean Days Recreating`, Scenario))

low_SLR_erosion_1 <- read_csv("data/scenario_2_crab_erosion/2021_Crab_Erosion_Low_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "Marsh Erosion\nMediated by Crabs") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_mean_erosion_2 <- select(low_SLR_erosion_1, c(time, `Total Marsh`, `Mean Days Recreating`, Scenario))

low_SLR_mitigation_1 <- read_csv("data/scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "Combined Mitigation\nPolicies and Crab Erosion") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_mean_mitigation_2 <- select(low_SLR_mitigation_1, 
                                c(time, `Total Marsh`, `Mean Days Recreating`, Scenario))

low_SLR_growth_1 <- read_csv("data/scenario_6_crab_growth/2021_Low_SLR_Crab_Growth.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "Marsh Growth\nMediated by Crabs") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

low_mean_growth_2 <- select(low_SLR_growth_1, c(time, `Total Marsh`, `Mean Days Recreating`, Scenario))

low_join <- full_join(low_mean_base_2, low_mean_erosion_2)
low_join <- full_join(low_join, low_mean_mitigation_2)
low_join <- full_join(low_join, low_mean_growth_2)

# low_rec_join_2 <- low_rec_join %>%
#   pivot_longer(cols = `Baseline`:`Crab Mediated\nMarsh Growth`,
#                names_to = "Scenario",
#                values_to = "time_recreating")

ggplot(data = low_join, 
       mapping = aes(x = time,
                     y = `Total Marsh`,
                     color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario)) +
  scale_y_continuous(limits = c(0, 80000)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  scale_linetype_manual(values = c("Baseline:\nSea Level Rise Only" = "solid",
                                   "Marsh Erosion\nMediated by Crabs" = "dotted",
                                   "Combined Mitigation\nPolicies and Crab Erosion" = "twodash",
                                   "Marsh Growth\nMediated by Crabs" = "dashed")) +
  scale_color_manual(values = c("Baseline:\nSea Level Rise Only" = "darkred", 
                                "Marsh Erosion\nMediated by Crabs" = "dodgerblue4",
                                "Combined Mitigation\nPolicies and Crab Erosion" = "skyblue3",
                                "Marsh Growth\nMediated by Crabs" = "gray25")) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       title = "Low Sea Level Rise Rate"
  ) +
  standard_theme +
  ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  theme(axis.title.y = element_text(size = 42, 
                                    face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))

ggsave(filename = "figures/low_SLR_all_scenarios.png",
       width = 22,
       height = 15)
