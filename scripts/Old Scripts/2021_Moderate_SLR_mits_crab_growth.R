source("scripts/libraries_and_formatting.R")

mid_SLR_base_1 <- read_csv("data/scenario_1_baseline/2021_Baseline_Moderate_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR Only") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `=\"Policy Demand: Erosion Control\"`, `Policy Demand Flood Control`,
         `=\"Flood Mitigation Rate: High Marsh\"`, `=\"Flood Mitigation Rate: Low Marsh\"`,
         `Erosion Control Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `=\"Policy Demand: Erosion Control\"`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`,
         `Flood Mitigation Rate: High Marsh` = `=\"Flood Mitigation Rate: High Marsh\"`,
         `Flood Mitigation Rate: Low Marsh` = `=\"Flood Mitigation Rate: Low Marsh\"`)

mid_mean_base_2 <- select(mid_SLR_base_1, c(time, 
                                            `Total Marsh`, 
                                            `Mean Days Recreating`, 
                                            Scenario,
                                            `Policy Demand: Erosion Control`,
                                            `Policy Demand: Flood Control`,
                                            `Flood Mitigation Rate: High Marsh`,
                                            `Flood Mitigation Rate: Low Marsh`,
                                            `Erosion Control Rate`))

mid_SLR_crab_growth_base_1 <- read_csv("data/scenario_6_crab_growth/2021_Moderate_SLR_Crab_Growth.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR with Crabs") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `=\"Policy Demand: Erosion Control\"`, `Policy Demand Flood Control`,
         `=\"Flood Mitigation Rate: High Marsh\"`, `=\"Flood Mitigation Rate: Low Marsh\"`,
         `Erosion Control Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `=\"Policy Demand: Erosion Control\"`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`,
         `Flood Mitigation Rate: High Marsh` = `=\"Flood Mitigation Rate: High Marsh\"`,
         `Flood Mitigation Rate: Low Marsh` = `=\"Flood Mitigation Rate: Low Marsh\"`)

mid_crab_growth_mean_base_2 <- select(mid_SLR_crab_growth_base_1, c(time, 
                                                                    `Total Marsh`, 
                                                                    `Mean Days Recreating`, 
                                                                    Scenario,
                                                                    `Policy Demand: Erosion Control`,
                                                                    `Policy Demand: Flood Control`,
                                                                    `Flood Mitigation Rate: High Marsh`,
                                                                    `Flood Mitigation Rate: Low Marsh`,
                                                                    `Erosion Control Rate`))

mid_SLR_crab_growth_erosion_control <- read_csv("data/scenario_6_crab_growth/mod_SLR_crab_growth_mits/2022_mod_slr_crab_growth_erosion_control.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR with Crabs:\nErosion Mit.") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `Policy Demand: Erosion Control`, `Policy Demand Flood Control`,
         `Flood Mitigation Rate: High Marsh`, `Flood Mitigation Rate: Low Marsh`,
         `Erosion Control Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `Policy Demand: Erosion Control`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`,
         `Flood Mitigation Rate: High Marsh` = `Flood Mitigation Rate: High Marsh`,
         `Flood Mitigation Rate: Low Marsh` = `Flood Mitigation Rate: Low Marsh`)

mid_crab_growth_mean_erosion_control <- select(mid_SLR_crab_growth_erosion_control, c(time, 
                                                                                      `Total Marsh`, 
                                                                                      `Mean Days Recreating`, 
                                                                                      Scenario,
                                                                                      `Policy Demand: Erosion Control`,
                                                                                      `Policy Demand: Flood Control`,
                                                                                      `Flood Mitigation Rate: High Marsh`,
                                                                                      `Flood Mitigation Rate: Low Marsh`,
                                                                                      `Erosion Control Rate`))

mid_SLR_crab_growth_tidal_flood <- read_csv("data/scenario_6_crab_growth/mod_SLR_crab_growth_mits/2022_mod_slr_crab_growth_tidal_control.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR with Crabs:\nTidal Flood Mit.") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `Policy Demand: Erosion Control`, `Policy Demand Flood Control`,
         `Flood Mitigation Rate: High Marsh`, `Flood Mitigation Rate: Low Marsh`,
         `Erosion Control Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `Policy Demand: Erosion Control`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`,
         `Flood Mitigation Rate: High Marsh` = `Flood Mitigation Rate: High Marsh`,
         `Flood Mitigation Rate: Low Marsh` = `Flood Mitigation Rate: Low Marsh`)

mid_crab_growth_mean_tidal_flood <- select(mid_SLR_crab_growth_tidal_flood, c(time, 
                                                                              `Total Marsh`, 
                                                                              `Mean Days Recreating`, 
                                                                              Scenario,
                                                                              `Policy Demand: Erosion Control`,
                                                                              `Policy Demand: Flood Control`,
                                                                              `Flood Mitigation Rate: High Marsh`,
                                                                              `Flood Mitigation Rate: Low Marsh`,
                                                                              `Erosion Control Rate`))

mid_SLR_crab_growth_combined <- read_csv("data/scenario_6_crab_growth/mod_SLR_crab_growth_mits/2022_mod_SLR_crab_growth_combined_mits.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR with Crabs:\nBoth Mit.") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `Policy Demand: Erosion Control`, `Policy Demand Flood Control`,
         `Flood Mitigation Rate: High Marsh`, `Flood Mitigation Rate: Low Marsh`,
         `Erosion Control Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `Policy Demand: Erosion Control`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`,
         `Flood Mitigation Rate: High Marsh` = `Flood Mitigation Rate: High Marsh`,
         `Flood Mitigation Rate: Low Marsh` = `Flood Mitigation Rate: Low Marsh`)

mid_crab_growth_mean_combined <- select(mid_SLR_crab_growth_combined, c(time, 
                                                                        `Total Marsh`, 
                                                                        `Mean Days Recreating`, 
                                                                        Scenario,
                                                                        `Policy Demand: Erosion Control`,
                                                                        `Policy Demand: Flood Control`,
                                                                        `Flood Mitigation Rate: High Marsh`,
                                                                        `Flood Mitigation Rate: Low Marsh`,
                                                                        `Erosion Control Rate`))

mid_crab_growth_join_2 <- full_join(mid_mean_base_2, mid_crab_growth_mean_base_2)
mid_crab_growth_join_2 <- full_join(mid_crab_growth_join_2, mid_crab_growth_mean_erosion_control)
mid_crab_growth_join_2 <- full_join(mid_crab_growth_join_2, mid_crab_growth_mean_tidal_flood)
mid_crab_growth_join_2 <- full_join(mid_crab_growth_join_2, mid_crab_growth_mean_combined) %>%
  mutate(`Sea Level Rise Rate` = "Moderate\nSea Level Rise")

mid_crab_growth_plot <- ggplot(data = mid_crab_growth_join_2, 
                               mapping = aes(x = time,
                                             y = `Total Marsh`,
                                             color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 100000)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  # scale_linetype_manual(values = c("Moderate SLR Only" = "twodash",
  #                                  "Moderate SLR with Crabs" = "dotdash",
  #                                  "Moderate SLR with Crabs:\nErosion Mit." = "solid",
  #                                  "Moderate SLR with Crabs:\nTidal Flood Mit." = "dotdash",
  #                                  "Moderate SLR with Crabs:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("Moderate SLR Only" = "burlywood2",
  #                               "Moderate SLR with Crabs" = "darkred", 
  #                               "Moderate SLR with Crabs:\nErosion Mit." = "thistle4",
  #                               "Moderate SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
  #                               "Moderate SLR with Crabs:\nBoth Mit." = "slateblue3")) +
  scale_linetype_manual(values = c("SLR Only" = "twodash",
                                   "SLR with Crabs" = "dotdash",
                                   "SLR with Crabs:\nErosion Mit." = "solid",
                                   "SLR with Crabs:\nTidal Flood Mit." = "dotdash",
                                   "SLR with Crabs:\nBoth Mit." = "twodash")) +
  scale_color_manual(values = c("SLR Only" = "burlywood2",
                                "SLR with Crabs" = "darkred", 
                                "SLR with Crabs:\nErosion Mit." = "thistle4",
                                "SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
                                "SLR with Crabs:\nBoth Mit." = "slateblue3")) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       title = "Moderate Sea Level Rise Rate and\nMarsh Growth by Fiddler Crabs with Mitigation Policies"
  ) +
  standard_theme +
  ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  theme(axis.title.y = element_text(size = 42, 
                                    face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))

# ggsave(filename = "figures/moderate_SLR_crab_growth_mits.png",
#        width = 22,
#        height = 15)

###
# Recreation
mid_crabs_growth_rec_plot <- ggplot(data = mid_crab_growth_join_2, 
                                    mapping = aes(x = time,
                                                  y = `Mean Days Recreating`,
                                                  color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 40)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  # scale_linetype_manual(values = c("Moderate SLR Only" = "twodash",
  #                                  "Moderate SLR with Crabs" = "dotdash",
  #                                  "Moderate SLR with Crabs:\nErosion Mit." = "solid",
  #                                  "Moderate SLR with Crabs:\nTidal Flood Mit." = "dotdash",
  #                                  "Moderate SLR with Crabs:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("Moderate SLR Only" = "burlywood2",
  #                               "Moderate SLR with Crabs" = "darkred", 
  #                               "Moderate SLR with Crabs:\nErosion Mit." = "thistle4",
  #                               "Moderate SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
  #                               "Moderate SLR with Crabs:\nBoth Mit." = "slateblue3")) +
  scale_linetype_manual(values = c("SLR Only" = "twodash",
                                   "SLR with Crabs" = "dotdash",
                                   "SLR with Crabs:\nErosion Mit." = "solid",
                                   "SLR with Crabs:\nTidal Flood Mit." = "dotdash",
                                   "SLR with Crabs:\nBoth Mit." = "twodash")) +
  scale_color_manual(values = c("SLR Only" = "burlywood2",
                                "SLR with Crabs" = "darkred", 
                                "SLR with Crabs:\nErosion Mit." = "thistle4",
                                "SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
                                "SLR with Crabs:\nBoth Mit." = "slateblue3")) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       #title = "Low Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
  ) +
  standard_theme +
  #ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  theme(axis.title.y = element_text(size = 42, 
                                    face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))
