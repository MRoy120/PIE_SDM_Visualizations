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

mid_SLR_crabs_base_1 <- read_csv("data/scenario_2_crab_erosion/2021_Crab_Erosion_Moderate_SLR.csv") %>%
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

mid_crabs_mean_base_2 <- select(mid_SLR_crabs_base_1, c(time, 
                                                        `Total Marsh`, 
                                                        `Mean Days Recreating`, 
                                                        Scenario,
                                                        `Policy Demand: Erosion Control`,
                                                        `Policy Demand: Flood Control`,
                                                        `Flood Mitigation Rate: High Marsh`,
                                                        `Flood Mitigation Rate: Low Marsh`,
                                                        `Erosion Control Rate`))

mid_SLR_crabs_erosion_control <- read_csv("data/scenario_3_crab_erosion_erosion_reduction/2021_Crab_Erosion_Moderate_SLR_Erosion_Mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR with Crabs:\nErosion Mit.") %>%
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

mid_mean_crabs_erosion_control <- select(mid_SLR_crabs_erosion_control, c(time, 
                                                                          `Total Marsh`, 
                                                                          `Mean Days Recreating`, 
                                                                          Scenario,
                                                                          `Policy Demand: Erosion Control`,
                                                                          `Policy Demand: Flood Control`,
                                                                          `Flood Mitigation Rate: High Marsh`,
                                                                          `Flood Mitigation Rate: Low Marsh`,
                                                                          `Erosion Control Rate`))

mid_SLR_crabs_tidal_flood <- read_csv("data/scenario_4_flood_mitigation/2021_Moderate_SLR_Flood_Mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR with Crabs:\nTidal Flood Mit.") %>%
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

mid_mean_crabs_tidal_flood <- select(mid_SLR_crabs_tidal_flood, c(time, 
                                                                  `Total Marsh`, 
                                                                  `Mean Days Recreating`, 
                                                                  Scenario,
                                                                  `Policy Demand: Erosion Control`,
                                                                  `Policy Demand: Flood Control`,
                                                                  `Flood Mitigation Rate: High Marsh`,
                                                                  `Flood Mitigation Rate: Low Marsh`,
                                                                  `Erosion Control Rate`))

mid_SLR_crabs_combined <- read_csv("data/scenario_5_combined_mitigation/2021_Moderate_SLR_Combined_Mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR with Crabs:\nBoth Mit.") %>%
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

mid_mean_crabs_combined <- select(mid_SLR_crabs_combined, c(time, 
                                                            `Total Marsh`, 
                                                            `Mean Days Recreating`, 
                                                            Scenario,
                                                            `Policy Demand: Erosion Control`,
                                                            `Policy Demand: Flood Control`,
                                                            `Flood Mitigation Rate: High Marsh`,
                                                            `Flood Mitigation Rate: Low Marsh`,
                                                            `Erosion Control Rate`))

mid_crabs_join_2 <- full_join(mid_mean_base_2, mid_crabs_mean_base_2)
mid_crabs_join_2 <- full_join(mid_crabs_join_2, mid_mean_crabs_erosion_control)
mid_crabs_join_2 <- full_join(mid_crabs_join_2, mid_mean_crabs_tidal_flood)
mid_crabs_join_2 <- full_join(mid_crabs_join_2, mid_mean_crabs_combined) %>%
  mutate(`Sea Level Rise Rate` = "Moderate\nSea Level Rise")

moderate_crab_erosion_join_3 <- mid_crabs_join_2 %>%
  group_by(time, Scenario) %>%
  mutate(`Total Flood\nControl Rate` = sum(`Flood Mitigation Rate: High Marsh`, `Flood Mitigation Rate: Low Marsh`)) %>%
  rename(`Erosion\nControl Rate` = `Erosion Control Rate`) %>%
  ungroup()

moderate_crab_erosion_join_long <- moderate_crab_erosion_join_3 %>%
  pivot_longer(cols = c(`Erosion\nControl Rate`, `Total Flood\nControl Rate`),
               values_to = "Mitigation Rates",
               names_to = "Mitigation Policy") %>%
  dplyr::filter(Scenario != "SLR Only") %>%
  dplyr::filter(Scenario != "SLR with Crabs")

mid_crabs_erosion_plot <- ggplot(data = mid_crabs_join_2, 
                                 mapping = aes(x = time,
                                               y = `Total Marsh`,
                                               color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 100000)) +
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
       # title = "Moderate Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
       subtitle = "Moderate Sea Level Rise"
  ) +
  standard_theme +
  theme(legend.position = "none") +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  theme(axis.title.y = element_text(size = 42, 
                                    face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))

# ggsave(filename = "figures/moderate_SLR_crab_erosion_mits.png",
#        width = 22,
#        height = 15)

###
# Recreation
mid_crabs_erosion_rec_plot <- ggplot(data = mid_crabs_join_2, 
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
       # title = "Moderate Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
       subtitle = "Moderate Sea Level Rise"
  ) +
  standard_theme +
  theme(legend.position = "none") +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  #ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  # theme(axis.title.y = element_text(size = 42, 
  #                                   face = "bold")) +
  ylab(expression(Mean~Days~Recreating~(days~year^{"-1"}))) +
  theme(legend.key.height = unit(3, 'cm'))

# ggsave(filename = "figures/moderate_SLR_crab_erosion_mits_recreation.png",
#        width = 22,
#        height = 15)

###
Policy Demand: Erosion Control
ggplot(data = mid_crabs_join_2,
       mapping = aes(x = time,
                     y = `Policy Demand: Erosion Control`,
                     color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
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

# ggsave(filename = "figures/moderate_SLR_crab_erosion_mits_erosion_policy_demand.png",
#        width = 22,
#        height = 15)

# Policy Demand: Erosion Control
ggplot(data = mid_crabs_join_2,
       mapping = aes(x = time,
                     y = `Policy Demand: Flood Control`,
                     color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
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

# ggsave(filename = "figures/moderate_SLR_crab_erosion_mits_tidal_flood_policy_demand.png",
#        width = 22,
#        height = 15)

## Mitigation Rates
mod_slr_mit_rates <- ggplot(data = moderate_crab_erosion_join_long,
                            mapping = aes(x = time,
                                          y = `Mitigation Rates`,
                                          color = `Mitigation Policy`)) +
  geom_line(size = 6,
            #aes(linetype = `Mitigation Policy`),
            alpha = 0.7) +
  facet_wrap(vars(Scenario), ncol = 3,labeller = as_labeller(c(`SLR with Crabs:\nErosion Mit.` = "Erosion Mitigation",
                                                               `SLR with Crabs:\nTidal Flood Mit.` = "Tidal Flood Mitigation",
                                                               `SLR with Crabs:\nBoth Mit.` = "Combined Policies"))) +
  scale_y_continuous(limits = c(0, 0.01)) +
  # scale_linetype_manual(values = c("moderate SLR Only" = "twodash",
  #                                  "moderate SLR with Crabs" = "dotdash",
  #                                  "moderate SLR with Crabs:\nErosion Mit." = "solid",
  #                                  "moderate SLR with Crabs:\nTidal Flood Mit." = "dotdash",
  #                                  "moderate SLR with Crabs:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("moderate SLR Only" = "burlywood2",
  #                               "moderate SLR with Crabs" = "darkred",
  #                               "moderate SLR with Crabs:\nErosion Mit." = "thistle4",
  #                               "moderate SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
  #                               "moderate SLR with Crabs:\nBoth Mit." = "slateblue3")) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       #title = "Low Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
       subtitle = "Moderate Sea Level Rise and Crab Erosion"
  ) +
  ylab(expression(Mitigation~Rates~(percent~year^{"-1"}))) +
  standard_theme +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  #ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  #theme(axis.title.y = element_text(size = 42)) +
  theme(legend.key.height = unit(3, 'cm')) +
  ggsci::scale_color_aaas()

# ggsave(filename = "figures/moderate_slr_mitigation_rates.png",
#        width = 30,
#        height = 15)

