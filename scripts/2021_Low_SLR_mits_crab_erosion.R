source("scripts/libraries_and_formatting.R")

low_SLR_base_1 <- read_csv("data/scenario_1_baseline/2021_Baseline_Low_SLR.csv") %>%
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

low_mean_base_2 <- select(low_SLR_base_1, c(time, 
                                            `Total Marsh`, 
                                            `Mean Days Recreating`, 
                                            Scenario,
                                            `Policy Demand: Erosion Control`,
                                            `Policy Demand: Flood Control`,
                                            `Flood Mitigation Rate: High Marsh`,
                                            `Flood Mitigation Rate: Low Marsh`,
                                            `Erosion Control Rate`))

low_SLR_crabs_base_1 <- read_csv("data/scenario_2_crab_erosion/2021_Crab_Erosion_Low_SLR.csv") %>%
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

low_crabs_mean_base_2 <- select(low_SLR_crabs_base_1, c(time, 
                                                        `Total Marsh`, 
                                                        `Mean Days Recreating`, 
                                                        Scenario,
                                                        `Policy Demand: Erosion Control`,
                                                        `Policy Demand: Flood Control`,
                                                        `Flood Mitigation Rate: High Marsh`,
                                                        `Flood Mitigation Rate: Low Marsh`,
                                                        `Erosion Control Rate`))

low_SLR_crabs_erosion_control <- read_csv("data/scenario_3_crab_erosion_erosion_reduction/2021_Crab_Erosion_Low_SLR_Erosion_Mit.csv") %>%
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

low_mean_crabs_erosion_control <- select(low_SLR_crabs_erosion_control, c(time, 
                                                                          `Total Marsh`, 
                                                                          `Mean Days Recreating`, 
                                                                          Scenario,
                                                                          `Policy Demand: Erosion Control`,
                                                                          `Policy Demand: Flood Control`,
                                                                          `Flood Mitigation Rate: High Marsh`,
                                                                          `Flood Mitigation Rate: Low Marsh`,
                                                                          `Erosion Control Rate`))

low_SLR_crabs_tidal_flood <- read_csv("data/scenario_4_flood_mitigation/2021_Low_SLR_Flood_Mit.csv") %>%
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

low_mean_crabs_tidal_flood <- select(low_SLR_crabs_tidal_flood, c(time, 
                                                                  `Total Marsh`, 
                                                                  `Mean Days Recreating`, 
                                                                  Scenario,
                                                                  `Policy Demand: Erosion Control`,
                                                                  `Policy Demand: Flood Control`,
                                                                  `Flood Mitigation Rate: High Marsh`,
                                                                  `Flood Mitigation Rate: Low Marsh`,
                                                                  `Erosion Control Rate`))

low_SLR_crabs_combined <- read_csv("data/scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit.csv") %>%
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

low_mean_crabs_combined <- select(low_SLR_crabs_combined, c(time, 
                                                            `Total Marsh`, 
                                                            `Mean Days Recreating`, 
                                                            Scenario,
                                                            `Policy Demand: Erosion Control`,
                                                            `Policy Demand: Flood Control`,
                                                            `Flood Mitigation Rate: High Marsh`,
                                                            `Flood Mitigation Rate: Low Marsh`,
                                                            `Erosion Control Rate`))

low_crabs_join_2 <- full_join(low_mean_base_2, low_crabs_mean_base_2)
low_crabs_join_2 <- full_join(low_crabs_join_2, low_mean_crabs_erosion_control)
low_crabs_join_2 <- full_join(low_crabs_join_2, low_mean_crabs_tidal_flood)
low_crabs_join_2 <- full_join(low_crabs_join_2, low_mean_crabs_combined) %>%
  mutate(`Sea Level Rise Rate` = "Low\nSea Level Rise")

low_crab_erosion_join_3 <- low_crabs_join_2 %>%
  group_by(time, Scenario) %>%
  mutate(`Total Flood\nControl Rate` = sum(`Flood Mitigation Rate: High Marsh`, `Flood Mitigation Rate: Low Marsh`)) %>%
  rename(`Erosion\nControl Rate` = `Erosion Control Rate`) %>%
  ungroup()

low_crab_erosion_long <- low_crabs_join_2 %>%
  pivot_longer(cols = c(`Total Marsh`, `Mean Days Recreating`),
               values_to = "Measurements",
               names_to = "Measurement Type")

low_crab_erosion_join_long <- low_crab_erosion_join_3 %>%
  pivot_longer(cols = c(`Erosion\nControl Rate`, `Total Flood\nControl Rate`),
               values_to = "Mitigation Rates",
               names_to = "Mitigation Policy") %>%
  dplyr::filter(Scenario != "SLR Only") %>%
  dplyr::filter(Scenario != "SLR with Crabs")

# Total Marsh Area
low_crab_erosion <- ggplot(data = low_crabs_join_2, 
                           mapping = aes(x = time,
                                         y = `Total Marsh`,
                                         color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 100000)) +
  # scale_linetype_manual(values = c("Low SLR Only" = "twodash",
  #                                  "Low SLR with Crabs" = "dotdash",
  #                                  "Low SLR with Crabs:\nErosion Mit." = "solid",
  #                                  "Low SLR with Crabs:\nTidal Flood Mit." = "dotdash",
  #                                  "Low SLR with Crabs:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("Low SLR Only" = "burlywood2",
  #                               "Low SLR with Crabs" = "darkred", 
  #                               "Low SLR with Crabs:\nErosion Mit." = "thistle4",
  #                               "Low SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
  #                               "Low SLR with Crabs:\nBoth Mit." = "slateblue3")) +
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
       subtitle = "Low Sea Level Rise"
  ) +
  standard_theme +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  # theme(axis.title.y = element_text(size = 26, 
  #                                   face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))

# ggsave(filename = "figures/low_SLR_crab_erosion_mits.png",
#        width = 22,
#        height = 15)

###
# Recreation
low_crab_erosion_rec <- ggplot(data = low_crabs_join_2, 
                               mapping = aes(x = time,
                                             y = `Mean Days Recreating`,
                                             color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 40)) +
  # scale_linetype_manual(values = c("Low SLR Only" = "twodash",
  #                                  "Low SLR with Crabs" = "dotdash",
  #                                  "Low SLR with Crabs:\nErosion Mit." = "solid",
  #                                  "Low SLR with Crabs:\nTidal Flood Mit." = "dotdash",
  #                                  "Low SLR with Crabs:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("Low SLR Only" = "burlywood2",
  #                               "Low SLR with Crabs" = "darkred", 
  #                               "Low SLR with Crabs:\nErosion Mit." = "thistle4",
  #                               "Low SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
  #                               "Low SLR with Crabs:\nBoth Mit." = "slateblue3")) +
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
       subtitle = "Low Sea Level Rise"
  ) +
  standard_theme +
  theme(legend.position = "none") +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  # theme(axis.title.y = element_text(size = 42, 
  #                                   face = "bold")) +
  ylab(expression(Mean~Days~Recreating~(days~year^{"-1"}))) +
  theme(legend.key.height = unit(3, 'cm'))

# ggsave(filename = "figures/low_SLR_crab_erosion_mits_recreation.png",
#        width = 22,
#        height = 15)

###
# Policy Demand: Erosion Control
ggplot(data = low_crabs_join_2,
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

# ggsave(filename = "figures/low_SLR_crab_erosion_mits_erosion_policy_demand.png",
#        width = 22,
#        height = 15)

# Policy Demand: Tidal Flood Mitigation
ggplot(data = low_crabs_join_2,
       mapping = aes(x = time,
                     y = `Policy Demand: Tidal Control`,
                     color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  scale_linetype_manual(values = c("Low SLR Only" = "twodash",
                                   "Low SLR with Crabs" = "dotdash",
                                   "Low SLR with Crabs:\nErosion Mit." = "solid",
                                   "Low SLR with Crabs:\nTidal Flood Mit." = "dotdash",
                                   "Low SLR with Crabs:\nBoth Mit." = "twodash")) +
  scale_color_manual(values = c("Low SLR Only" = "burlywood2",
                                "Low SLR with Crabs" = "darkred",
                                "Low SLR with Crabs:\nErosion Mit." = "thistle4",
                                "Low SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
                                "Low SLR with Crabs:\nBoth Mit." = "slateblue3")) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       #title = "Low Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
  ) +
  standard_theme +
  #ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  theme(axis.title.y = element_text(size = 42,
                                    face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))


## Mitigation Rates
low_slr_mit_rates <- ggplot(data = low_crab_erosion_join_long,
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
  # scale_linetype_manual(values = c("low SLR Only" = "twodash",
  #                                  "low SLR with Crabs" = "dotdash",
  #                                  "low SLR with Crabs:\nErosion Mit." = "solid",
  #                                  "low SLR with Crabs:\nTidal Flood Mit." = "dotdash",
  #                                  "low SLR with Crabs:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("low SLR Only" = "burlywood2",
  #                               "low SLR with Crabs" = "darkred",
  #                               "low SLR with Crabs:\nErosion Mit." = "thistle4",
  #                               "low SLR with Crabs:\nTidal Flood Mit." = "skyblue3",
  #                               "low SLR with Crabs:\nBoth Mit." = "slateblue3")) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       #title = "Low Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
       subtitle = "Low Sea Level Rise and Crab Erosion"
  ) +
  ylab(expression(Mitigation~Rates~(percent~year^{"-1"}))) +
  standard_theme +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  #ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  #theme(axis.title.y = element_text(size = 42)) +
  theme(legend.key.height = unit(3, 'cm')) +
  ggsci::scale_color_aaas()

# ggsave(filename = "figures/low_slr_mitigation_rates.png",
#        width = 30,
#        height = 15)

## Combined Marsh Area and Recreation
low_slr_faceted_plot <- ggplot(data = low_crab_erosion_long,
                               mapping = aes(x = time,
                                             y = `Measurements`,
                                             color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = `Scenario`),
            alpha = 0.7) +
  # facet_grid(Measurements ~ `Measurement Type`,
  #            #scales = "free",
  #            #space = "free_y",
  #            labeller = as_labeller(c(`Total Marsh` = "Marsh Area",
  #                                     `Mean Days Recreating` = "Recreation Time"))) +
  facet_wrap(~`Measurement Type`, ncol = 2, labeller = as_labeller(c(`Total Marsh` = "Marsh Area",
                                                                          `Mean Days Recreating` = "Recreation Time")),
             scales = "free_y") +
  scale_y_continuous(limits = c(0, 50000)) +
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
       subtitle = "Low Sea Level Rise and Crab Erosion"
  ) +
  ylab(expression(Mitigation~Rates~(percent~year^{"-1"}))) +
  standard_theme +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  #ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  #theme(axis.title.y = element_text(size = 42)) +
  theme(legend.key.height = unit(3, 'cm'))

# ggsave(filename = "figures/low_slr_mitigation_rates.png",
#        width = 30,
#        height = 15)
