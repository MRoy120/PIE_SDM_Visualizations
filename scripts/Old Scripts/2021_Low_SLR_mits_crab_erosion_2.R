source("scripts/libraries_and_formatting.R")

low_SLR_base_1 <- read_csv("data/scenario_1_baseline/2021_Baseline_Low_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR Only") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `=\"Policy Demand: Erosion Control\"`, `Policy Demand Flood Control`,
         `Mean Mitigation Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `=\"Policy Demand: Erosion Control\"`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`) %>%
  dplyr::group_by(time_step) %>%
  mutate(`Mean Policy Demand` = mean(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`)) %>%
  ungroup() %>%
  dplyr::select(-c(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`))

low_mean_base <- select(low_SLR_base_1, c(time, 
                                          `Total Marsh`, 
                                          `Mean Days Recreating`, 
                                          Scenario,
                                          `Mean Policy Demand`,
                                          `Mean Mitigation Rate`))

low_SLR_combined <- read_csv("data/scenario_7_combined_mit_no_crabs/2021_Low_SLR_Combined_Mit_No_Crab.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR - Mitigation") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `Policy Demand: Erosion Control`, `Policy Demand Flood Control`,
         `Mean Mitigation Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `Policy Demand: Erosion Control`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`) %>%
  dplyr::group_by(time_step) %>%
  mutate(`Mean Policy Demand` = mean(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`)) %>%
  ungroup() %>%
  dplyr::select(-c(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`))

low_mean_combined <- select(low_SLR_combined, c(time, 
                                                `Total Marsh`, 
                                                `Mean Days Recreating`, 
                                                Scenario,
                                                `Mean Policy Demand`,
                                                `Mean Mitigation Rate`))

low_SLR_crabs_base_1 <- read_csv("data/scenario_2_crab_erosion/2021_Crab_Erosion_Low_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR - Crab Erosion") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `=\"Policy Demand: Erosion Control\"`, `Policy Demand Flood Control`,
         `Mean Mitigation Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `=\"Policy Demand: Erosion Control\"`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`) %>%
  dplyr::group_by(time_step) %>%
  mutate(`Mean Policy Demand` = mean(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`)) %>%
  ungroup() %>%
  dplyr::select(-c(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`))

low_mean_crabs_base <- select(low_SLR_crabs_base_1, c(time, 
                                                      `Total Marsh`, 
                                                      `Mean Days Recreating`, 
                                                      Scenario,
                                                      `Mean Policy Demand`,
                                                      `Mean Mitigation Rate`))

low_SLR_crabs_combined <- read_csv("data/scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR - Crab Erosion\nand Mitigation") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `=\"Policy Demand: Erosion Control\"`, `Policy Demand Flood Control`,
         `Mean Mitigation Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `=\"Policy Demand: Erosion Control\"`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`) %>%
  dplyr::group_by(time_step) %>%
  mutate(`Mean Policy Demand` = mean(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`)) %>%
  ungroup() %>%
  dplyr::select(-c(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`))

low_mean_crabs_combined <- select(low_SLR_crabs_combined, c(time, 
                                                            `Total Marsh`, 
                                                            `Mean Days Recreating`, 
                                                            Scenario,
                                                            `Mean Policy Demand`,
                                                            `Mean Mitigation Rate`))

low_SLR_crab_growth_base_1 <- read_csv("data/scenario_6_crab_growth/2021_Low_SLR_Crab_Growth.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25),
         Scenario = "SLR - Crab Induced\nMarsh Growth") %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, 
         Years, `Fiddler Crabs`, `Mean Days Recreating`, Scenario,
         `=\"Policy Demand: Erosion Control\"`, `Policy Demand Flood Control`,
         `Mean Mitigation Rate`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`,
         `Policy Demand: Erosion Control` = `=\"Policy Demand: Erosion Control\"`,
         `Policy Demand: Flood Control` = `Policy Demand Flood Control`) %>%
  dplyr::group_by(time_step) %>%
  mutate(`Mean Policy Demand` = mean(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`)) %>%
  ungroup() %>%
  dplyr::select(-c(`Policy Demand: Erosion Control`, `Policy Demand: Flood Control`))

low_SLR_crab_growth_mean <- select(low_SLR_crab_growth_base_1, c(time, 
                                                                 `Total Marsh`, 
                                                                 `Mean Days Recreating`, 
                                                                 Scenario,
                                                                 `Mean Policy Demand`,
                                                                 `Mean Mitigation Rate`))

low_crabs_join_2 <- full_join(low_mean_base, low_mean_combined)
low_crabs_join_2 <- full_join(low_crabs_join_2, low_mean_crabs_base)
low_crabs_join_2 <- full_join(low_crabs_join_2, low_mean_crabs_combined)
low_crabs_join_2 <- full_join(low_crabs_join_2, low_SLR_crab_growth_mean) %>%
  mutate(`Sea Level Rise Rate` = "Low\nSea Level Rise")

# Total Marsh Area
low_crab_erosion <- ggplot(data = low_crabs_join_2 #%>% filter(Scenario == "SLR Only\nand Mitigation")
                           , 
                           mapping = aes(x = time,
                                         y = `Total Marsh`,
                                         color = Scenario)) +
  geom_line(size = 3.5,
            aes(linetype = Scenario),
            alpha = 0.8) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_linetype_manual(values = c("SLR Only" = "twodash",
                                   "SLR - Mitigation" = "dotdash",
                                   "SLR - Crab Erosion" = "solid",
                                   "SLR - Crab Erosion\nand Mitigation" = "twodash",
                                   "SLR - Crab Induced\nMarsh Growth" = "dotdash")) +
  scale_color_manual(values = c("SLR Only" = "skyblue4",
                                "SLR - Mitigation" = "firebrick4",
                                "SLR - Crab Erosion" = "royalblue3",
                                "SLR - Crab Erosion\nand Mitigation" = "black",
                                "SLR - Crab Induced\nMarsh Growth" = "goldenrod3"
  )) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       #title = "Low Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
       subtitle = "Low Sea Level Rise - Marsh Area"
  ) +
  standard_theme +
  theme_classic2(base_size = 35,
             base_family = "Arial") +
  ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
  # theme(axis.title.y = element_text(size = 26, 
  #                                   face = "bold")) +
  theme(legend.key.height = unit(2.5, 'cm'),
        legend.key.width = unit(2, 'cm'),
        legend.position = "right")

low_crab_erosion

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
  #                                  "Low SLR and Crabs" = "dotdash",
  #                                  "Low SLR and Crabs:\nErosion Mit." = "solid",
  #                                  "Low SLR and Crabs:\nTidal Flood Mit." = "dotdash",
  #                                  "Low SLR and Crabs:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("Low SLR Only" = "burlywood2",
  #                               "Low SLR and Crabs" = "darkred", 
  #                               "Low SLR and Crabs:\nErosion Mit." = "thistle4",
  #                               "Low SLR and Crabs:\nTidal Flood Mit." = "skyblue3",
  #                               "Low SLR and Crabs:\nBoth Mit." = "slateblue3")) +
  scale_linetype_manual(values = c("SLR Only" = "twodash",
                                   "SLR and Crabs" = "dotdash",
                                   "SLR and Crabs:\nErosion Mit." = "solid",
                                   "SLR and Crabs:\nTidal Flood Mit." = "dotdash",
                                   "SLR and Crabs:\nBoth Mit." = "twodash")) +
  scale_color_manual(values = c("SLR Only" = "burlywood2",
                                "SLR and Crabs" = "darkred",
                                "SLR and Crabs:\nErosion Mit." = "thistle4",
                                "SLR and Crabs:\nTidal Flood Mit." = "skyblue3",
                                "SLR and Crabs:\nBoth Mit." = "slateblue3")) +
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
                                   "SLR and Crabs" = "dotdash",
                                   "SLR and Crabs:\nErosion Mit." = "solid",
                                   "SLR and Crabs:\nTidal Flood Mit." = "dotdash",
                                   "SLR and Crabs:\nBoth Mit." = "twodash")) +
  scale_color_manual(values = c("SLR Only" = "burlywood2",
                                "SLR and Crabs" = "darkred",
                                "SLR and Crabs:\nErosion Mit." = "thistle4",
                                "SLR and Crabs:\nTidal Flood Mit." = "skyblue3",
                                "SLR and Crabs:\nBoth Mit." = "slateblue3")) +
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
                                   "Low SLR and Crabs" = "dotdash",
                                   "Low SLR and Crabs:\nErosion Mit." = "solid",
                                   "Low SLR and Crabs:\nTidal Flood Mit." = "dotdash",
                                   "Low SLR and Crabs:\nBoth Mit." = "twodash")) +
  scale_color_manual(values = c("Low SLR Only" = "burlywood2",
                                "Low SLR and Crabs" = "darkred",
                                "Low SLR and Crabs:\nErosion Mit." = "thistle4",
                                "Low SLR and Crabs:\nTidal Flood Mit." = "skyblue3",
                                "Low SLR and Crabs:\nBoth Mit." = "slateblue3")) +
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
  facet_wrap(vars(Scenario), ncol = 3,labeller = as_labeller(c(`SLR and Crabs:\nErosion Mit.` = "Erosion Mitigation",
                                                               `SLR and Crabs:\nTidal Flood Mit.` = "Tidal Flood Mitigation",
                                                               `SLR and Crabs:\nBoth Mit.` = "Combined Policies"))) +
  scale_y_continuous(limits = c(0, 0.01)) +
  # scale_linetype_manual(values = c("low SLR Only" = "twodash",
  #                                  "low SLR and Crabs" = "dotdash",
  #                                  "low SLR and Crabs:\nErosion Mit." = "solid",
  #                                  "low SLR and Crabs:\nTidal Flood Mit." = "dotdash",
  #                                  "low SLR and Crabs:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("low SLR Only" = "burlywood2",
  #                               "low SLR and Crabs" = "darkred",
  #                               "low SLR and Crabs:\nErosion Mit." = "thistle4",
  #                               "low SLR and Crabs:\nTidal Flood Mit." = "skyblue3",
  #                               "low SLR and Crabs:\nBoth Mit." = "slateblue3")) +
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
                                   "SLR and Crabs" = "dotdash",
                                   "SLR and Crabs:\nErosion Mit." = "solid",
                                   "SLR and Crabs:\nTidal Flood Mit." = "dotdash",
                                   "SLR and Crabs:\nBoth Mit." = "twodash")) +
  scale_color_manual(values = c("SLR Only" = "burlywood2",
                                "SLR and Crabs" = "darkred",
                                "SLR and Crabs:\nErosion Mit." = "thistle4",
                                "SLR and Crabs:\nTidal Flood Mit." = "skyblue3",
                                "SLR and Crabs:\nBoth Mit." = "slateblue3")) +
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
