source("scripts/libraries_and_formatting.R")

## Function for Data Cleaning and Plot Generation

clean_mod <- function(data_slr,
                      data_slr_mit,
                      data_slr_crab,
                      data_slr_crab_mit,
                      data_slr_crab_grow) {
  
  slr_only <- readr::read_csv(stringr::str_c("data/", data_slr, ".csv"), col_types = cols()) %>%
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
  
  slr_only_clean <- select(slr_only, c(time, 
                                       `Total Marsh`, 
                                       `Mean Days Recreating`, 
                                       Scenario,
                                       `Mean Policy Demand`,
                                       `Mean Mitigation Rate`))
  
  slr_mitigation <- readr::read_csv(stringr::str_c("data/", data_slr_mit, ".csv"), col_types = cols()) %>%
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
  
  slr_mitigation_clean <- select(slr_mitigation, c(time, 
                                                   `Total Marsh`, 
                                                   `Mean Days Recreating`, 
                                                   Scenario,
                                                   `Mean Policy Demand`,
                                                   `Mean Mitigation Rate`))
  
  slr_crab_erosion <- readr::read_csv(stringr::str_c("data/", data_slr_crab, ".csv"), col_types = cols()) %>%
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
  
  slr_crab_erosion_clean <- select(slr_crab_erosion, c(time, 
                                                       `Total Marsh`, 
                                                       `Mean Days Recreating`, 
                                                       Scenario,
                                                       `Mean Policy Demand`,
                                                       `Mean Mitigation Rate`))
  
  slr_crab_mit <- readr::read_csv(stringr::str_c("data/", data_slr_crab_mit, ".csv"), col_types = cols()) %>%
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
  
  slr_crab_mit_clean <- select(slr_crab_mit, c(time, 
                                               `Total Marsh`, 
                                               `Mean Days Recreating`, 
                                               Scenario,
                                               `Mean Policy Demand`,
                                               `Mean Mitigation Rate`))
  
  slr_crab_grow <- readr::read_csv(stringr::str_c("data/", data_slr_crab_grow, ".csv"), col_types = cols()) %>%
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
  
  slr_crab_grow_clean <- select(slr_crab_grow, c(time, 
                                                 `Total Marsh`, 
                                                 `Mean Days Recreating`, 
                                                 Scenario,
                                                 `Mean Policy Demand`,
                                                 `Mean Mitigation Rate`))
  
  low_crabs_join_2 <- full_join(slr_only_clean, slr_mitigation_clean) %>% suppressMessages()
  low_crabs_join_2 <- full_join(low_crabs_join_2, slr_crab_erosion_clean) %>% suppressMessages()
  low_crabs_join_2 <- full_join(low_crabs_join_2, slr_crab_mit_clean) %>% suppressMessages()
  low_crabs_join_2 <- full_join(low_crabs_join_2, slr_crab_grow_clean) %>%
    mutate(`Sea Level Rise Rate` = "Low\nSea Level Rise") %>% suppressMessages()
  
}

test <- clean_mod(data_slr = "scenario_1_baseline/2021_Baseline_Low_SLR",
                  data_slr_mit = "scenario_7_combined_mit_no_crabs/2021_Low_SLR_Combined_Mit_No_Crab",
                  data_slr_crab = "scenario_2_crab_erosion/2021_Crab_Erosion_Low_SLR",
                  data_slr_crab_mit = "scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit",
                  data_slr_crab_grow = "scenario_6_crab_growth/2021_Low_SLR_Crab_Growth")


marsh_area_plot <- function(data_slr,
                            data_slr_mit,
                            data_slr_crab,
                            data_slr_crab_mit,
                            data_slr_crab_grow) {
  
  clean_data <- clean_mod(data_slr,
                          data_slr_mit,
                          data_slr_crab,
                          data_slr_crab_mit,
                          data_slr_crab_grow)
  
  ggplot(data = clean_data, 
         mapping = aes(x = time,
                       y = `Total Marsh`,
                       color = Scenario)) +
    geom_line(size = 3.5,
              aes(linetype = Scenario),
              alpha = 0.8) +
    labs(x = "Year",
         subtitle = "Low Sea Level Rise - Marsh Area") +
    ylab(expression(Total~Marsh~Area~(m^{"2"}))) +
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
                                  "SLR - Crab Induced\nMarsh Growth" = "goldenrod3")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Arial") +
    theme(legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
}

test_2 <- marsh_area_plot(data_slr = "scenario_1_baseline/2021_Baseline_Low_SLR",
                          data_slr_mit = "scenario_7_combined_mit_no_crabs/2021_Low_SLR_Combined_Mit_No_Crab",
                          data_slr_crab = "scenario_2_crab_erosion/2021_Crab_Erosion_Low_SLR",
                          data_slr_crab_mit = "scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit",
                          data_slr_crab_grow = "scenario_6_crab_growth/2021_Low_SLR_Crab_Growth")

recreation_plot <- function(data_slr,
                            data_slr_mit,
                            data_slr_crab,
                            data_slr_crab_mit,
                            data_slr_crab_grow) {
  
  clean_data <- clean_mod(data_slr,
                          data_slr_mit,
                          data_slr_crab,
                          data_slr_crab_mit,
                          data_slr_crab_grow)
  
  ggplot(data = clean_data, 
         mapping = aes(x = time,
                       y = `Mean Days Recreating`,
                       color = Scenario)) +
    geom_line(size = 3.5,
              aes(linetype = Scenario),
              alpha = 0.8) +
    labs(x = "Year",
         subtitle = "Low Sea Level Rise - Recreation") +
    ylab(expression(Mean~Days~Recreating~(days~year^{"-1"}))) +
    scale_y_continuous(limits = c(0, 30)) +
    scale_linetype_manual(values = c("SLR Only" = "twodash",
                                     "SLR - Mitigation" = "dotdash",
                                     "SLR - Crab Erosion" = "solid",
                                     "SLR - Crab Erosion\nand Mitigation" = "twodash",
                                     "SLR - Crab Induced\nMarsh Growth" = "dotdash")) +
    scale_color_manual(values = c("SLR Only" = "skyblue4",
                                  "SLR - Mitigation" = "firebrick4",
                                  "SLR - Crab Erosion" = "royalblue3",
                                  "SLR - Crab Erosion\nand Mitigation" = "black",
                                  "SLR - Crab Induced\nMarsh Growth" = "goldenrod3")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Arial") +
    theme(legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
}

test_3 <- recreation_plot(data_slr = "scenario_1_baseline/2021_Baseline_Low_SLR",
                          data_slr_mit = "scenario_7_combined_mit_no_crabs/2021_Low_SLR_Combined_Mit_No_Crab",
                          data_slr_crab = "scenario_2_crab_erosion/2021_Crab_Erosion_Low_SLR",
                          data_slr_crab_mit = "scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit",
                          data_slr_crab_grow = "scenario_6_crab_growth/2021_Low_SLR_Crab_Growth")

policy_plot <- function(data_slr,
                        data_slr_mit,
                        data_slr_crab,
                        data_slr_crab_mit,
                        data_slr_crab_grow) {
  
  clean_data <- clean_mod(data_slr,
                          data_slr_mit,
                          data_slr_crab,
                          data_slr_crab_mit,
                          data_slr_crab_grow)
  
  ggplot(data = clean_data, 
         mapping = aes(x = time,
                       y = `Mean Policy Demand` * 100,
                       color = Scenario)) +
    geom_line(size = 3.5,
              aes(linetype = Scenario),
              alpha = 0.8) +
    labs(x = "Year",
         subtitle = "Low Sea Level Rise - Policy Demand") +
    ylab(expression(Mean~Policy~Demand~(percent~year^{"-1"}))) +
    scale_y_continuous(limits = c(0, 40)) +
    scale_linetype_manual(values = c("SLR Only" = "twodash",
                                     "SLR - Mitigation" = "dotdash",
                                     "SLR - Crab Erosion" = "solid",
                                     "SLR - Crab Erosion\nand Mitigation" = "twodash",
                                     "SLR - Crab Induced\nMarsh Growth" = "dotdash")) +
    scale_color_manual(values = c("SLR Only" = "skyblue4",
                                  "SLR - Mitigation" = "firebrick4",
                                  "SLR - Crab Erosion" = "royalblue3",
                                  "SLR - Crab Erosion\nand Mitigation" = "black",
                                  "SLR - Crab Induced\nMarsh Growth" = "goldenrod3")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Arial") +
    theme(legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
}

test_4 <- policy_plot(data_slr = "scenario_1_baseline/2021_Baseline_Low_SLR",
                      data_slr_mit = "scenario_7_combined_mit_no_crabs/2021_Low_SLR_Combined_Mit_No_Crab",
                      data_slr_crab = "scenario_2_crab_erosion/2021_Crab_Erosion_Low_SLR",
                      data_slr_crab_mit = "scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit",
                      data_slr_crab_grow = "scenario_6_crab_growth/2021_Low_SLR_Crab_Growth")

mitigation_plot <- function(data_slr,
                            data_slr_mit,
                            data_slr_crab,
                            data_slr_crab_mit,
                            data_slr_crab_grow) {
  
  clean_data <- clean_mod(data_slr,
                          data_slr_mit,
                          data_slr_crab,
                          data_slr_crab_mit,
                          data_slr_crab_grow) %>%
    dplyr::filter(Scenario %in% c("SLR - Mitigation", "SLR - Crab Erosion\nand Mitigation"))
  
  ggplot(data = clean_data, 
         mapping = aes(x = time,
                       y = `Mean Mitigation Rate` * 100,
                       color = Scenario)) +
    geom_line(size = 3.5,
              aes(linetype = Scenario),
              alpha = 0.8) +
    labs(x = "Year",
         subtitle = "Low Sea Level Rise - Mitigation Rate") +
    ylab(expression(Mean~Mitigation~Rate~(percent~year^{"-1"}))) +
    scale_y_continuous(limits = c(0, 0.40)) +
    scale_linetype_manual(values = c("SLR - Mitigation" = "dotdash",
                                     "SLR - Crab Erosion\nand Mitigation" = "twodash")) +
    scale_color_manual(values = c("SLR - Mitigation" = "firebrick4",
                                  "SLR - Crab Erosion\nand Mitigation" = "black")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Arial") +
    theme(legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
}

test_5 <- mitigation_plot(data_slr = "scenario_1_baseline/2021_Baseline_Low_SLR",
                          data_slr_mit = "scenario_7_combined_mit_no_crabs/2021_Low_SLR_Combined_Mit_No_Crab",
                          data_slr_crab = "scenario_2_crab_erosion/2021_Crab_Erosion_Low_SLR",
                          data_slr_crab_mit = "scenario_5_combined_mitigation/2021_Low_SLR_Combined_Mit",
                          data_slr_crab_grow = "scenario_6_crab_growth/2021_Low_SLR_Crab_Growth")

complete_figure <- ggpubr::ggarrange(test_2, test_3, test_4, test_5, 
                                     ncol = 2, 
                                     nrow = 2, 
                                     labels = "AUTO", font.label = list(size = 30, face = "bold"))

ggsave(plot = complete_figure,
       filename = "figures/complete_figure_test.png",
       width = 40,
       height = 20)




