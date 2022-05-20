source("scripts/libraries_and_formatting.R")

## Function for Data Cleaning and Plot Generation
stella_plots <- function(slr_scenario,
                         plot_title,
                         area_min,
                         area_max,
                         rec_min,
                         rec_max,
                         pol_min,
                         pol_max,
                         mit_min,
                         mit_max
                         ) {
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  ## Data
  # Sea Level Rise (SLR) Only Data - Just SLR by itself with no mitigation policies or crab effect
  slr_only <- readr::read_csv(stringr::str_c("data/scenario_1_baseline/2021_Baseline_", slr_scenario, "_SLR.csv"), col_types = cols()) %>%
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
  
  # Pull only necessary data (see selected data below)
  slr_only_clean <- select(slr_only, c(time, 
                                       `Total Marsh`, 
                                       `Mean Days Recreating`, 
                                       Scenario,
                                       `Mean Policy Demand`,
                                       `Mean Mitigation Rate`))
  
  # SLR with Mitigation - SLR with mitigation policies, but no crab effect
  slr_mitigation <- readr::read_csv(stringr::str_c("data/scenario_7_combined_mit_no_crabs/2021_", slr_scenario, "_SLR_Combined_Mit_No_Crab.csv"), 
                                    col_types = cols()) %>%
    mutate(time = seq(from = 2014, to = 2249, by = 0.25),
           Scenario = "SLR - Mitigation") %>%
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
  
  # Pull only necessary data (see selected data below)
  slr_mitigation_clean <- select(slr_mitigation, c(time, 
                                                   `Total Marsh`, 
                                                   `Mean Days Recreating`, 
                                                   Scenario,
                                                   `Mean Policy Demand`,
                                                   `Mean Mitigation Rate`))
  
  # SLR with Crab Erosion, No Mitigation - SLR with enhanced erosion by crabs, no mitigation policies
  slr_crab_erosion <- readr::read_csv(stringr::str_c("data/scenario_2_crab_erosion/2021_Crab_Erosion_", slr_scenario, "_SLR.csv"), 
                                      col_types = cols()) %>%
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
  
  # Pull only necessary data (see selected data below)
  slr_crab_erosion_clean <- select(slr_crab_erosion, c(time, 
                                                       `Total Marsh`, 
                                                       `Mean Days Recreating`, 
                                                       Scenario,
                                                       `Mean Policy Demand`,
                                                       `Mean Mitigation Rate`))
  
  # SLR with Crab Erosion and Mitigation - SLR with enhanced erosion by crabs and mitigation policies
  slr_crab_mit <- readr::read_csv(stringr::str_c("data/scenario_5_combined_mitigation/2021_", slr_scenario, "_SLR_Combined_Mit.csv"), 
                                  col_types = cols()) %>%
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
  
  # Pull only necessary data (see selected data below)
  slr_crab_mit_clean <- select(slr_crab_mit, c(time, 
                                               `Total Marsh`, 
                                               `Mean Days Recreating`, 
                                               Scenario,
                                               `Mean Policy Demand`,
                                               `Mean Mitigation Rate`))
  
  # SLR with Crab Marsh Growth, No Mitigation - SLR with enhanced marsh growth by crabs, no mitigation policies
  slr_crab_grow <- readr::read_csv(stringr::str_c("data/scenario_6_crab_growth/2021_", slr_scenario, "_SLR_Crab_Growth.csv"), 
                                   col_types = cols()) %>%
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
  
  # Pull only necessary data (see selected data below)
  slr_crab_grow_clean <- select(slr_crab_grow, c(time, 
                                                 `Total Marsh`, 
                                                 `Mean Days Recreating`, 
                                                 Scenario,
                                                 `Mean Policy Demand`,
                                                 `Mean Mitigation Rate`))
  
  # Join all the data together into one large dataset
  clean_data <- full_join(slr_only_clean, slr_mitigation_clean) %>% suppressMessages()
  clean_data <- full_join(clean_data, slr_crab_erosion_clean) %>% suppressMessages()
  clean_data <- full_join(clean_data, slr_crab_mit_clean) %>% suppressMessages()
  clean_data <- full_join(clean_data, slr_crab_grow_clean) %>%
    mutate(`Sea Level Rise Rate` = "Low\nSea Level Rise") %>% suppressMessages()
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  ## Plots
  # Total Marsh Area Plot
  area_plot <- ggplot(data = clean_data, 
                      mapping = aes(x = time,
                                    y = `Total Marsh`/1000,
                                    color = Scenario)) +
    geom_line(size = 2.7,
              aes(linetype = Scenario),
              alpha = 0.7) +
    labs(x = "Year",
         title = plot_title,
         subtitle = "Total Marsh Area") +
    ylab(expression(Total~Marsh~Area~(km^{"2"}))) +
    scale_y_continuous(limits = c(area_min, area_max)) +
    scale_linetype_manual(values = c("SLR Only" = "dashed",
                                     "SLR - Mitigation" = "dotted",
                                     "SLR - Crab Erosion" = "dotdash",
                                     "SLR - Crab Erosion\nand Mitigation" = "solid",
                                     "SLR - Crab Induced\nMarsh Growth" = "twodash")) +
    scale_color_manual(values = c("SLR Only" = "skyblue4",
                                  "SLR - Mitigation" = "#84879a",
                                  "SLR - Crab Erosion" = "royalblue3",
                                  "SLR - Crab Erosion\nand Mitigation" = "#eea093",
                                  "SLR - Crab Induced\nMarsh Growth" = "firebrick4")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Times") +
    theme(legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
  # Recreation Plot
  recreation_plot <- ggplot(data = clean_data, 
                            mapping = aes(x = time,
                                          y = `Mean Days Recreating`,
                                          color = Scenario)) +
    geom_line(size = 2.7,
              aes(linetype = Scenario),
              alpha = 0.7) +
    labs(x = "Year",
         title = plot_title,
         subtitle = "Recreation") +
    ylab(expression(Mean~Days~Recreating~(days~year^{"-1"}))) +
    scale_y_continuous(limits = c(rec_min, rec_max)) +
    scale_linetype_manual(values = c("SLR Only" = "dashed",
                                     "SLR - Mitigation" = "dotted",
                                     "SLR - Crab Erosion" = "dotdash",
                                     "SLR - Crab Erosion\nand Mitigation" = "solid",
                                     "SLR - Crab Induced\nMarsh Growth" = "twodash")) +
    scale_color_manual(values = c("SLR Only" = "skyblue4",
                                  "SLR - Mitigation" = "#84879a",
                                  "SLR - Crab Erosion" = "royalblue3",
                                  "SLR - Crab Erosion\nand Mitigation" = "#eea093",
                                  "SLR - Crab Induced\nMarsh Growth" = "firebrick4")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Times") +
    theme(legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
  # Policy Demand Plot
  policy_plot <- ggplot(data = clean_data, 
                        mapping = aes(x = time,
                                      y = `Mean Policy Demand` * 100,
                                      color = Scenario)) +
    geom_line(size = 2.7,
              aes(linetype = Scenario),
              alpha = 0.7) +
    labs(x = "Year",
         title = plot_title,
         subtitle = "Policy Demand") +
    ylab(expression(Mean~Policy~Demand~(percent~year^{"-1"}))) +
    scale_y_continuous(limits = c(pol_min, pol_max),
                       breaks = seq(-10, 50, 10)) +
    scale_linetype_manual(values = c("SLR Only" = "dashed",
                                     "SLR - Mitigation" = "dotted",
                                     "SLR - Crab Erosion" = "dotdash",
                                     "SLR - Crab Erosion\nand Mitigation" = "solid",
                                     "SLR - Crab Induced\nMarsh Growth" = "twodash")) +
    scale_color_manual(values = c("SLR Only" = "skyblue4",
                                  "SLR - Mitigation" = "#84879a",
                                  "SLR - Crab Erosion" = "royalblue3",
                                  "SLR - Crab Erosion\nand Mitigation" = "#eea093",
                                  "SLR - Crab Induced\nMarsh Growth" = "firebrick4")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Times") +
    theme(legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
  # Mitigation Rate Plot
  mitigation_plot <- ggplot(data = clean_data %>%
                              dplyr::filter(Scenario %in% c("SLR - Mitigation", "SLR - Crab Erosion\nand Mitigation")), 
                            mapping = aes(x = time,
                                          y = `Mean Mitigation Rate` * 100,
                                          color = Scenario)) +
    geom_line(size = 2.7,
              aes(linetype = Scenario),
              alpha = 0.7) +
    labs(x = "Year",
         title = plot_title,
         subtitle = "Mitigation Rate") +
    ylab(expression(Mean~Mitigation~Rate~(percent~year^{"-1"}))) +
    scale_y_continuous(limits = c(mit_min, mit_max),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_linetype_manual(values = c("SLR - Mitigation" = "dotted",
                                     "SLR - Crab Erosion\nand Mitigation" = "solid")) +
    scale_color_manual(values = c("SLR - Mitigation" = "#84879a",
                                  "SLR - Crab Erosion\nand Mitigation" = "#eea093")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Times") +
    theme(legend.key.height = unit(2.5, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right") 
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  ## Complete Figure
  # combined the plots above into one figure
  complete_figure <- ggpubr::ggarrange(area_plot, recreation_plot, policy_plot, mitigation_plot, 
                                       ncol = 2, 
                                       nrow = 2, 
                                       labels = "AUTO", 
                                       font.label = list(size = 40, face = "bold")) %>% suppressWarnings()
  
  ## Put the data the figure in one list
  plot_data_list <- list(data = clean_data,
                         plots = complete_figure)
  
  ## Return the combined list
  return(plot_data_list)
  
}


