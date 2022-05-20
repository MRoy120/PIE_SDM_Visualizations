source("scripts/libraries_and_formatting.R")

## Function for Data Cleaning and Plot Generation
supp_plots <- function(slr_scenario,
                       plot_title,
                       min,
                       max
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
                                       `Low Marsh`,
                                       `High Marsh`,
                                       `Total Marsh`,
                                       Scenario))
  
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
                                                   `Low Marsh`,
                                                   `High Marsh`,
                                                   `Total Marsh`,
                                                   Scenario))
  
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
                                                       `Low Marsh`,
                                                       `High Marsh`,
                                                       `Total Marsh`,
                                                       Scenario))
  
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
                                               `Low Marsh`,
                                               `High Marsh`,
                                               `Total Marsh`,
                                               Scenario))
  
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
                                                 `Low Marsh`,
                                                 `High Marsh`,
                                                 `Total Marsh`,
                                                 Scenario))
  
  # Join all the data together into one large dataset
  clean_data <- full_join(slr_only_clean, slr_mitigation_clean) %>% suppressMessages()
  clean_data <- full_join(clean_data, slr_crab_erosion_clean) %>% suppressMessages()
  clean_data <- full_join(clean_data, slr_crab_mit_clean) %>% suppressMessages()
  clean_data <- full_join(clean_data, slr_crab_grow_clean) %>% suppressMessages()
  
  clean_long <- tidyr::pivot_longer(clean_data,
                                    cols = c(`Low Marsh`, `High Marsh`, `Total Marsh`),
                                    names_to = "Marsh Zone",
                                    values_to = "Area")
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  ## Plots
  # SLR Only
  slr_only_plot <- ggplot(data = clean_long %>% dplyr::filter(Scenario == "SLR Only"), 
                          mapping = aes(x = time,
                                        y = Area/1000,
                                        color = `Marsh Zone`)) +
    geom_line(size = 3.5,
              aes(linetype = `Marsh Zone`),
              alpha = 0.8) +
    labs(x = "Year",
         title = plot_title,
         subtitle = "Sea Level Rise Only") +
    ylab(expression(Marsh~Area~(km^{"2"}))) +
    scale_y_continuous(limits = c(min, max)) +
    scale_linetype_manual(values = c("Total Marsh" = "solid",
                                     "Low Marsh" = "twodash",
                                     "High Marsh" = "dotted")) +
    scale_color_manual(values = c("Total Marsh" = "darkgreen", 
                                  "Low Marsh" = "forestgreen",
                                  "High Marsh" = "limegreen")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Times") +
    theme(legend.key.height = unit(2, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
  # SLR with Mitigation
  slr_mit_plot <- ggplot(data = clean_long %>% dplyr::filter(Scenario == "SLR - Mitigation"), 
                         mapping = aes(x = time,
                                       y = Area/1000,
                                       color = `Marsh Zone`)) +
    geom_line(size = 3.5,
              aes(linetype = `Marsh Zone`),
              alpha = 0.8) +
    labs(x = "Year",
         title = plot_title,
         subtitle = "Sea Level Rise and Mitigation") +
    ylab(expression(Marsh~Area~(km^{"2"}))) +
    scale_y_continuous(limits = c(min, max)) +
    scale_linetype_manual(values = c("Total Marsh" = "solid",
                                     "Low Marsh" = "twodash",
                                     "High Marsh" = "dotted")) +
    scale_color_manual(values = c("Total Marsh" = "darkgreen", 
                                  "Low Marsh" = "forestgreen",
                                  "High Marsh" = "limegreen")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Times") +
    theme(legend.key.height = unit(2, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
  # SLR with Crabs
  slr_crabs_plot <- ggplot(data = clean_long %>% dplyr::filter(Scenario == "SLR - Crab Erosion"), 
                           mapping = aes(x = time,
                                         y = Area/1000,
                                         color = `Marsh Zone`)) +
    geom_line(size = 3.5,
              aes(linetype = `Marsh Zone`),
              alpha = 0.8) +
    labs(x = "Year",
         title = plot_title,
         subtitle = "Sea Level Rise and Crabs") +
    ylab(expression(Marsh~Area~(km^{"2"}))) +
    scale_y_continuous(limits = c(min, max)) +
    scale_linetype_manual(values = c("Total Marsh" = "solid",
                                     "Low Marsh" = "twodash",
                                     "High Marsh" = "dotted")) +
    scale_color_manual(values = c("Total Marsh" = "darkgreen", 
                                  "Low Marsh" = "forestgreen",
                                  "High Marsh" = "limegreen")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Times") +
    theme(legend.key.height = unit(2, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
  # SLR, Crabs, and Mitigation
  slr_crabs_mit_plot <- ggplot(data = clean_long %>% dplyr::filter(Scenario == "SLR - Crab Erosion\nand Mitigation"), 
                               mapping = aes(x = time,
                                             y = Area/1000,
                                             color = `Marsh Zone`)) +
    geom_line(size = 3.5,
              aes(linetype = `Marsh Zone`),
              alpha = 0.8) +
    labs(x = "Year",
         title = plot_title,
         subtitle = "Sea Level Rise, Crabs, and Mitigation") +
    ylab(expression(Marsh~Area~(km^{"2"}))) +
    scale_y_continuous(limits = c(min, max)) +
    scale_linetype_manual(values = c("Total Marsh" = "solid",
                                     "Low Marsh" = "twodash",
                                     "High Marsh" = "dotted")) +
    scale_color_manual(values = c("Total Marsh" = "darkgreen", 
                                  "Low Marsh" = "forestgreen",
                                  "High Marsh" = "limegreen")) +
    standard_theme +
    theme_classic2(base_size = 35,
                   base_family = "Times") +
    theme(legend.key.height = unit(2, 'cm'),
          legend.key.width = unit(2, 'cm'),
          legend.position = "right")
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  ## Complete Figure
  # combined the plots above into one figure
  complete_figure <- ggpubr::ggarrange(slr_only_plot, slr_mit_plot, slr_crabs_plot, slr_crabs_mit_plot, 
                                       ncol = 2, 
                                       nrow = 2, 
                                       labels = "AUTO", font.label = list(size = 40, face = "bold")) %>% suppressWarnings()
  
  ## Put the data the figure in one list
  plot_data_list <- list(data = clean_long,
                         plots = complete_figure)
  
  ## Return the combined list
  return(plot_data_list)
  
}


