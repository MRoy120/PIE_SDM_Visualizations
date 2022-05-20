## Load in Libraries and Plot Function
source("scripts/stella_plots_func.R")

###
## Low SLR  
# Generate the Figure with the stella_plots Function
low_slr_figure <- stella_plots(slr_scenario = "Low",
                               plot_title = "Low Sea Level Rise Scenarios",
                               area_min = 0,
                               area_max = 100,
                               rec_min = 0,
                               rec_max = 40,
                               pol_min = -10,
                               pol_max = 30,
                               mit_min = 0,
                               mit_max = 0.40
)

# Save the Figure
ggsave(plot = low_slr_figure$plots,
       filename = "figures/Final Plots/low_slr_figure.png",
       width = 30,
       height = 22)

###
## Moderate SLR
# Generate the Figure with the stella_plots Function
moderate_slr_figure <- stella_plots(slr_scenario = "Moderate",
                                    plot_title = "Moderate Sea Level Rise Scenarios",
                                    area_min = 0,
                                    area_max = 100,
                                    rec_min = 0,
                                    rec_max = 40,
                                    pol_min = -10,
                                    pol_max = 30,
                                    mit_min = 0,
                                    mit_max = 0.40)

# Save the Figure
ggsave(plot = moderate_slr_figure$plots,
       filename = "figures/Final Plots/moderate_slr_figure.png",
       width = 30,
       height = 22)

###
## High SLR
# Generate the Figure with the stella_plots Function
high_slr_figure <- stella_plots(slr_scenario = "High",
                                plot_title = "High Sea Level Rise Scenarios",
                                area_min = 0,
                                area_max = 100,
                                rec_min = 0,
                                rec_max = 40,
                                pol_min = -10,
                                pol_max = 30,
                                mit_min = 0,
                                mit_max = 0.40)

# Save the Figure
ggsave(plot = high_slr_figure$plots,
       filename = "figures/Final Plots/high_slr_figure.png",
       width = 30,
       height = 22)

