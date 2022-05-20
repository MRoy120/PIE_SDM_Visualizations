## Load in Libraries and Plot Function
source("scripts/supp_plots_func.R")
 
###
## Low SLR
# Generate the Figure with the stella_plots Function
low_slr_supp_figure <- supp_plots(slr_scenario = "Low",
                                  plot_title = "Low Sea Level Rise Scenarios",
                                  min = 0,
                                  max = 50
)

# Save the Figure
ggsave(plot = low_slr_supp_figure$plots,
       filename = "figures/Final Plots/low_slr_supp_figure.png",
       width = 27,
       height = 20)

###
## Moderate SLR
# Generate the Figure with the stella_plots Function
moderate_slr_supp_figure <- supp_plots(slr_scenario = "Moderate",
                                       plot_title = "Moderate Sea Level Rise Scenarios",
                                       min = 0,
                                       max = 60)

# Save the Figure
ggsave(plot = moderate_slr_supp_figure$plots,
       filename = "figures/Final Plots/moderate_slr_supp_figure.png",
       width = 27,
       height = 20)

###
## High SLR
# Generate the Figure with the stella_plots Function
high_slr_supp_figure <- supp_plots(slr_scenario = "High",
                                   plot_title = "High Sea Level Rise Scenarios",
                                   min = 0,
                                   max = 50)

# Save the Figure
ggsave(plot = high_slr_supp_figure$plots,
       filename = "figures/Final Plots/high_slr_supp_figure.png",
       width = 27,
       height = 20)

