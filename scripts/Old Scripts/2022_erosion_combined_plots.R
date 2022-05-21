source("scripts/2021_Low_SLR_mits_crab_erosion.R")
source("scripts/2021_Moderate_SLR_mits_crab_erosion.R")
source("scripts/2021_High_SLR_mits_crab_erosion.R")

ggpubr::ggarrange(plotlist = (list(low_crab_erosion,
                                   low_crab_erosion_rec,
                                   mid_crabs_erosion_plot,
                                   mid_crabs_erosion_rec_plot,
                                   high_crabs_erosion_plot,
                                   high_crabs_erosion_rec_plot)),
                  nrow = 3,
                  ncol = 2,
                  legend = "right")

ggsave(filename = "figures/all_erosion_plots.png",
       width = 40,
       height = 35)