source("scripts/2021_Low_SLR_mits_crab_erosion.R")
source("scripts/2021_Moderate_SLR_mits_crab_erosion.R")
source("scripts/2021_High_SLR_mits_crab_erosion.R")

ggpubr::ggarrange(plotlist = (list(low_slr_mit_rates,
                                   mod_slr_mit_rates,
                                   high_slr_mit_rates)),
                  nrow = 3,
                  legend = "right")

ggsave(filename = "figures/all_mitigation_rates.png",
       width = 40,
       height = 35)

