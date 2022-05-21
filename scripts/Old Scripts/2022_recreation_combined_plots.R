source("scripts/2021_Low_SLR_mits_crab_erosion.R")
source("scripts/2021_Moderate_SLR_mits_crab_erosion.R")
source("scripts/2021_High_SLR_mits_crab_erosion.R")

source("scripts/2021_Low_SLR_mits_crab_growth.R")
source("scripts/2021_Moderate_SLR_mits_crab_growth.R")
source("scripts/2021_High_SLR_mits_crab_growth.R")

###
# Combining SLR Scenario Data for Erosion
total_slr_erosion <- rbind(low_crabs_join_2,
                           mid_crabs_join_2,
                           high_crab_erosion_join_2) %>%
  mutate(`Sea Level Rise Rate` = factor(`Sea Level Rise Rate`, levels = c("Low\nSea Level Rise", 
                                                                          "Moderate\nSea Level Rise",
                                                                          "High\nSea Level Rise")))

total_crabs_erosion_rec_plot <- ggplot(data = total_slr_erosion, 
                                       mapping = aes(x = time,
                                                     y = `Mean Days Recreating`,
                                                     color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.6) +
  scale_y_continuous(limits = c(0, 50)) +
  # scale_linetype_manual(values = c("High SLR Only" = "twodash",
  #                                  "High SLR - Crab Erosion" = "dotdash",
  #                                  "High SLR - Crab Erosion:\nErosion Mit." = "solid",
  #                                  "High SLR - Crab Erosion:\nTidal Flood Mit." = "dotdash",
  #                                  "High SLR - Crab Erosion:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("High SLR Only" = "burlywood2",
  #                               "High SLR - Crab Erosion" = "darkred", 
  #                               "High SLR - Crab Erosion:\nErosion Mit." = "thistle4",
  #                               "High SLR - Crab Erosion:\nTidal Flood Mit." = "skyblue3",
  #                               "High SLR - Crab Erosion:\nBoth Mit." = "slateblue3")) +
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
  facet_wrap(~`Sea Level Rise Rate`, ncol = 3) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       #title = "High Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
       title = "Marsh Erosion by Mud Fiddler Crabs"
  ) +
  standard_theme +
  theme(legend.position = "none") +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  ylab(expression(Mean~Days~Recreating~(days~year^{"-1"}))) +
  theme(axis.title.y = element_text(size = 42, 
                                    face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))

total_crabs_erosion_rec_plot

ggsave(filename = "figures/total_slr_erosion_rec.png",
       width = 30,
       height = 15)

###
# Combining SLR Scenario Data for Growth
total_slr_growth <- rbind(low_crab_growth_join_2,
                          mid_crab_growth_join_2,
                          high_crab_growth_join_2) %>%
  mutate(`Sea Level Rise Rate` = factor(`Sea Level Rise Rate`, levels = c("Low\nSea Level Rise", 
                                                                          "Moderate\nSea Level Rise",
                                                                          "High\nSea Level Rise")))

total_crabs_growth_rec_plot <- ggplot(data = total_slr_growth, 
                                      mapping = aes(x = time,
                                                    y = `Mean Days Recreating`,
                                                    color = Scenario)) +
  geom_line(size = 6,
            aes(linetype = Scenario),
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, 50)) +
  # scale_linetype_manual(values = c("High SLR Only" = "twodash",
  #                                  "High SLR - Crab Erosion" = "dotdash",
  #                                  "High SLR - Crab Erosion:\nErosion Mit." = "solid",
  #                                  "High SLR - Crab Erosion:\nTidal Flood Mit." = "dotdash",
  #                                  "High SLR - Crab Erosion:\nBoth Mit." = "twodash")) +
  # scale_color_manual(values = c("High SLR Only" = "burlywood2",
  #                               "High SLR - Crab Erosion" = "darkred", 
  #                               "High SLR - Crab Erosion:\nErosion Mit." = "thistle4",
  #                               "High SLR - Crab Erosion:\nTidal Flood Mit." = "skyblue3",
  #                               "High SLR - Crab Erosion:\nBoth Mit." = "slateblue3")) +
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
  facet_wrap(~`Sea Level Rise Rate`, ncol = 3) +
  labs(x = "Year",
       #y = "Average Time Recreating\n(days/year)",
       #title = "High Sea Level Rise Rate and\nMarsh Erosion by Fiddler Crabs with Mitigation Policies"
       title = "Marsh Growth by Mud Fiddler Crabs"
  ) +
  standard_theme +
  theme(legend.position = "none") +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  ylab(expression(Mean~Days~Recreating~(days~year^{"-1"}))) +
  theme(axis.title.y = element_text(size = 42, 
                                    face = "bold")) +
  theme(legend.key.height = unit(3, 'cm'))

total_crabs_growth_rec_plot

ggsave(filename = "figures/total_slr_growth_rec.png",
       width = 30,
       height = 15)

ggpubr::ggarrange(plotlist = (list(total_crabs_erosion_rec_plot,
                                   total_crabs_growth_rec_plot)),
                  nrow = 2,
                  ncol = 1,
                  legend = "right")

ggsave(filename = "figures/erosion_growth_rec_plots.png",
       width = 35,
       height = 30)
