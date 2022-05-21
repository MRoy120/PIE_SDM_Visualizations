source("scripts/libraries_and_formatting.R")

crab_density <- read_csv("data/scenario_1_baseline/2021_Baseline_Low_SLR.csv") %>%
  mutate(time = seq(from = 2014, to = 2249, by = 0.25)) %>%
  select(time, `Total Marsh Area`, `High Marsh Area`, `Low Marsh Area`, Years, `Fiddler Crabs`) %>%
  rename(time_step = Years,
         `Total Marsh` = `Total Marsh Area`,
         `High Marsh` = `High Marsh Area`,
         `Low Marsh` = `Low Marsh Area`,
         `Crab Density` = `Fiddler Crabs`)

crab_density_2 <- crab_density %>%
  pivot_longer(cols = `Crab Density`,
               names_to = "Crab Density",
               values_to = "crabs_per_m2")

ggplot(data = crab_density_2, 
       mapping = aes(x = time,
                     y = crabs_per_m2,
                     color = `Crab Density`)) +
  geom_line(size = 6) +
  scale_y_continuous(limits = c(0, 300)) +
  theme_bw(base_size = 40,
           base_family = "Arial") +
  labs(x = "Year",
       y = "Crab Density\n(crabs/square meters)",
       title = "Fiddler Crab Population Density") +
  standard_theme +
  theme(legend.position = "none") +
  scale_color_manual("Crab Density", values = c("Crab Density" = "darkslateblue")) +
  ggsave(filename = "figures/crab_density.png",
         width = 22,
         height = 15)


