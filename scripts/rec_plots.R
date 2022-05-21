source("scripts/libraries_and_formatting.R")

rec_tibble <- dplyr::tibble(
  `Recreational Activity` = c(
    "Fishing by Land",
    "Hiking",
    "Fishing by Boat",
    "Duck Hunting",
    "Pleasure Boating",
    "Scientific/Field Research",
    "Birding",
    "Walking Through the Marsh",
    "Driving by the Marsh"),
  `Mean Time (days/year)` = c(1,
                              2,
                              3,
                              3,
                              6,
                              14,
                              24,
                              40,
                              56),
  `Maximum Time (days)` = c(5,
                            10,
                            20,
                            40,
                            50,
                            120,
                            340,
                            340,
                            300
  ),
  
)

rec_tibble_long <- rec_tibble %>%
  arrange(`Recreational Activity`) %>%
  pivot_longer(cols = `Mean Time (days/year)`:`Maximum Time (days)`,
               names_to = "Time Category",
               values_to = "Time Spent")

ggplot(data = rec_tibble_long %>% dplyr::filter(`Time Category` %in% "Mean Time (days/year)"),
       aes(x = `Recreational Activity`,
           y = `Time Spent`,
           fill = `Time Category`)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_errorbar(aes(x = `Recreational Activity`, 
                    ymin = `Time Category` - sd(rec_tibble_long$`Time Category`), 
                    ymax = `Time Category` + sd), 
                width = 0.4)
  # facet_wrap(vars(`Time Category`)) +
  theme_classic(base_size = 18,
           base_family = "Times") +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.95, 
                                   hjust=1, 
                                   face = "bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13,
                                 face = "bold",
                                 family = "Times"),
        axis.title.y = element_text(size = 15,
                                    face = "bold")) +
  ggsci::scale_fill_aaas() +
  labs(title = "Time Stakeholders Spent Engaging in Recreational Activities",
       subtitle = "Plum Island Estuary (PIE) Massachusetts")




