year_replace <- seq(from = 2014, to = 2249, by = 0.25)
year_start <- seq(from = 1, to = 236, by = 0.25)
year <- high_SLR_base$years
str(year_replace)
str(year_start)
str(year)
length(year_replace)
length(high_SLR_base$years)
length(year_start)

replace(1:20, 10:15, 9:14)

test <- high_SLR_base %>%
  mutate(years = replace(year, year_start, year_replace))


ggplot(data = high_SLR_base_2, mapping = aes(x = time,
                                             y = areal_extent,
                                             color = `Habitat Type`)) +
  geom_line(size = 2,
            aes(linetype = `Habitat Type`)) +
  # scale_color_manual("Habitat Type", values = c("High Marsh Area" = "darkgreen", 
  #                                            "Low Marsh Area" = "slategrey",
  #                                            "Total Marsh Area" = "steelblue4")) +
  scale_y_continuous(limits = c(0, 50000)) +
  labs(x = "Years",
       y = "Marsh Extent\n(Square Meters)") +
  theme_dark(base_size = 20,
             base_family = "Arial") +
  scale_linetype_manual(values = c("dotted", "twodash", "solid")) +
  scale_color_brewer(palette = "Greens") +
  theme(axis.title.x = element_text(size = 22, 
                                    face = "bold"),
        axis.title.y = element_text(size = 22, 
                                    face = "bold"),
        legend.title = element_text(size = 22, 
                                    face = "bold"))
# scale_color_manual("Habitat Type", values = brewer.pal("Greens"))
# scale_color_manual("Habitat Type", values = wes_palette("GrandBudapest1", n = 3))
# scale_color_manual("Habitat Type", values = c("High Marsh Area" = "darkgreen", 
#                                               "Low Marsh Area" = "slategrey",
#                                               "Total Marsh Area" = "steelblue4"))
