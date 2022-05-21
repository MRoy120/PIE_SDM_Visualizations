library(ggplot2)

set.seed(111)

mydf <- data.frame(year = rep(1999:2010, time = 4),
                   treatment.type = rep(c("AC", "AF", "EC", "EF"), each = 12),
                   treatment = rep(c("treatment1", "treatment2"), each = 24),
                   mean = c(runif(min = 0.3, max = 0.55, 12),
                            rep(NA, 5), runif(min = 0.3, max = 0.55, 7),
                            runif(min = 0.3, max = 0.55, 12),
                            rep(NA, 5), runif(min = 0.3, max = 0.55, 7)),
                   se = c(runif(min = 0.01, max = 0.03, 12),
                          rep(NA, 5), runif(min = 0.01, max = 0.03, 7),
                          runif(min = 0.01, max = 0.03, 12),
                          rep(NA, 5), runif(min = 0.01, max = 0.03, 7)),
                   stringsAsFactors = FALSE)


ggplot(data = mydf, aes(x = year, y = mean,
                        color = interaction(treatment, treatment.type),
                        linetype = interaction(treatment, treatment.type))) +
  geom_point(show.legend = FALSE) +
  geom_line() +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),width = 0.1, size = 0.5) +
  scale_color_manual(name = "Treatment conditions", values = rep(c("blue", "blue", "red", "red"), times = 2)) +
  scale_linetype_manual(name = "Treatment conditions", values = rep(c(1,2), times = 4))

set.seed(965743)                 # Create example data
data <- data.frame(x = 1:20,
                   y = rnorm(100),
                   group = letters[1:5])

ggp <- ggplot(data, aes(x, y,    # Line plot created with ggplot2
                        color = group,
                        linetype = group)) +
  geom_line()

ggp +
  scale_linetype_manual("group", values = c(rep("solid", 2), rep("dashed", 2), "twodash")) +
  scale_color_manual("group", values = c("red", "green", "black", "orange", "pink"))


