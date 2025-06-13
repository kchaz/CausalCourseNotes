# Load necessary libraries
library(ggplot2)


# Define the data points
data <- data.frame(
  unit = c("i", "i", "j", "j"),
  time = c(0, 1, 0, 1),
  treated = c(TRUE, TRUE, FALSE, FALSE),
  outcome = c(3, 7, 1, 3)
)

# Add counterfactual for unit i at time 1 (Y_i1(0))
counterfactual <- data.frame(
  unit = "i_cf",
  time = 1,
  treated = FALSE,
  outcome = 5 # following the parallel trend
)

# Combine all data
full_data <- rbind(data, counterfactual)
# Load necessary library
library(ggplot2)

# Base plot with increased margins
p <- ggplot() +
  # Control unit line
  geom_line(data = subset(full_data, unit == "j"),
            aes(x = time, y = outcome),
            color = "black", size = 1.5) +
  # Treated unit line
  geom_line(data = subset(full_data, unit == "i"),
            aes(x = time, y = outcome),
            color = "red", size = 1.5) +
  # Dashed counterfactual line
  geom_line(data = data.frame(x = c(0, 1), y = c(3, 5)),
            aes(x = x, y = y), color = "blue", linetype = "dashed", size = 1.2) +
  # Points
  geom_point(data = full_data,
             aes(x = time, y = outcome, color = unit),
             size = 4) +
  # Manual colors
  scale_color_manual(values = c("i" = "red", "j" = "black", "i_cf" = "blue")) +
  # Axis labels
  scale_x_continuous(breaks = c(0, 1), labels = c("0\n(No One \nTreated)",
                                                  "1\n(Only Unit i \nTreated)")) +
  labs(x = "Time", y = "Outcome") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.margin = unit(c(1, 2, 2, 1), "cm")  # top, right, bottom, left
  ) +
  geom_segment(aes(x = 1, xend = 1, y = 5, yend = 7), color = "red", size = 2, linetype = "dotted") +
  geom_segment(aes(x = 1, xend = 1, y = 1, yend = 3), color = "black", size = 2, linetype = "dotted") +

  annotate("text", x = 1.07, y = 5.1, label = expression(Y[i1](0)), size = 6, col = "blue") +
  annotate("text", x = 1.07, y = 7.1, label = expression(Y[i1](1)), size = 6, col = "red") +
  annotate("text", x = 1.07, y = 3.1, label = expression(Y[j1](0)), size = 6, col = "black") +
  annotate("text", x = 0, y = 3.5, label = expression(Y[i0](0)), size = 6, col = "red") +
  annotate("text", x = 0, y = 1.5, label = expression(Y[j0](0)), size = 6, col = "black") +

  annotate("text", x = 1.1, y = 6,
           label = "Causal\nEffect",
           size = 6, col = "red") +
  annotate("text", x = 1.1, y = 1.75,
           label = "Change\n Not from\n Treatment",
           size = 6, col = "black") +
  coord_cartesian(clip = "off", xlim = c(0, 1.1), ylim = c(0, 7))


print(p)



