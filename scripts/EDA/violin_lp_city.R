library(ggplot2)
violin_plot <- ggplot(last_quarter, aes(x = reorder(city, lp), y = lp, fill = city)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") + # Optional, to add a boxplot
  labs(
    x = NULL,
    y = "Log Price"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5), # Reduce city text size and make horizontal
    axis.title.x = element_text(size = 10), # X-axis label size
    axis.title.y = element_text(size = 10), # Y-axis label size
    plot.title = element_blank(), # Remove title
    legend.position = "none" # Remove legend
  )
