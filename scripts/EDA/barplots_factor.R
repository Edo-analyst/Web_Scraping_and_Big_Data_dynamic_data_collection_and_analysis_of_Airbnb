library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)
library(RColorBrewer)

# Generic function to create plots for a given factor
create_factor_plot <- function(data, factor_variable, cities1, cities2, title1 = NULL, title2 = NULL) {
  
  # Automatically generate a color palette based on the levels of the given factor variable
  factor_levels <- levels(data[[factor_variable]])
  factor_colors <- RColorBrewer::brewer.pal(length(factor_levels), "Set3")  # Choose a color palette
  
  # If there are more levels than the palette's maximum, generate custom colors
  if (length(factor_levels) > 12) {
    factor_colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(factor_levels))
  }
  
  # Create first plot (for cities1)
  plot1 <- data %>%
    filter(city %in% cities1) %>%
    group_by(city, !!sym(factor_variable)) %>%
    tally() %>%
    group_by(city) %>%
    mutate(percentage = n / sum(n) * 100,
           !!sym(factor_variable) := fct_reorder(!!sym(factor_variable), n)) %>%
    ggplot(aes(x = city, y = percentage, fill = !!sym(factor_variable))) +
    geom_bar(stat = "identity", position = "dodge") +  
    labs(title = title1, x = NULL, y = NULL, fill = NULL) +  
    scale_fill_manual(values = setNames(factor_colors, factor_levels)) +  # Apply colors dynamically
    theme_minimal() +  
    theme(axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "bottom",
          legend.text = element_text(size = 8)) +  
    guides(fill = guide_legend(ncol = 1))  # Arrange legend items vertically
  
  # Create second plot (for cities2)
  plot2 <- data %>%
    filter(city %in% cities2) %>%
    group_by(city, !!sym(factor_variable)) %>%
    tally() %>%
    group_by(city) %>%
    mutate(percentage = n / sum(n) * 100,
           !!sym(factor_variable) := fct_reorder(!!sym(factor_variable), n)) %>%
    ggplot(aes(x = city, y = percentage, fill = !!sym(factor_variable))) +
    geom_bar(stat = "identity", position = "dodge") +  
    labs(title = title2, x = NULL, y = NULL, fill = NULL) +  
    scale_fill_manual(values = setNames(factor_colors, factor_levels)) +  # Apply the same colors
    theme_minimal() +  
    theme(axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "none")  # Remove legend from second plot
  
  # Combine both plots vertically and collect the legend
  return(plot1 / plot2 + plot_layout(guides = "collect"))
}

