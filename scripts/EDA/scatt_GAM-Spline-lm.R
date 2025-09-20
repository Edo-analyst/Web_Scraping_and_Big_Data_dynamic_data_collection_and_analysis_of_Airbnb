library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(forcats)
library(splines)
library(mgcv)
library(patchwork)
load("last_quarter.RData")

#bedrooms_recoded---------------------------------------------------------------
bedrooms_scat <- ggplot(last_quarter, aes(x = bedrooms_recoded, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Simple linear model (LM)
  geom_smooth(aes(color = "Linear model", fill = "Linear model"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  
  
  # GAM with cubic spline basis
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  
  
  # Linear model with natural splines
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  
  
  # Custom legend with specific colors for lines only
  scale_color_manual(name = "Model", 
                     values = c("Linear model" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Custom colors for confidence intervals
  scale_fill_manual(values = c("Linear model" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Remove legend for confidence intervals
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Number of bedrooms", 
       y = "Log price",
       title = NULL)


#accommodates-------------------------------------------------------------------
accommodates_scat <- ggplot(last_quarter, aes(x = accommodates, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Simple linear model (LM)
  geom_smooth(aes(color = "Linear model", fill = "Linear model"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  
  
  # GAM with cubic spline basis
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  
  
  # Linear model with natural splines
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  
  
  # Custom legend with specific colors for lines only
  scale_color_manual(name = "Model", 
                     values = c("Linear model" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Custom colors for confidence intervals
  scale_fill_manual(values = c("Linear model" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Remove legend for confidence intervals
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Number of guests", 
       y = "Log price",
       title = NULL)


#beds---------------------------------------------------------------------------
beds_scat <- ggplot(last_quarter, aes(x = beds, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Simple linear model (LM)
  geom_smooth(aes(color = "Linear model", fill = "Linear model"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  
  
  # GAM with cubic spline basis
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  
  
  # Linear model with natural splines
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  
  
  # Custom legend with specific colors for lines only
  scale_color_manual(name = "Model", 
                     values = c("Linear model" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Custom colors for confidence intervals
  scale_fill_manual(values = c("Linear model" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Remove legend for confidence intervals
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Number of beds", 
       y = "Log price",
       title = NULL)

#bathrooms_recoded--------------------------------------------------------------
bathrooms_scat <- ggplot(last_quarter, aes(x = bathrooms_recoded, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Simple linear model (LM)
  geom_smooth(aes(color = "Linear model", fill = "Linear model"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  
  
  # GAM with cubic spline basis
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  
  
  # Linear model with natural splines
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  
  
  # Custom legend with specific colors for lines only
  scale_color_manual(name = "Model", 
                     values = c("Linear model" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Custom colors for confidence intervals
  scale_fill_manual(values = c("Linear model" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Remove legend for confidence intervals
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Number of bathrooms", 
       y = "Log price",
       title = NULL)


# Combine plots into a 2x2 grid and collect legend at the bottom
final_plot <- (bedrooms_scat | accommodates_scat) / (beds_scat | bathrooms_scat) + plot_layout(guides = "collect")
final_plot

final_plot <- (bedrooms_scat | accommodates_scat) / (beds_scat | bathrooms_scat) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
        legend.spacing.x = unit(0.5, "cm"))  # Reduce spacing between legend entries

print(final_plot)
