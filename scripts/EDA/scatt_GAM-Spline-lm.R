library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(forcats)
library(splines)
library(mgcv)
library(patchwork)
load("last_quarter.RData")

# bedrooms_recoded---------------------------------------------------------------
bedrooms_scat <- ggplot(last_quarter, aes(x = bedrooms_recoded, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Simple linear model (LM)
  geom_smooth(aes(color = "Linear Model", fill = "Linear Model"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  
  
  # GAM with cubic spline
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  
  
  # Linear model with natural splines
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  
  
  # Custom legend with colors only for the lines
  scale_color_manual(name = "Model", 
                     values = c("Linear Model" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Remove fill legend (confidence interval colors)
  scale_fill_manual(values = c("Linear Model" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Remove legend for confidence interval
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Number of bedrooms", 
       y = "Log price",
       title = NULL)


# accommodates-------------------------------------------------------------------
accommodates_scat <- ggplot(last_quarter, aes(x = accommodates, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Simple linear model (LM)
  geom_smooth(aes(color = "Linear Model", fill = "Linear Model"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  
  
  # GAM with cubic spline
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  
  
  # Linear model with natural splines
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  
  
  # Custom legend with colors only for the lines
  scale_color_manual(name = "Model", 
                     values = c("Linear Model" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Remove fill legend (confidence interval colors)
  scale_fill_manual(values = c("Linear Model" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Remove legend for confidence interval
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Number of guests", 
       y = "Log price",
       title = NULL)


# beds---------------------------------------------------------------------------
beds_scat <- ggplot(last_quarter, aes(x = beds, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Simple linear model (LM)
  geom_smooth(aes(color = "Linear Model", fill = "Linear Model"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  
  
  # GAM with cubic spline
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  
  
  # Linear model with natural splines
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  
  
  # Custom legend with colors only for the lines
  scale_color_manual(name = "Model", 
                     values = c("Linear Model" = "blue",
                                "GAM" = "red",
