library(labelled)   # labeling data
library(rstatix)    # summary statistics
library(ggpubr)     # convenient summary statistics and plots
library(GGally)     # advanced plot
library(car)        # useful for anova/wald test
library(Epi)        # easy getting CI for model coef/pred
library(lme4)       # linear mixed-effects models
library(lmerTest)   # test for linear mixed-effects models
library(emmeans)    # marginal means
library(multcomp)   # CI for linear combinations of model coef
library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions
library(gt)         # nice tables
library(tidyverse)  # for everything (data manipulation, visualization, coding, and more)
theme_set(theme_minimal() + theme(legend.position = "bottom")) # theme for ggplot
library(ggplot2)
library(nlme)
library(dplyr)
library(tidyverse) 

options(scipen = 999)
load("dati_modelli.RData")

# Keep listings with 'maximum_nights' <= 1125
dati <- dati_def %>%
  filter(maximum_nights <= 1125) 
rm(dati_def)

# Keep listings for Milan, Venice, Rome, and Naples
dati <- dati %>%
  filter(city %in% c("Milan", "Venice", "Rome", "Naples")) %>%
  droplevels() 

# Keep houses that appear in all 4 quarters
dati <- dati %>%
  group_by(house_id_num) %>%
  filter(n_distinct(id_period) == 4) %>%
  ungroup()
table(dati$id_period)

#remove useless variables
dati$price=NULL  #use lp
dati$id=NULL
dati$host_id=NULL
dati$latitude=NULL
dati$longitude=NULL
dati$neighbourhood_cleansed=NULL
length(unique(dati$house_id_num))

# Log of average prices over the 4 quarters--------------------------------------------------
dati %>%
  group_by(city, id_period) %>%
  summarise(mean_ci_data = mean_ci(lp), .groups = "drop") %>%
  unnest_wider(mean_ci_data) %>%
  mutate(
    id_period = id_period, 
    agex = as.numeric(id_period) - 0.05 + 0.05 * (city == "Milan")  
  ) %>%
  ggplot(aes(x = agex, y, col = city, shape = city)) +  
  geom_point(size = 3) +  
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_line(size = 0.9) +
  labs(x = "Quarter", y = "Average Log Price", shape = "City", col = "City") +
  theme_minimal() 


# Individual trajectories--------------------------------------------------------------------
library(lattice)
# Select 5 random IDs per city
selected_ids <- dati %>%
  group_by(city) %>%            
  slice_sample(n = 5, replace = FALSE) %>%  
  pull(house_id_num) 
dati_filt <- dati %>%
  filter(house_id_num %in% selected_ids) %>%
  arrange(city, house_id_num)  
num_ids <- length(unique(dati_filt$house_id_num))
colors <- c("red", "brown", "yellow", "green")
xyplot(lp ~ id_period | factor(house_id_num, levels = unique(dati_filt$house_id_num)), 
       group = city,
       cex = 1,  # Increase point size
       par.settings = list(superpose.symbol = list(pch = 16, 
                                                   cex = 1.5,  
                                                   col = colors)),  
       as.table = TRUE, 
       auto.key = list(points = TRUE, columns = 2),
       data = dati_filt)
