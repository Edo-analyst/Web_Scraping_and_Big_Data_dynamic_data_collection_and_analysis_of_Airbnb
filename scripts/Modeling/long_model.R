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
load("dati_prof.RData")
length(unique(dati$house_id_num))

# Log of average prices over the 4 quarters
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
  labs(x = "Quarter", y = "Log mean price", shape = "City", col = "City") +
  theme_minimal() 


# Individual trajectories
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


# Models------------------------------------------------------------------------
m0 <- lme(lp  ~ as.numeric(id_period), ven,
          random=list(house_id_num= ~ as.numeric(id_period) ),
          correlation=corCAR1(form= ~ as.numeric(id_period)|house_id_num))
plot(m0)
summary(m0)

view(Loblolly)
m0 <- lme(height  ~ age ,Loblolly,
          random=list(Seed= ~ age),
          correlation=corCAR1(form= ~ age|Seed))
summary(m0)



# Mixed effects random intercept------------------------------------------------
library(nlme)
lme1 <- lme(lp ~ city * as.numeric(id_period), random = ~ 1 | house_id_num,
                  data = dati)
summary(lme1)
(0.5443611^2) /(0.5443611^2 + 0.2150244^2)   
AIC(lme1)

# Compare interactions----------------------------------------------------------
lme0_ml <- lme(lp ~ city + as.numeric(id_period), random = ~ 1 | house_id_num,
                   , method= "ML", data = dati)

lme1_ml <- lme(lp ~ city * as.numeric(id_period), random = ~ 1 | house_id_num,
                  , method="ML", data = dati)
anova(lme1_ml, lme0_ml)

# Mixed effects random intercept - AR1 errors-----------------------------------
lme1_ar1 <- lme(lp~ city*as.numeric(id_period), random = ~ 1 | house_id_num,
                   correlation = corAR1(form = ~ 1 | house_id_num),
                   data = dati)
summary(lme1_ar1)
(0.524727 ^2) /(0.524727^2 + 0.2579329^2)  
AIC(lme1_ar1)


# Mixed effects random intercept with variables - AR1 errors---------------------
lme2_ar1 <- lme(lp~ host_response_time + host_is_superhost + host_identity_verified + 
                   room_type + accommodates + beds + minimum_nights + maximum_nights + 
                   number_of_reviews + review_scores_rating + review_scores_accuracy + 
                   review_scores_cleanliness + review_scores_checkin + 
                   review_scores_communication + review_scores_location + 
                   review_scores_value + instant_bookable + bathrooms_recoded + 
                   bedrooms_recoded +property_type_recoded +
                   city*as.numeric(id_period), random = ~ 1 | house_id_num,
                   correlation = corAR1(form = ~ 1 | house_id_num),
                   data = dati)
summary(lme2_ar1)
(0.3786898^2)/(0.3786898^2 + 0.2625594^2)  
AIC(lme2_ar1)


# Compare random slopes---------------------------------------------------------
lme1_ar1_ml <- lme(lp~ city*as.numeric(id_period), random = ~ 1 | house_id_num,
                   correlation = corAR1(form = ~ 1 | house_id_num),
                   method = "ML",
                   data = dati)
lme3_ar1_ml <- lme(lp ~ city * as.numeric(id_period), 
                   random = ~ 1 + as.numeric(id_period) | house_id_num,
                   correlation = corAR1(form = ~ 1 | house_id_num),
                   method = "ML",
                   data = dati)   
anova(lme3_ar1_ml, lme1_ar1_ml)



# Mixed effects random intercept and slope with variables - AR1 errors-----------
lme4_ar1 <- lme(lp ~ host_response_time + host_is_superhost + host_identity_verified + 
                  room_type + accommodates + beds + minimum_nights + maximum_nights + 
                  number_of_reviews + review_scores_rating + review_scores_accuracy + 
                  review_scores_cleanliness + review_scores_checkin + 
                  review_scores_communication + review_scores_location + 
                  review_scores_value + instant_bookable + bathrooms_recoded + 
                  bedrooms_recoded +property_type_recoded +
                  city*as.numeric(id_period), 
                  random = ~ 1 + as.numeric(id_period) | house_id_num,
                  correlation = corAR1(form = ~ 1 | house_id_num),
                  data = dati)
summary(lme4_ar1)
AIC(lme4_ar1)
(0.39934522^2 + 0.05949297^2)/(0.39934522^2 + 0.05949297^2+0.22449214^2)


# Models for each city-------------------------------------------------------
mil<- dati %>% 
  filter(city=="Milan") 

ven<- dati %>% 
  filter(city=="Venice") 

rom<- dati %>% 
  filter(city=="Rome")

nap<- dati %>% 
  filter(city=="Naples") 

 
mil_ar1 <- lme(lp~ host_response_time + host_is_superhost + host_identity_verified + 
                 room_type + accommodates + beds + minimum_nights + maximum_nights + 
                 number_of_reviews + review_scores_rating + review_scores_accuracy + 
                 review_scores_cleanliness + review_scores_checkin + 
                 review_scores_communication + review_scores_location + 
                 review_scores_value + instant_bookable + bathrooms_recoded + 
                 bedrooms_recoded +property_type_recoded + as.numeric(id_period),
               random = ~ 1 | house_id_num,
               correlation = corAR1(form = ~ 1 | house_id_num),
               data = mil)
summary(mil_ar1)
AIC(mil_ar1)
(0.3863885  ^2)/(0.3863885  ^2 + 0.2847384^2) 

ven_ar1 <- lme(lp~ host_response_time + host_is_superhost + host_identity_verified + 
                 room_type + accommodates + beds + minimum_nights + maximum_nights + 
                 number_of_reviews + review_scores_rating + review_scores_accuracy + 
                 review_scores_cleanliness + review_scores_checkin + 
                 review_scores_communication + review_scores_location + 
                 review_scores_value + instant_bookable + bathrooms_recoded + 
                 bedrooms_recoded +property_type_recoded + as.numeric(id_period),
               random = ~ 1 | house_id_num,
               correlation = corAR1(form = ~ 1 | house_id_num),
               data = ven)
summary(ven_ar1)
AIC(ven_ar1)
(0.3608288   ^2)/(0.3608288   ^2 + 0.2742836^2)

rom_ar1 <- lme(lp~ host_response_time + host_is_superhost + host_identity_verified + 
                 room_type + accommodates + beds + minimum_nights + maximum_nights + 
                 number_of_reviews + review_scores_rating + review_scores_accuracy + 
                 review_scores_cleanliness + review_scores_checkin + 
                 review_scores_communication + review_scores_location + 
                 review_scores_value + instant_bookable + bathrooms_recoded + 
                 bedrooms_recoded +property_type_recoded + as.numeric(id_period),
               random = ~ 1 | house_id_num,
               correlation = corAR1(form = ~ 1 | house_id_num),
               data = rom)
summary(rom_ar1)
AIC(rom_ar1)
(0.3624197 ^2)/(0.3624197 ^2 + 0.2581382^2)

nap_ar1 <- lme(lp~ host_response_time + host_is_superhost + host_identity_verified + 
                 room_type + accommodates + beds + minimum_nights + maximum_nights + 
                 number_of_reviews + review_scores_rating + review_scores_accuracy + 
                 review_scores_cleanliness + review_scores_checkin + 
                 review_scores_communication + review_scores_location + 
                 review_scores_value + instant_bookable + bathrooms_recoded + 
                 bedrooms_recoded +property_type_recoded + as.numeric(id_period),
               random = ~ 1 | house_id_num,
               correlation = corAR1(form = ~ 1 | house_id_num),
               data = nap)
summary(nap_ar1)
(0.374006  ^2)/(0.374006  ^2 + 0.2147459^2)
