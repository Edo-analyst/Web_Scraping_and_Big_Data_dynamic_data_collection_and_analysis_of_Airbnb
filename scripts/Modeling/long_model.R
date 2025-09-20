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





