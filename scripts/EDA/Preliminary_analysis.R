
library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(forcats)
library(patchwork)

options(scipen = 999)
load("dati_modelli.RData")

variables <- c("lp","accommodates", "beds", "minimum_nights", 
               "maximum_nights", "number_of_reviews", "review_scores_rating", "review_scores_accuracy", 
               "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication", 
               "review_scores_location", "review_scores_value", "bathrooms_recoded", "bedrooms_recoded")


cities <- unique(dati_def$city)

for (c in cities) {
  cat("\nCittà:", c, "\n")  
  print(
    dati_def %>%
      filter(city == c) %>%
      select(neighbourhood_cleansed) %>%
      distinct()
  )
}

dati_def <- dati_def %>%
  filter(maximum_nights <= 1125) 

tabella_stats <- lapply(setNames(variables, variables), function(var) {
  dati_def %>% 
    summarize(
      mean   = mean(!!sym(var), na.rm = TRUE),
      std_dev    = sd(!!sym(var), na.rm = TRUE),
      Min      = min(!!sym(var), na.rm = TRUE),
      q_005 = quantile(!!sym(var), 0.05, na.rm = TRUE),
      q_025 = quantile(!!sym(var), 0.25, na.rm = TRUE),
      q_050 = quantile(!!sym(var), 0.50, na.rm = TRUE),
      q_075 = quantile(!!sym(var), 0.75, na.rm = TRUE),
      q_095 = quantile(!!sym(var), 0.95, na.rm = TRUE),
      Max      = max(!!sym(var), na.rm = TRUE)
    )
})
tab1 <- bind_rows(tabella_stats, .id = "variabile")
tab1 <- tab1 %>%
  mutate(across(c(mean, std_dev, Min, q_005, q_025, q_050, q_075, q_095, Max), ~ round(., 2)))
print(tab1)


#spaghetti plot host-listings---------------------------------------------------
dati_def %>%
  count(host_id) %>%
  filter(n >= 20 & n <= 30) %>%
  sample_n(2)  

source("spaghetti_plot.R")


#ANALYSIS LAST QUARTER----------------------------------------------------------

last_quarter <- dati_def %>%
  filter(id_period =="4")
save(file="last_quarter.RData",last_quarter)

#violin plot lp-----------------------------------------------------------------
source("violin_lp_city.R")
print(violin_plot)

#barplots-----------------------------------------------------------------------
################################################################################
source("barplots_factor.R")


cities1 <- c("Milan", "Venice", "Rome", "Naples", "Bologna")
cities2 <- c("Bergamo", "Florence", "Puglia", "Sicily", "Trentino")
factor_variables <- c("room_type", "host_response_time", "host_is_superhost", 
                      "host_identity_verified", "instant_bookable", "property_type_recoded")

for(name in factor_variables) {
  # Generate the plot for each factor variable
  plot_result <- create_factor_plot(last_quarter, name, cities1, cities2)
  print(plot_result)
}


create_factor_plot(last_quarter, "room_type", cities1, cities2, 
                   title1 = NULL, title2 = NULL)



#scatterplots-------------------------------------------------------------------
source("scatterplots.R")
print(bedrooms_scat)
print(accommodates_scat)
print(beds_scat)
print(bathrooms_scat)





