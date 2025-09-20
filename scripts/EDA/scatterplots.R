

#bedrooms_recoded---------------------------------------------------------------
bedrooms_scat <- last_quarter %>%
  ggplot(aes(x = bedrooms_recoded, y = lp)) +  
  geom_point(alpha = 0.6, color = "black") +  # Scatter plot with light orange points
  # Spline line using a cubic spline basis ("cs") (solid red)
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs"),
              se = TRUE, 
              linetype = "solid", 
              color = "red") +
  theme_minimal() +
  labs(title = NULL,
       x = "Bedrooms",
       y = "Log Price")


#accommodates-------------------------------------------------------------------
accommodates_scat <- last_quarter %>%
  ggplot(aes(x = accommodates, y = lp)) +  
  geom_point(alpha = 0.6, color = "black") +  # Scatter plot with light orange points
  # Spline line using a cubic spline basis ("cs") (solid red)
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs"),
              se = TRUE, 
              linetype = "solid", 
              color = "red") +
  theme_minimal() +
  labs(title = NULL,
       x = "Guests",
       y = "Log Price")

#beds---------------------------------------------------------------------------
beds_scat <- last_quarter %>%
  ggplot(aes(x = beds, y = lp)) +  
  geom_point(alpha = 0.6, color = "black") +  # Scatter plot with light orange points
  # Spline line using a cubic spline basis ("cs") (solid red)
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs"),
              se = TRUE, 
              linetype = "solid", 
              color = "red") +
  theme_minimal() +
  labs(title = NULL,
       x = "Beds",
       y = "Log Price")

#bathrooms_recoded--------------------------------------------------------------
bathrooms_scat <- last_quarter %>%
  ggplot(aes(x = bathrooms_recoded, y = lp)) +  
  geom_point(alpha = 0.6, color = "black") +  # Scatter plot with light orange points
  # Spline line using a cubic spline basis ("cs") (solid red)
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs"),
              se = TRUE, 
              linetype = "solid", 
              color = "red") +
  theme_minimal() +
  labs(title = NULL,
       x = "Bathrooms",
       y = "Log Price")






