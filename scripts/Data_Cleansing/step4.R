dati_def$property_type=NULL   # Drop original property_type (only keep recoded version)

# Function to extract the names of factor variables
fattori <- function(dataset) {
  # Select columns that are factors
  nomi_fattori <- names(dataset)[sapply(dataset, is.factor)]
  return(nomi_fattori)
}

# Host response time: check distribution (as proportion of dataset)
sort(table(dati_def$host_response_time), decreasing = TRUE)/nrow(dati_def)

# Recode response time: merge "within a day" and "a few days or more" into "more time"
levels(dati_def$host_response_time)[levels(
  dati_def$host_response_time) %in% c("within a day", "a few days or more")] <- "more time"

# Host is superhost: check distribution (as proportion of dataset)
sort(table(dati_def$host_is_superhost), decreasing = TRUE)/nrow(dati_def)

# Host identity verified: check distribution (as proportion of dataset)
sort(table(dati_def$host_identity_verified), decreasing = TRUE)/nrow(dati_def)

# Property type (recoded): check distribution (as proportion of dataset)
sort(table(dati_def$property_type_recoded), decreasing = TRUE)/nrow(dati_def)

# Room type: check distribution and merge categories
sort(table(dati_def$room_type), decreasing = TRUE)/nrow(dati_def)

# Recode room_type: merge "Hotel room", "Private room", "Shared room" into "Private/Shared/Hotel"
levels(dati_def$room_type)[levels(dati_def$room_type) %in% 
      c("Hotel room", "Private room", "Shared room")] <- "Private/Shared/Hotel"

# Instant bookable: check distribution (as proportion of dataset)
sort(table(dati_def$instant_bookable), decreasing = TRUE)/nrow(dati_def)

# City: check distribution (as proportion of dataset)
sort(table(dati_def$city), decreasing = TRUE)/nrow(dati_def)

#Save the dataset created
save(file = "dati_modelli.RData", dati_def)

