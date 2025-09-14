
dati_def$property_type=NULL

# Funzione per estrarre i nomi delle variabili di tipo fattore
fattori <- function(dataset) {
  # Filtra le colonne che sono di tipo factor
  nomi_fattori <- names(dataset)[sapply(dataset, is.factor)]
  return(nomi_fattori)
}

# host_response_time
sort(table(dati_def$host_response_time), 
     decreasing = TRUE)/nrow(dati_def)
levels(dati_def$host_response_time)[levels(
  dati_def$host_response_time) %in% c("within a day"
              , "a few days or more")] <- "more time"

# host_is_superhost
sort(table(dati_def$host_is_superhost), 
     decreasing = TRUE)/nrow(dati_def)

# host_identity_verified
sort(table(dati_def$host_identity_verified), 
     decreasing = TRUE)/nrow(dati_def)

# property_type
sort(table(dati_def$property_type_recoded), 
     decreasing = TRUE)/nrow(dati_def)

# room_type
sort(table(dati_def$room_type), 
     decreasing = TRUE)/nrow(dati_def)
levels(dati_def$room_type)[levels(dati_def$room_type) 
      %in% c("Hotel room", "Private room", 
             "Shared room")] <- "Private/Shared/Hotel"


# instant_bookable
sort(table(dati_def$instant_bookable), 
     decreasing = TRUE)/nrow(dati_def)

# city
sort(table(dati_def$city), 
     decreasing = TRUE)/nrow(dati_def)



