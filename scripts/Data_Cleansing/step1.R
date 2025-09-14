library(dplyr)
# Aggregazione automatica
city_names <- c("Bergamo", "Bologna", "Florence", "Milan",
                "Naples", "Puglia", "Rome", "Sicily", 
                "Trentino", "Venice")
for (city_name in city_names) {
  assign(paste0("dati_", city_name), data_listings(city_name))
}
dati_tmp <- ls(pattern = "^dati_")

# Aggiungere la citta'
for (name in dati_tmp) {
  df <- get(name)
  df$city <- sub("dati_", "", name)
  assign(name, df)
}

# id_period
assign_period <- function(dates) {
  dates <- sort(as.Date(dates))  
  breaks <- c(min(dates), as.Date(c("2024-03-01", "2024-06-01", 
                                    "2024-09-01")), max(dates))  
  periods <- cut(dates, breaks = breaks, labels = 1:4, 
                 include.lowest = TRUE)
  return(setNames(as.character(periods), dates))  
}

# Ciclo attraverso ogni dataset e aggiungo la nuova colonna
for (dati in dati_tmp) {
  df <- get(dati)  
  period_mapping <- assign_period(levels(
    as.factor(df$last_scraped)))
  df$id_period <- period_mapping[as.character(df$last_scraped)]
  assign(dati, df)
}

dati_full <- do.call(rbind, lapply(dati_tmp, get))
setwd("C:/Users/matez/Desktop/Pulizia_dataset")
save(file="dati_full.RData", dati_full)

# Verifica se il numero di righe e' corretto
sum(sapply(dati_tmp, function(df_name) nrow(get(df_name))))   
unique(dati_full$city)

# Rimuovo i dataset per singola citta'
for (name in dati_tmp) {
  rm(list = name)
}

# house_id (usando la variabile "id")
unique_houses <- dati_full %>%
  distinct(host_id, id, .keep_all = TRUE) %>%
  group_by(host_id) %>%
  mutate(house_number = row_number(),
         house_id = paste(host_id, house_number, sep = "_")) %>%
  ungroup()
dati_full <- dati_full %>%
  left_join(unique_houses %>% select(host_id, id, house_id),
            by = c("host_id", "id"))

# house_id (usando latitudine e longitudine)
unique_houses <- dati_full %>%
  distinct(host_id, latitude, longitude, .keep_all = TRUE) %>%
  group_by(host_id) %>%
  mutate(house_number = row_number(),
         house_id2 = paste(host_id, house_number, sep = "_")) %>%
  ungroup()
dati_full <- dati_full %>%
  left_join(unique_houses %>% select(host_id, latitude, 
                                     longitude, house_id2),
            by = c("host_id", "latitude", "longitude"))
str(dati_full)

dati <- dati_full[,-c(2:5,7,8,9,11,13:15,20:26,28,40,        
                      44:56,58:61,69,71:75)]
setdiff(colnames(dati_full), colnames(dati))  
rm(dati_full)
dati_puliti <- dati
rm(dati)

