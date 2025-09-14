
str(dati_puliti)
dati_puliti$bathrooms=NULL
dati_puliti$bedrooms=NULL

# Funzione per ottenere il numero di valori NA
na_get=function(dati){
  na_vars=sapply(dati, function(col)sum(is.na(col)))
  na_vars=sort(na_vars[na_vars>0])
  na_vars=data.frame(variabile=names(na_vars),
                     freq_assoluta=as.numeric(na_vars),
                     freq_relativa=round(as.numeric(
                       na_vars)/nrow(dati),4))
  na_vars
}
na_tab=na_get(dati_puliti) 
na_tab <- na_tab[na_tab$freq_relativa >0,] 

# Rimuovi variabili con % di NA superiore al 70%
variabili_da_rimuovere <- na_tab %>%
  filter(freq_relativa > 0.70) %>%
  pull(variabile)
dati_puliti <- dati_puliti %>%
  select(-one_of(variabili_da_rimuovere))

na_tab=na_get(dati_puliti) 
na_tab <- na_tab[na_tab$freq_relativa >0,] 

# Trova le variabili che sono fattori
variabili_fattori <- sapply(dati_puliti[, 
                            na_tab$variabile], is.factor)
nomi_fattori <- names(variabili_fattori)[variabili_fattori]

# Sostituisci NA con 'Unknown' nelle variabili fattoriali
for (colonna in nomi_fattori) {
  dati_puliti[[colonna]] <- factor(dati_puliti[[colonna]],
                          levels = c(levels(
                          dati_puliti[[colonna]]),"Unknown"))
  dati_puliti[[colonna]][is.na(
    dati_puliti[[colonna]])] <- "Unknown"
}

# Rimuovi colonne inutili
dati_puliti$host_name=NULL
dati_puliti$host_location=NULL
dati_puliti$host_since=NULL
dati_puliti$name=NULL

# Converte house_id in formato numerico
# house_id num
options(scipen = 999)
dati_puliti$house_id<- as.numeric(dati_puliti$house_id)

na_tab=na_get(dati_puliti) 
na_tab <- na_tab[na_tab$freq_relativa >0,] 
na_tab

# Controlla i prezzi
summary(dati_puliti$price)
dati <- dati_puliti[!is.na(dati_puliti$price), ]    
dati <- dati[dati$price <= 4000,]
summary(dati$price)

# Controlla i tipi di proprieta'
sort(table(dati$property_type), decreasing = T)
top4 <- names(sort(table(dati$property_type), 
                   decreasing = TRUE)[1:4])
dati$property_type_recoded <- ifelse(
  dati$property_type %in% top4, 
  as.character(dati$property_type), 
  "Other")
dati$property_type_recoded <- as.factor(
  dati$property_type_recoded)
table(dati$property_type_recoded)
dati$property_type=NULL

# Rimuovi host_response_rate e host_acceptance_rate
na_get(dati)
dati$host_response_rate=NULL
dati$host_acceptance_rate=NULL

# Controlla NA
na_get(dati)
dati_def <- na.omit(dati)

