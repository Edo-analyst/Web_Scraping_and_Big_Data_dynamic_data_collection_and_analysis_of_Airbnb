str(dati_puliti)

# Remove original bathrooms and bedrooms (only keep recoded versions)
dati_puliti$bathrooms=NULL
dati_puliti$bedrooms=NULL

# Function to compute number and percentage of NAs for each variable
na_get=function(dati){
  na_vars=sapply(dati, function(col)sum(is.na(col)))
  na_vars=sort(na_vars[na_vars>0])
  na_vars=data.frame(variabile=names(na_vars),
                     freq_assoluta=as.numeric(na_vars),
                     freq_relativa=round(as.numeric(na_vars)/nrow(dati),4))
  na_vars
}
na_tab=na_get(dati_puliti) 
na_tab <- na_tab[na_tab$freq_relativa >0,] 

# Remove variables with more than 70% missing values
variabili_da_rimuovere <- na_tab %>%
  filter(freq_relativa > 0.70) %>%
  pull(variabile)
dati_puliti <- dati_puliti %>%
  select(-one_of(variabili_da_rimuovere))

# Recompute NA summary after removal
na_tab=na_get(dati_puliti) 
na_tab <- na_tab[na_tab$freq_relativa >0,] 

# Find variables among NA columns that are factors
variabili_fattori <- sapply(dati_puliti[, na_tab$variabile], is.factor)
nomi_fattori <- names(variabili_fattori)[variabili_fattori]

# Replace NA values in factor variables with "Unknown"
for (colonna in nomi_fattori) {
  dati_puliti[[colonna]] <- factor(dati_puliti[[colonna]],
                                   levels = c(levels(dati_puliti[[colonna]]),"Unknown"))
  dati_puliti[[colonna]][is.na(dati_puliti[[colonna]])] <- "Unknown"
}

# Remove irrelevant columns
dati_puliti$host_name=NULL
dati_puliti$host_location=NULL
dati_puliti$host_since=NULL
dati_puliti$name=NULL

# Convert house_id into numeric format
options(scipen = 999)  # prevent scientific notation
dati_puliti$house_id_num<- as.numeric(dati_puliti$house_id)

# Recheck NA summary
na_tab=na_get(dati_puliti) 
na_tab <- na_tab[na_tab$freq_relativa >0,] 
na_tab

# Check price distribution and filter extreme values
summary(dati_puliti$price)
dati <- dati_puliti[!is.na(dati_puliti$price), ]    # remove rows with missing price
dati <- dati[dati$price <= 4000,]                   # keep only listings below 4000
summary(dati$price)

# Explore property types and recode into Top 4 vs "Other"
sort(table(dati$property_type), decreasing = T)
top4 <- names(sort(table(dati$property_type), decreasing = TRUE)[1:4])
dati$property_type_recoded <- ifelse(
  dati$property_type %in% top4, 
  as.character(dati$property_type), 
  "Other")
dati$property_type_recoded <- as.factor(dati$property_type_recoded)
table(dati$property_type_recoded)
dati$property_type=NULL   # drop original property_type

# Remove host response and acceptance rates (too many missing values or not relevant)
na_get(dati)
dati$host_response_rate=NULL
dati$host_acceptance_rate=NULL

# Final NA check
na_get(dati)

# Create final dataset without missing values
dati_def <- na.omit(dati)

