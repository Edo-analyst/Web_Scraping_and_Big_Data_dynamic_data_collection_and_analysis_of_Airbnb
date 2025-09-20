library(ggplot2)

str(dati_puliti)
dati_puliti$host_name = NULL   # Remove host_name column (not useful for analysis)

# Find character columns
char_cols <- sapply(dati_puliti, is.character)

# Check for "N/A" values in character columns
na_columns <- sapply(dati_puliti[, char_cols], function(x) 
  any(x == "N/A", na.rm = TRUE))

# Return columns containing "N/A"
columns_with_na <- names(na_columns[na_columns == TRUE])
print(columns_with_na)

# Convert host_response_time into factor and clean levels
dati_puliti$host_response_time = as.factor(dati_puliti$host_response_time)
table(dati_puliti$host_response_time)

# Recode host_response_time with consistent categories
res_time_host=rep(NA,nrow(dati_puliti))
res_time_host[dati_puliti$host_response_time=="a few days or more"]="a few days or more"
res_time_host[dati_puliti$host_response_time=="within a day"]="within a day"
res_time_host[dati_puliti$host_response_time=="within an hour"]= "within an hour"
res_time_host[dati_puliti$host_response_time=="within a few hours"]="within a few hours"
res_time_host=as.factor(res_time_host)
dati_puliti$host_response_time=res_time_host
rm(res_time_host)

# Convert categorical variables to factors
dati_puliti$host_is_superhost<-as.factor(dati_puliti$host_is_superhost)
dati_puliti$host_identity_verified<-as.factor(dati_puliti$host_identity_verified)
dati_puliti$neighbourhood_group_cleansed<-as.factor(dati_puliti$neighbourhood_group_cleansed)
dati_puliti$neighbourhood_cleansed<-as.factor(dati_puliti$neighbourhood_cleansed)

# Clean host_response_rate: remove %, replace "N/A" with NA, convert to numeric
dati_puliti$host_response_rate <- gsub("%", "", dati_puliti$host_response_rate)  
dati_puliti$host_response_rate[dati_puliti$host_response_rate == "N/A"] <- NA    
dati_puliti$host_response_rate <- as.numeric(dati_puliti$host_response_rate)
head(dati_puliti$host_response_rate)

# Clean host_acceptance_rate: remove %, replace "N/A" with NA, convert to numeric
dati_puliti$host_acceptance_rate <- gsub("%", "", dati_puliti$host_acceptance_rate)  
dati_puliti$host_acceptance_rate[dati_puliti$host_acceptance_rate == "N/A"] <- NA    
dati_puliti$host_acceptance_rate <- as.numeric(dati_puliti$host_acceptance_rate)
head(dati_puliti$host_acceptance_rate)

# Explore property_type and room_type distribution
sort(table(dati_puliti$property_type), decreasing = T)
sort(table(dati_puliti$room_type), decreasing = T)

# Convert to factors
dati_puliti$room_type <- as.factor(dati_puliti$room_type)
dati_puliti$property_type <- as.factor(dati_puliti$property_type)
dati_puliti$instant_bookable<-as.factor(dati_puliti$instant_bookable)

# Clean price: remove "$" and commas, convert to numeric
head(dati_puliti$price)
sum(is.na(dati_puliti$price)) 
sum(grepl(",", dati_puliti$price))  
dati_puliti$price[grepl(",", dati_puliti$price)]  
dati_puliti$price <- as.numeric(gsub(",", "", gsub("\\$", "", dati_puliti$price)))
summary(dati_puliti$price)

# Convert city and id_period to factors
dati_puliti$city <- as.factor(dati_puliti$city)  
table(dati_puliti$city)
dati_puliti$id_period <- as.factor(dati_puliti$id_period)

# Function to extract factor variables
factors <- function(df) {
  variabili_fattoriali <- names(df)[sapply(df, is.factor)]
  return(variabili_fattoriali)
}
factor_names <- factors(dati_puliti)

# Frequency tables of categorical variables
sort(table(dati_puliti$host_response_time), decreasing = T)
sort(table(dati_puliti$host_is_superhost), decreasing = T)
sort(table(dati_puliti$host_identity_verified), decreasing = T)
sort(table(dati_puliti$neighbourhood_cleansed), decreasing = T)  
sort(table(dati_puliti$room_type), decreasing = T)
sort(table(dati_puliti$instant_bookable), decreasing = T)

# Clean house_id: replace underscores with "00"
dati_puliti$house_id <- gsub("\\_", "00", dati_puliti$house_id)

# Function to calculate NA statistics
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

# Plot missing values by variable
ggplot(na_tab, aes(x = reorder(variabile, freq_relativa), y = freq_relativa)) +
  geom_bar(stat = "identity", fill = "#404080", color = "white", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(x = "", y = "", title = "") +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 8, face = "bold"),  
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 10)
  ) +
  expand_limits(y = c(0, 0.70))  

# Bathrooms cleaning
summary(dati_puliti$bathrooms)  
sum(is.na((dati_puliti$bathrooms_text))) 
table(as.factor(dati_puliti$bathrooms_text))  

# Function to recode bathrooms from text
baths_num <- function(x) {
  if (is.na(x)) {
    return(NA)
  }
  if (grepl("half[- ]?bath", x, ignore.case = TRUE)) {
    return(0.5)
  } 
  num <- as.numeric(gsub("([0-9\\.]+).*", "\\1", x))
  return(num)
}

# Apply recoding
dati_puliti$bathrooms_recoded <- sapply(dati_puliti$bathrooms_text, baths_num)
sum(is.na(dati_puliti$bathrooms_recoded))
summary(dati_puliti$bathrooms_recoded)

# Compare original vs recoded bathrooms
dati_puliti$difference <- ifelse(
  !is.na(dati_puliti$bathrooms) & !is.na(dati_puliti$bathrooms_recoded), 
  abs(dati_puliti$bathrooms - dati_puliti$bathrooms_recoded), 
  NA
)
dati_da_visualizzare <- cbind(
  dati_puliti[, c("bathrooms", "bathrooms_text", "bathrooms_recoded")], 
  dati_puliti$difference
)

# Remove intermediate columns
dati_puliti$bathrooms_text=NULL
dati_puliti$difference=NULL
str(dati_puliti)

# Bedrooms cleaning
extract_bedrooms <- function(name) {
  match <- regexpr("\\d+\\s+bedroom", name, ignore.case = TRUE)
  if (match[1] != -1) {
    return(as.numeric(sub(" bedroom.*", "", regmatches(name, match))))
  } else {
    return(NA)
  }
}

# Fill missing bedrooms using listing name
sum(is.na(dati_puliti$bedrooms))  
dati_puliti <- dati_puliti %>%
  mutate(
    bedrooms_recoded = ifelse(is.na(bedrooms) & !is.na(name), 
                              sapply(name, extract_bedrooms), 
                              bedrooms)
  )

View(dati_puliti[,c("name", "bedrooms", "bedrooms_recoded")])
sum(is.na(dati_puliti$bedrooms_recoded)) 

# Create log(price) for further analysis
lp=log(dati_puliti$price)
dati_puliti<-cbind(dati_puliti,lp)
rm(lp)

# Update NA summary
na_tab=na_get(dati_puliti) 
na_tab <- na_tab[na_tab$freq_relativa >0,] 
na_tab

# Compare NA rates in bathrooms vs bedrooms
na_tab %>%
  filter(variabile %in% c("bathrooms", "bathrooms_recoded",
                          "bedrooms", "bedrooms_recoded")) %>%
  mutate(gruppo = case_when(
    variabile %in% c("bathrooms", "bathrooms_recoded") ~ "Bathrooms",
    variabile %in% c("bedrooms", "bedrooms_recoded") ~ "Bedrooms"
  )) %>%
  ggplot(aes(x = gruppo, y = freq_relativa, fill = variabile)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::percent(freq_relativa, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3      
  ) +
  labs(
    x = NULL, 
    y = NULL,
    fill = NULL 
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    legend.position = "top"
  )


