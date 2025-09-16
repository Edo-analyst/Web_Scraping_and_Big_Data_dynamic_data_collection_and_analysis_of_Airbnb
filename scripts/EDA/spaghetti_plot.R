library(ggplot2)
library(dplyr)
library(patchwork)

# Creazione del grafico per la distribuzione sulla scala originale (price)
grafico1 <- dati_def %>%
  filter(host_id == "538114385") %>% 
  mutate(house_id_label = sub(".*00(\\d+)", "\\1", as.character(house_id_num)),  # Extract the numbers after "00"
         house_id_label = as.factor(house_id_label)) %>%  # Convert to factor for sorting
  mutate(house_id_label = factor(house_id_label, 
                                 levels = sort(as.numeric(levels(house_id_label))))) %>%  # Sort the numbers
  ggplot(aes(x = id_period, y = lp, group = house_id_num, color = house_id_label)) +
  geom_line() +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = NULL,
       x = "Trimestre",
       y = "Prezzo",
       color = "ID casa") +
  theme(legend.position = "right")

# Creazione del grafico per la distribuzione sulla scala logaritmica (log(price))
grafico2 <- dati_def %>%
  filter(host_id == "1478007") %>% 
  mutate(house_id_label = sub(".*00(\\d+)", "\\1", as.character(house_id_num)),  # Extract the numbers after "00"
         house_id_label = as.factor(house_id_label)) %>%  # Convert to factor for sorting
  mutate(house_id_label = factor(house_id_label, 
                                 levels = sort(as.numeric(levels(house_id_label))))) %>%  # Sort the numbers
  ggplot(aes(x = id_period, y = lp, group = house_id_num, color = house_id_label)) +
  geom_line() +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = NULL,
       x = "Trimestre",
       y = "Prezzo",
       color = "ID casa") +
  theme(legend.position = "right")


# Posizionare i grafici uno accanto all'altro
final_plot <- grafico1 + grafico2 +
  plot_layout(ncol = 2)

# Visualizza il risultato
print(final_plot)  

