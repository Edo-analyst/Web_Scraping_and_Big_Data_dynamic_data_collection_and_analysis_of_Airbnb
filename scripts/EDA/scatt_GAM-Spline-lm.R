
library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(forcats)
library(splines)
library(mgcv)
library(patchwork)
load("last_quarter.RData")

#bedrooms_recoded---------------------------------------------------------------
bedrooms_scat <- ggplot(last_quarter, aes(x = bedrooms_recoded, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Modello lineare semplice (LM)
  geom_smooth(aes(color = "Modello lineare", fill = "Modello lineare"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  
  
  # GAM con spline (cubic spline basis)
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  
  
  # Modello lineare con splines (natural splines)
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  
  
  # Legenda personalizzata con colori specifici solo per le linee
  scale_color_manual(name = "Modello", 
                     values = c("Modello lineare" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Rimuovere la legenda per il "fill" (colori delle regioni di confidenza)
  scale_fill_manual(values = c("Modello lineare" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Rimuovere la legenda per la regione di confidenza
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Numero di stanze da letto", 
       y = "Log prezzo",
       title = NULL)


#accommodates-------------------------------------------------------------------
accommodates_scat <- ggplot(last_quarter, aes(x = accommodates, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Modello lineare semplice (LM)
  geom_smooth(aes(color = "Modello lineare", fill = "Modello lineare"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # GAM con spline (cubic spline basis)
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # Modello lineare con splines (natural splines)
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # Legenda personalizzata con colori specifici solo per le linee
  scale_color_manual(name = "Modello", 
                     values = c("Modello lineare" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Rimuovere la legenda per il "fill" (colori delle regioni di confidenza)
  scale_fill_manual(values = c("Modello lineare" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Rimuovere la legenda per la regione di confidenza
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Numero di ospiti", 
       y = "Log prezzo",
       title = NULL)


#beds---------------------------------------------------------------------------
beds_scat <- ggplot(last_quarter, aes(x = beds, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Modello lineare semplice (LM)
  geom_smooth(aes(color = "Modello lineare", fill = "Modello lineare"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # GAM con spline (cubic spline basis)
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # Modello lineare con splines (natural splines)
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # Legenda personalizzata con colori specifici solo per le linee
  scale_color_manual(name = "Modello", 
                     values = c("Modello lineare" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Rimuovere la legenda per il "fill" (colori delle regioni di confidenza)
  scale_fill_manual(values = c("Modello lineare" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Rimuovere la legenda per la regione di confidenza
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Numero di letti", 
       y = "Log prezzo",
       title = NULL)

#bathrooms_recoded--------------------------------------------------------------
bathrooms_scat <- ggplot(last_quarter, aes(x = bathrooms_recoded, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Modello lineare semplice (LM)
  geom_smooth(aes(color = "Modello lineare", fill = "Modello lineare"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # GAM con spline (cubic spline basis)
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # Modello lineare con splines (natural splines)
  geom_smooth(aes(color = "Splines", fill = "Splines"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +  # Regola l'alpha per la trasparenza
  
  # Legenda personalizzata con colori specifici solo per le linee
  scale_color_manual(name = "Modello", 
                     values = c("Modello lineare" = "blue",
                                "GAM" = "red",
                                "Splines" = "green")) +
  
  # Rimuovere la legenda per il "fill" (colori delle regioni di confidenza)
  scale_fill_manual(values = c("Modello lineare" = "blue",
                               "GAM" = "red",
                               "Splines" = "green")) +
  
  # Rimuovere la legenda per la regione di confidenza
  guides(fill = "none") +
  
  theme_minimal() +
  labs(x = "Numero di bagni", 
       y = "Log prezzo",
       title = NULL)


# Unire i grafici in una griglia 2x2 e raccogliere la legenda in basso
final_plot <- (bedrooms_scat | accommodates_scat) / (beds_scat | bathrooms_scat) + plot_layout(guides = "collect")
final_plot

final_plot <- (bedrooms_scat | accommodates_scat) / (beds_scat | bathrooms_scat) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8),
        legend.spacing.x = unit(0.5, "cm"))  # Riduci lo spazio tra le voci

print(final_plot)



#aggiunta di spline cubica---------------
library(ggplot2)
library(splines)  # Per ns() e bs()
library(mgcv)  # Per GAM

# Creazione del grafico con 4 modelli
plot_spline <- ggplot(last_quarter, aes(x = beds, y = lp)) +
  geom_point(alpha = 0.6, color = "black") +
  
  # Modello lineare semplice (LM)
  geom_smooth(aes(color = "Modello lineare", fill = "Modello lineare"), 
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, alpha = 0.2) +
  
  # GAM con spline (cubic spline basis)
  geom_smooth(aes(color = "GAM", fill = "GAM"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              se = TRUE, alpha = 0.2) +
  
  # Spline naturale
  geom_smooth(aes(color = "Spline Naturale", fill = "Spline Naturale"), 
              method = "lm", 
              formula = y ~ ns(x, df = 4), 
              se = TRUE, alpha = 0.2) +
  
  # Spline cubica
  geom_smooth(aes(color = "Spline Cubica", fill = "Spline Cubica"), 
              method = "lm", 
              formula = y ~ bs(x, degree = 3, df = 4), 
              se = TRUE, alpha = 0.2) +
  
  # Personalizzazione dei colori della legenda
  scale_color_manual(name = "Modello", 
                     values = c("Modello lineare" = "blue",
                                "GAM" = "red",
                                "Spline Naturale" = "green",
                                "Spline Cubica" = "purple")) +
  
  scale_fill_manual(values = c("Modello lineare" = "blue",
                               "GAM" = "red",
                               "Spline Naturale" = "green",
                               "Spline Cubica" = "purple")) +
  
  guides(fill = "none") +  # Rimuove la legenda per le aree di confidenza
  
  theme_minimal() +
  labs(x = "Numero di letti", 
       y = "Log prezzo",
       title = "Confronto tra diversi modelli di regressione")

# Stampare il grafico
print(plot_spline)









