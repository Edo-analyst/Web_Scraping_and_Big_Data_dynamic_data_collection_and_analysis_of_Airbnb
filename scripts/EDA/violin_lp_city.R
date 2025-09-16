library(ggplot2)
violin_plot <- ggplot(last_quarter, aes(x = reorder(city, lp), y = lp, fill = city)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") + # Facoltativo, per aggiungere un boxplot
  labs(
    x = NULL,
    y = "Log Prezzo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5), # Riduci la dimensione del testo delle città e mettile orizzontali
    axis.title.x = element_text(size = 10), # Dimensione dell'etichetta asse x
    axis.title.y = element_text(size = 10), # Dimensione dell'etichetta asse y
    plot.title = element_blank(), # Rimuove il titolo
    legend.position = "none" # Rimuove la legenda
  )

