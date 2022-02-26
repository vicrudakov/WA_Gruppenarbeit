# Helfer-Funktionen

# balkendiagramm - gibt ein Balkendiagramm von y ~ x zurueck
#
# Input:  x - Vektor fuer x-Achse
#         y - Vektor fuer y-Achse
# 
# Output: Grafik (intern als eine Liste)

balkendiagramm <- function(x, y) {
  df <- data.frame(Variablen = x, Haeufigkeit = y)

  plot1 <- ggplot(df, aes(x = Variablen, y = Haeufigkeit)) + 
    geom_bar(stat = "identity", width = 0.5,
             fill = "steelblue", color = "black") +
    geom_text(aes(label = Haeufigkeit), vjust = -0.5) +
    ylim(0, 1.1 * max(df$Haeufigkeit)) +
    theme(axis.title = element_text(size = 13),
          axis.text = element_text(size = 13), 
          axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
          axis.title.y = element_text(margin = margin(0, 20, 0, 0)))
  
  return(plot1)
}

# randvert - gibt Zeilen- oder Spaltensumme zurueck
#
# Input: t - eine Tabelle mit Zahlen
# dim - 1 - Zeilen, 2 - Spalten
#
# Output: Zeilen- oder Spaltensumme

randvert <- function(t, dim){
  return(apply(t, dim, sum))
}
