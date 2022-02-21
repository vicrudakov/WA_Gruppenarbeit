source("functions2.R") # Helfer-Funktionen


# a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# für metrische Variablen berechnet und ausgibt


# b)

# deskr_kat - gibt Modus/Modi und Haeufigkeitstabelle von Vektor mit kategoriale 
#             Merkmalen zurueck
#
# Input:  daten - Vektor mit kategoriale Merkmalen
# 
# Output: eine benannte Liste:
#         haeuf_tab - eine Haeufigkeitstabelle
#         modus - Modus/Modi von Vektor

deskr_kat <- function(daten) {
  stopifnot(is.vector(daten),
            !is.na(daten))
  
  df_daten <- data.frame(table(daten))
  df_daten <- cbind(df_daten, df_daten[, 2] / sum(df_daten[, 2]))
  colnames(df_daten) <- c("daten", "abs_haeuf", "rel_haeuf")
  
  modus <- as.vector(df_daten[which(df_daten[, 2] == max(df_daten[, 2])), 1])
  
  li <- list(haeuf_tab = df_daten, modus = modus)
  return(li)
}


# c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
# Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt


# d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
# Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
# berechnet und ausgibt


# e) Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert
# kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)


# f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen erstellt

