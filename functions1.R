source("functions2.R") # Helfer-Funktionen


# a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# für metrische Variablen berechnet und ausgibt


# b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für
# kategoriale Variablen berechnet und ausgibt

# deskr.kat - gibt Modus/Modi und Haeufigkeitstabelle von Vektor mit kategoriale 
#             Merkmalen zurueck
#
# Input:  daten - Vektor mit kategoriale Merkmalen
# 
# Output: eine benannte Liste:
#         haeuf.tab - eine Haeufigkeitstabelle
#         modus - Modus/Modi von Vektor

deskr.kat <- function(daten) {
  stopifnot(is.vector(daten),
            !is.na(daten))
  
  df.daten <- data.frame(table(daten))
  df.daten <- cbind(df.daten, df.daten[, 2] / sum(df.daten[, 2]))
  colnames(df.daten) <- c("daten", "abs.haeuf", "rel.haeuf")
  
  modus <- as.vector(df.daten[which(df.daten[, 2] == max(df.daten[, 2])), 1])
  
  li <- list(haeuf.tab = df.daten, modus = modus)
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

