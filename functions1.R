source("functions2.R") # Helfer-Funktionen


<<<<<<< Updated upstream
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
=======
# a) 


>>>>>>> Stashed changes


# c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
# Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt


# d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
# Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
# berechnet und ausgibt

# deskr.biv - gibt deskriptive bivariate Statistiken für den Zusammengang
#             zwischen einer metrischen und einer dichotomen Variablen zurueck
#
# Input:  daten.metr - Vektor mit metrischen Variablen
#         daten.dich - Vektor mit dichotomen Variablen
# 
# Output: eine benannte Liste:
#         kor - Korrelation, liegt in [0, 1]
#         kont.tafel - Kontingenztafel
#         phi - Phi-Koeffizient, normiert, liegt in [0, 1]
#         cramers.kont - Cramers Kontingenzmass, liegt in [0, 1]
#         korr.pearson.kont - korrigiertes Kontingenzmaß von Pearson, in [0, 1]

deskr.biv <- function(daten.metr, daten.dich) {
  stopifnot(is.vector(daten.metr), is.vector(daten.dich),
            is.numeric(daten.metr),
            length(unique(daten.dich)) == 2,
            !is.na(daten.metr), !is.na(daten.dich))
  
  kor <- cor(daten.metr, daten.dich)
  
  # bedingte Haeufigkeitstabelle wie data.frame
  tab <- table(daten.metr, daten.dich)
  df <- data.frame(rownames(tab), tab[, 1], tab[, 2])
  colnames(df) <- c("daten.metr", unique(daten.dich)[1], unique(daten.dich)[2])
  
  kont.tafel <- cbind(df, rowSums(tab))
  kont.tafel <- rbind(kont.tafel, c("Summe", colSums(tab), sum(rowSums(tab))))
  colnames(kont.tafel)[4] <- "Summe"
  
  # quadratische Kontingenz
  n <- as.numeric(kont.tafel[nrow(kont.tafel), 4])
  H.i.1 <- as.numeric(kont.tafel[1:(nrow(kont.tafel) - 1), 4]) *
    as.numeric(kont.tafel[nrow(kont.tafel), 2]) / n
  H.i.2 <- as.numeric(kont.tafel[1:(nrow(kont.tafel) - 1), 4]) *
    as.numeric(kont.tafel[nrow(kont.tafel), 3]) / n
  H.1 <- (as.numeric(kont.tafel[1:(nrow(kont.tafel) - 1), 2]) - H.i.1)^2 / H.i.1
  H.2 <- (as.numeric(kont.tafel[1:(nrow(kont.tafel) - 1), 3]) - H.i.2)^2 / H.i.2
  chi.q <- sum(H.1, H.2)
  
  phi <- sqrt(chi.q / n)
  
  # min(k, l) = 2 <=> dichotome Variable
  cramers.kont <- sqrt(chi.q / n)
  
  korr.pearson.kont <- sqrt(chi.q / (chi.q + n)) * sqrt(2)
  
  li <- list(kor = kor, kont.tafel = kont.tafel, phi = phi, 
             cramers.kont = cramers.kont, korr.pearson.kont = korr.pearson.kont)
  return(li)
}


# e) Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert
# kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)


# f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen erstellt

