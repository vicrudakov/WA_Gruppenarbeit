source("functions2.R") # Helfer-Funktionen
library(ggplot2) # fuer Funktion (f)


# a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken
# für metrische Variablen berechnet und ausgibt


# b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für
# kategoriale Variablen berechnet und ausgibt


# c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
# Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt


# d)

# deskr_biv - gibt deskriptive bivariate Statistiken für den Zusammengang
#             zwischen einer metrischen und einer dichotomen Variablen zurueck
#
# Input:  daten_metr - Vektor mit metrischen Variablen
#         daten_dich - Vektor mit dichotomen Variablen
# 
# Output: eine benannte Liste:
#         kor - Korrelation, liegt in [0, 1]
#         kont_tafel - Kontingenztafel
#         phi - Phi-Koeffizient, normiert, liegt in [0, 1]
#         cramers_kont - Cramers Kontingenzmass, liegt in [0, 1]
#         korr_pearson_kont - korrigiertes Kontingenzmaß von Pearson, in [0, 1]

deskr_biv <- function(daten_metr, daten_dich) {
  stopifnot(is.vector(daten_metr), is.vector(daten_dich),
            is.numeric(daten_metr),
            length(unique(daten_dich)) == 2,
            !is.na(daten_metr), !is.na(daten_dich))
  
  kor <- cor(daten_metr, daten_dich)
  
  # bedingte Haeufigkeitstabelle wie data.frame
  tab <- table(daten_metr, daten_dich)
  df <- data.frame(rownames(tab), tab[, 1], tab[, 2])
  colnames(df) <- c("daten_metr", unique(daten_dich)[1], unique(daten_dich)[2])
  
  kont_tafel <- cbind(df, rowSums(tab))
  kont_tafel <- rbind(kont_tafel, c("Summe", colSums(tab), sum(rowSums(tab))))
  colnames(kont_tafel)[4] <- "Summe"
  
  # quadratische Kontingenz
  n <- as.numeric(kont_tafel[nrow(kont_tafel), 4])
  H_i_1 <- as.numeric(kont_tafel[1:(nrow(kont_tafel) - 1), 4]) *
    as.numeric(kont_tafel[nrow(kont_tafel), 2]) / n
  H_i_2 <- as.numeric(kont_tafel[1:(nrow(kont_tafel) - 1), 4]) *
    as.numeric(kont_tafel[nrow(kont_tafel), 3]) / n
  H_1 <- (as.numeric(kont_tafel[1:(nrow(kont_tafel) - 1), 2]) - H_i_1)^2 / H_i_1
  H_2 <- (as.numeric(kont_tafel[1:(nrow(kont_tafel) - 1), 3]) - H_i_2)^2 / H_i_2
  chi_q <- sum(H_1, H_2)
  
  phi <- sqrt(chi_q / n)
  
  # min(k, l) = 2 <=> dichotome Variable
  cramers_kont <- sqrt(chi_q / n)
  
  korr_pearson_kont <- sqrt(chi_q / (chi_q + n)) * sqrt(2)
  
  li <- list(kor = kor, kont_tafel = kont_tafel, phi = phi, 
             cramers_kont = cramers_kont, korr_pearson_kont = korr_pearson_kont)
  return(li)
}


# e) Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert
# kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)


# f)

# kat_vis - gibt ein Balkendiagramm zurueck, das die absolute Haeufigkeiten von
#           kategorialen Variablen veranschaulicht
#
# Input:  daten - Vektor mit mehreren kategorialen Variablen
# 
# Output: Grafik (intern als eine Liste)

kat_vis <- function(daten) {
  require(ggplot2)
  
  stopifnot(is.vector(daten),
            !is.na(daten),
            length(unique(daten)) > 1)
  
  df <- data.frame(table(daten))

  plot1 <- balkendiagramm(x = df$daten, y = df$Freq)
  
  return(plot1)
}

