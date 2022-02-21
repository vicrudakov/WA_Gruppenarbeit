source("functions2.R") # Helfer-Funktionen


# a) 

# deskr_metr - eine Funktion, die verschiedene geeignete deskriptive 
#              Statistiken fuer metrische Variablen berechnet und ausgibt. 
#              Die Statistiken sind in Output aufgezaehlt. 

# Input: x - der Vektor mit Merkmalauspraegungen eines quantitatives Merkmals,
#            der metrische Skala verwendet.
# Output: eine benannte Liste der Statistiken, die Funktion zurueckgibt:
#          -"Mittelwert" - eine Zahl, Mittelwert,
#          -"Median" - eine Zahl, der Median
#          -"Quartile" - ein 2-elementiger num. Vektor, 0.25- und 0.75-Quantile
#          -"Extrempunkte" - ein 2-elementiger num. Vektor, Minimum und Maximum
#          -"Quartilabstand" - eine Zahl, Quartilabstand
#          -"MQA" - eine Zahl, mittl. quadratische Abweichung s^2,
#               s^2 = (1/n) * sum((x[i] ??? mean(x))^2)
#          -"Variationskoeffizient" - eine Zahl, Variationskoeffizient,
#               Dispersionsmass,  v = s/mean(x)

deskr_metr <- function(x){
  
  #Sicherheits-Check
  stopifnot(is.vector(x), !is.na(x))
  #Laenge des Datensatzes
  n = length(x)
  
  quant <- quantile(x, c(0.25, 0.5, 0.75))
  
  q_abst <- quant[3] - quant[1]
  names(q_abst) <- NULL
  
  mqa <- sum((x - mean(x))^2) / n
  
  return(list("Mittelwert" = mean(x),
              "Median" = quant[2],
              "Quartile" = quant[-2],
              "Extrempunkte" = range(x),
              "Quartilabstand" = q_abst,
              "MQA" = mqa,
              "Variationskoeffizient" = (sqrt(mqa) / mean(x))
  ))
}

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
#         korr.pearson.kont - korrigiertes Kontingenzmass von Pearson, in [0, 1]

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
# kategorisiert (z.B. in "niedrig", "mittel", "hoch")


# f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen erstellt

