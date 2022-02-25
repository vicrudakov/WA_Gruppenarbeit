source("functions2.R") # Helfer-Funktionen
library(ggplot2) # fuer Funktion (f)


# a) 

# deskr_metr - eine Funktion, die verschiedene geeignete deskriptive 
#              Statistiken für metrische Variablen berechnet und ausgibt. 
#              Die Statistiken sind in Output aufgezaehlt. 

# Input: x - der Vektor mit Merkmalauspraegungen eines quantitatives Merkmals,
#            der metrische Skala verwendet.
# Output: eine bennante Liste der Statistiken, die Funktion zurueckgibt:
#          -"Mittelwert" - eine Zahl, Mittelwert,
#          -"Median" - eine Zahl, der Median
#          -"Quartile" - ein 2-elementiger num. Vektor, 0.25- und 0.75-Quantile
#          -"Extrempunkte" - ein 2-elementiger num. Vektor, Minimum und Maximum
#          -"Quartilabstand" - eine Zahl, Quartilabstand
#          -"MQA" - eine Zahl, mittl. quadratische Abweichung s^2,
#               s^2 = (1/n) * sum((x[i] − mean(x))^2)
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

# deskr_metr - eine Funktion, die mehrere deskriptive Statistiken
#              fuer metrische Variablen berechnet und ausgibt. 
#              Die Statistiken sind in Output aufgezaehlt. 
#
# Input: x - der Vektor mit Merkmalauspraegungen eines quantitatives Merkmals,
#            der metrische Skala verwendet.
# Output: eine bennante Liste der Statistiken, die Funktion zurueckgibt:
#          - Mittelwert - eine Zahl, Mittelwert,
#          - Median - eine Zahl, der Median
#          - Quartile - ein 2-elementiger num. Vektor, 0.25- und 0.75-Quantile
#          - Extrempunkte - ein 2-elementiger num. Vektor, Minimum und Maximum
#          - Quartilabstand - eine Zahl, Quartilabstand
#          - MQA  - eine Zahl, mittl. quadratische Abweichung s^2,
#               s^2 = (1/n) * sum((x[i] - mean(x))^2)
#          - Variationskoeffizient - eine Zahl, Variationskoeffizient,
#            Dispersionsmass,  v = s/mean(x)

deskr_metr <- function(x){
  
  #Sicherheits-Check
  stopifnot(is.vector(x), !is.na(x))
  #Laenge des Datensatzes
  n = length(x)
  
  quant <- quantile(x, c(0.25, 0.5, 0.75))
  
  q_abst <- quant[3] - quant[1]
  names(q_abst) <- NULL
  
  mqa <- sum((x - mean(x))^2) / n
  
  return(list(Mittelwert = mean(x),
              Median = quant[2],
              Quartile = quant[-2],
              Extrempunkte = range(x),
              Quartilabstand = q_abst,
              MQA = mqa,
              Variationskoeffizient = (sqrt(mqa) / mean(x))
  ))
}


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


# c)

# zshg_deskr_kat - Eine Funktion, die geeignete deskriptive bivariate Statistiken 
#                 für den Zusammenhang zwischen zwei kategorialen Variablen 
#                 berechnet ausgibt
# Input: x - der Vektor mit Merkmalauspraegungen eines kategoriales Merkmals
#        y - der Vektor mit Merkmalauspraegungen eines kategoriales Merkmals
# Output: eine bennante Liste der briviate Statistiken, die Funktion zurueckgibt:
#          -"Chi^2" - eine Zahl, quadratische Kontingenz,
#               Chi^2 = sum l(sum k(hfgt[i,j]^2 / rand_x[i] * rand_y[j]))
#          -"Pearson's Kontingenzmass" - eine Zahll, Kontingenzkoeffizient nach 
#           Pearson
#                Kp = sqrt(chi2 / (chi2 + n))


zshg_deskr_kat <- function(x, y) {
  #Anzahl der Antworten
  n <- length(x)
  
  #Zweidimensionale Haeufigkeitsverteilung
  hfkt <- table(x,y)
  
  #Anzahl moeglicher Merkmalauspraegungen
  l <- length(unique(y))
  k <- length(unique(x))
  
  #Speichern von Randverteilung von X und Y
  rand_x <- randvert(hfkt, 1)
  rand_y <- randvert(hfkt, 2)
  names(rand_x) <- NULL
  names(rand_y) <- NULL
  
  #Berechnen von quadratischer Kontingenz chi^2
  chi2 <- 0
  for(i in 1:k){
    for(j in 1:l){
      temp <- (hfkt[i, j]^2)
      temp <- temp / (rand_x[i] * rand_y[j])
      chi2 <- chi2 + temp
    }
  }
  chi2 <- (chi2 - 1) * n
  
  #Berechnen von Pearson's Kontingenzmass
  pearson_koef <- sqrt(chi2 / (chi2 + n))
  
  return(list("Chi^2" = chi2,
              "Pearson's Kontingenzmass" = pearson_koef))
}


# d)

# deskr_biv - gibt deskriptive bivariate Statistiken fuer den Zusammenhang
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
#         korr_pearson_kont - korrigiertes Kontingenzmass von Pearson, in [0, 1]

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


# e) 

# quant_kat_ord <- eine Funktion, die eine mindestens ordinal skalierte
# Variable quantilbasiert kategorisiert und kategorisierte Werte und 
# Verteilung von Kategorien zurueckgibt.
#
# Input:
#  x - ein numerischer Vektor; Werte einer mindestens ordinal skalierten 
#      Variable
#  prob - ein numerischer Vektor; die Wahrscheinlichkeiten fuer die Quantilen, 
#          die von der Interesse sind, default = c(0.25, 0.75)
#  kat - ein character- oder String-Vektor; die Namen fuer Kategorien: wenn der 
#        Wert von x[j] zwischen den Quantilen fuer die Wahrscheinlichkeiten
#        prob[i] und prob[i+1] liegt, er wird zur Kategorie kat[i+1] zugeordnet,
#        i = 1, ..., m - 1, wobei m - Anzahl der Kategorien,
#        j = 1, ..., n, wobei n - Anzahl der Beobachtungen
#        die Werte x[j] < quantile(prob[1]) werden Kategorie kat[1] zugeordnet;
#        default: kat = c("niegrig", "mittel", "hoch")
#
# Output:  eine bennante Liste mit Elementen
#  Kategorisiert - ein character- oder String-Vektor;
#                  der Vektor der jeder Beobachtung zugeordneten Kategorien
#  Quantilen - eine Matrix mit 2 Zeilen; die W'keiten und dafuer berechnete
#              Quantilen
#  Verteilung - ein Object der Klasse table; die Haeufigkeitstabelle der 
#               verteilung von der zugeordneten Kategorien
#
#  Kommentar: in default Fall werden die Daten quartilbasiert kategorisiert,
#            also die Werte zwischen 1. und 3.Quartil werden Kategorie "mittel"
#            zugeordnet

quant_kat_ord <- function(x, prob = c(0.25, 0.75),
                          kat = c("niedrig", "mittel", "hoch")) {
  #Sicherheit-Check:
  stopifnot( is.vector(x), !is.na(x) )
  
  if( !is.numeric(x) )
    stop("Convert data to the numeric vector and try again")
  
  if( length(kat) != length(prob) + 1 )
    stop("Need more names for the categories")
  
  categories <- numeric(length(x))
  
  q <- quantile(x, prob)
  
  #Wenn 2 berechnete Quantile gleich sind, ist solche Kategorisation ist
  #nicht ganz sinnvoll, darum wird eine warnung-Befehl geworfen, die 
  #Kategorisation wird trotzdem durchgefuehrt
  
  if( length(unique(q)) != length(q) )
    warning("These quantiles are not really useful for the data")
  
  for( i in 1:length(q) ){
    categories[x >= q[i]] <- kat[i + 1]
  }
  
  categories[categories == "0"] <- kat[1]
  
  return( list(Kategorisiert = categories,
               Quantilen = q,
               Verteilung = table(categories)[kat]) )
}

# f)

# kat_vis - gibt ein Balkendiagramm zurueck, das die absolute Haeufigkeiten von
#           kategorialen Variablen veranschaulicht
#
#  Input:  daten - Vektor mit mehreren kategorialen Variablen
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
