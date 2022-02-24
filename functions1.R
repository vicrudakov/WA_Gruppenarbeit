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


# c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer den
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


# deskr.biv - gibt deskriptive bivariate Statistiken fuer den Zusammengang
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



#e)

#quant_kat_ord - eine Funktion, die eine mindestens ordinal skalierte Variable 
#                quantilbasiert kategorisiert. Quantilen und Namen von 
#                entsprechenden Kategorien selbst eingeben. Wenn der i-te Wert 
#                zwischen zwei bestimmten Quantilen liegt, wird dem i-ten Element
#                des zurueckgegebenen Vektor entsprehende Kategorie zugewiesen.
#Input:
#  x - ein numerischer Vektor; die Werte mind. einer Ordinalskala
#  quant - ein numerischer Vektor; die Wahrscheinlichkeiten der zur  
#          Kategorisierung verwendeten Quantile,
#          default: 1.tes und 3.tes Quantile
#  kat - ein character- oder String-Vektor; die Namen fuer Kategorien,
#        default: "niedrig", mittel", "hoch".
#Output: eine benannte Liste aus folg. Elemente:
#  Kategorisation - ein Vektor mit string-Elementen, der jedem Element 
#                   zugewiesene Kategorie
#  Quantilen - berechnete Quantile zur Wahrscheinlichkeiten, die in quant
#              gegeben werden 
#  Vertfktn - Verteilung der Kategorisierungsergebnisse
#
#Kommentar: wenn zwei berechnete Quantile gleich sind, wird die Funktino 
#           warning ausgeben.

quant_kat_ord <- function(x, quant =  c(0.25, 0.75),
                          kat = c("niedrig", "mittel", "hoch")){
  #Sicherheits-Check
  stopifnot(is.vector(x), !is.na(x))
  if( length(kat) != length(quant) + 1 )
    stop("Need more names for the categories")
  if( !is.numeric(x) )
    stop("Transform to the numeric vector and try again")
  
  categories <- numeric(length(x))
  q <- quantile(x, quant)
  if(length(unique(q)) != length(quant))
    warning("These quantiles are not very useful in this case")
  
  i <- 1
  for(i in 1:length(q)){
    categories[x >= q[i]] <- kat[i + 1]
  }
  categories[categories == "0"] <- kat[1]
  
  return( list(Kategorisation = categories, Quantilen = rbind(quant, q), 
              Vertfktn = rev(table(categories))) )
  
}


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

