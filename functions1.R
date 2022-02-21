source("functions2.R") # Helfer-Funktionen


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

# b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken für
# kategoriale Variablen berechnet und ausgibt


# c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
# Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt


# d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für den
# Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
# berechnet und ausgibt


# e) Eine Funktion, die eine mindestens ordinal skalierte Variable quantilbasiert
# kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)


# f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen erstellt

