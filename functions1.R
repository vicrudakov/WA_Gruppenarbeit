source("functions2.R") # Helfer-Funktionen


# a) 


# b) 


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


zshg_deskr_kat <- function(x, y){
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


# e) 


# f) 

