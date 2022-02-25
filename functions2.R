# Helfer-Funktionen


# randvert - gibt Zeilen- oder Spaltensumme zurueck
#
# Input: t - eine Tabelle mit Zahlen
# dim - 1 - Zeilen, 2 - Spalten
#
# Output: Zeilen- oder Spaltensumme
randvert <- function(t, dim){
  return(apply(t, dim, sum))
}