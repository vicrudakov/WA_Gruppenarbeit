source("functions1.R")
daten <- read.csv("results.csv")

Alter <- daten$Alter
Studiengang <- daten$Studiengang
Interesse_Mathe <- daten$Interesse_Mathe
Interesse_Progr <- daten$Interesse_Progr
Mathe_LK <- daten$Mathe_LK

### Alter 
## Deskription
deskr_metr(Alter)
# $Mittelwert
# [1] 24.81
# 
# $Median
# 50% 
# 25 
# 
# $Quartile
# 25% 75% 
#   24  26 
# 
# $Extrempunkte
# [1] 20 30
# 
# $MQA
# [1] 3.7339
# 
# $Variationskoeffizient
# [1] 0.07788514
## Kategorisierung 
Alter_Kat <- quant_kat_ord(Alter, prob = c(0.25, 0.5, 0.75),
                           kat = c("20 - 23", "24", "25", "26 - 30"))
deskr_kat(Alter_Kat$Kategorisiert)
# $haeuf_tab
#     daten abs_haeuf rel_haeuf
# 1 20 - 23        21      0.21
# 2      24        14      0.14
# 3      25        30      0.30
# 4 26 - 30        35      0.35
# 
# $modus
# [1] "26 - 30"
## Visualisierung
kat_vis(Alter_Kat$Kategorisiert)


### Studiengang
## Deskription
deskr_kat(Studiengang)
# $haeuf_tab
#          daten abs_haeuf rel_haeuf
# 1 Data Science        33      0.33
# 2   Informatik        23      0.23
# 3   Mathematik         9      0.09
# 4    Statistik        35      0.35
# 
# $modus
# [1] "Statistik"
## Visualisierung
kat_vis(Studiengang)


### Interesse_Mathe
## Kategorisierung
Interesse_Mathe_Kat <- quant_kat_ord(Interesse_Mathe, prob = c(0.04, 0.8))
## Zusammenhang mit Studiengang
zshg_deskr_kat(Interesse_Mathe_Kat$Kategorisiert, Studiengang)
# $Phi
# [1] 0.7607515
# 
# $Korr_Pearson
# [1] 0.7415369

## Genaue Zahlen anhand von Studiengang
Interesse_Mathe_Statistik <- daten[daten$Studiengang == "Statistik", ]$Interesse_Mathe
deskr_metr(Interesse_Mathe_Statistik)
# $Mittelwert
# [1] 4.8
# 
# $Median
# 50% 
#   5 
Interesse_Mathe_DS <- daten[daten$Studiengang == "Data Science", ]$Interesse_Mathe
deskr_metr(Interesse_Mathe_DS)
# $Mittelwert
# [1] 4.272727
# 
# $Median
# 50% 
#   4 
Interesse_Mathe_Informatik <- daten[daten$Studiengang == "Informatik", ]$Interesse_Mathe
deskr_metr(Interesse_Mathe_Informatik)
# $Mittelwert
# [1] 3.347826
# 
# $Median
# 50% 
#   4 


### Interesse_Progr
## Kategorisierung
Interesse_Progr_Kat <- quant_kat_ord(Interesse_Progr, prob = c(0.1, 0.7))
## Zusammenhang mit Studiengang
zshg_deskr_kat(Interesse_Progr_Kat$Kategorisiert, Studiengang)
# $Phi
# [1] 0.7056172
# 
# $Korr_Pearson
# [1] 0.7061127

## Genaue Zahlen anhand von Studiengang
Interesse_Progr_Statistik <- daten[daten$Studiengang == "Statistik", ]$Interesse_Progr
deskr_metr(Interesse_Progr_Statistik)
# $Mittelwert
# [1] 2.942857
# 
# $Median
# 50% 
#   3 
Interesse_Progr_DS <- daten[daten$Studiengang == "Data Science", ]$Interesse_Progr
deskr_metr(Interesse_Progr_DS)
# $Mittelwert
# [1] 5.939394
# 
# $Median
# 50% 
#   6 
Interesse_Progr_Informatik <- daten[daten$Studiengang == "Informatik", ]$Interesse_Progr
deskr_metr(Interesse_Progr_Informatik)
# $Mittelwert
# [1] 6.130435
# 
# $Median
# 50% 
#   6


### Mathe-LK
## Zusammenhang mit Studiengang
zshg_deskr_kat(Mathe_LK, Studiengang)
# $Phi
# [1] 0.2303672
# 
# $Korr_Pearson
# [1] 0.3174733
## Zusammenhang mit Interesse an Mathematik
deskr_biv(Interesse_Mathe, Mathe_LK)
# $kor
# [1] 0.1713571
# 
# $phi
# [1] 0.2754038
# 
# $cramers_kont
# [1] 0.2754038
# 
# $korr_pearson_kont
# [1] 0.3754997
## Zusammenhang mit Interesse an Programmieren
deskr_biv(Interesse_Progr, Mathe_LK)
# $kor
# [1] -0.1004273
# 
# $phi
# [1] 0.2613457
# 
# $cramers_kont
# [1] 0.2613457
# 
# $korr_pearson_kont
# [1] 0.3575884
