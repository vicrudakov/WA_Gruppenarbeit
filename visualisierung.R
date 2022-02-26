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
