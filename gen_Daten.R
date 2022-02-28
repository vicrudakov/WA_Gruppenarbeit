#Generieren einzelne Merkmalauspreagungen
#Alter
alter <- rnorm(100, 25, 2)
alter <- floor(alter)

#Studienfach
fach <- sample(c("Statistik", "Data Science", "Informatik", "Mathematik"), 100,
               replace = TRUE, prob = c(0.35, 0.35, 0.2, 0.1))

#Anzahl Studierenden pro Fach
stud_m <- length(fach[fach == "Mathematik"])
stud_i <- length(fach[fach == "Informatik"])
stud_s <- length(fach[fach == "Statistik"])
stud_ds <- length(fach[fach == "Data Science"])

#Interesse fuer Mathe
mathe <- numeric(100)
mathe[fach == "Mathematik"] <- sample(6:7, stud_m, replace = TRUE)
mathe[fach == "Informatik"] <- sample(1:5, stud_i, replace = TRUE)
mathe[fach == "Data Science"] <- sample(3:5, stud_ds, replace = TRUE)
mathe[fach == "Statistik"] <- sample(4:6, stud_s, replace = TRUE)

#Interesse Programmierung
info <- numeric(100)
info[fach == "Mathematik"] <- sample(1:5, stud_m, replace = TRUE)
info[fach == "Informatik"] <- sample(5:7, stud_i, replace = TRUE)
info[fach == "Data Science"] <- sample(5:7, stud_ds, replace = TRUE)
info[fach == "Statistik"] <- sample(1:5, stud_s, replace = TRUE)

#Mathe - LK
m_lk <- numeric(100)
m_lk[fach == "Mathematik"] <- sample(0:1, stud_m, replace = TRUE, prob = c(0.05, 0.95))
m_lk[fach == "Informatik"] <- sample(0:1, stud_i, replace = TRUE, prob = c(0.4, 0.6))
m_lk[fach == "Data Science"] <- sample(0:1, stud_ds, replace = TRUE, prob = c(0.4, 0.6))
m_lk[fach == "Statistik"] <- sample(0:1, stud_s, replace = TRUE, prob = c(0.3, 0.7))

#Generieren von IDs
id <- sample(210000:230000, 100)

#Zusammenfassung in einer data.frame
results <- data.frame(id, alter, fach, mathe, info, m_lk)
colnames(results) <- c("ID", "Alter", "Studiengang", "Interesse_Mathe", 
                       "Interesse_Progr", "Mathe_LK")


#Hinzufuegen von Ausreissern
results[7,4] <- 3
results[90,6] <- 0
results[35,5] <- 6
results[40,4] <- 7


#Konvertation in csv.format
write.csv(results, 
          file = "D:/Маша/TUDO/3 семестр/Wissenschaft. Arbeit/WA_Gruppenarbeit/WA_Rcode/results.csv")
