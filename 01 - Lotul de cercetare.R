# Incarcarea pachetelor necesare
library(foreign); library(expss); library(epiDisplay)
library(psych); library(nortest)

# Incarcarea structurii de date si ajustarea
date <- read.spss("Baza de date.sav", use.value.labels = T, to.data.frame = T)
date$online <- factor(date$online, labels = c("Paper-Pencil", "Online"))
date$gen <- factor(date$online, labels = c("Male", "Female"))
date$studii <- ordered(date$studii, labels = c("Illiterate", "Primary (4 years)", "Gymnasium (8 years)",
                                               "Arts and crafts school", "Highschool (12 years)", 
                                               "Post graduated school", "University (Bachelor level)", 
                                               "University (Master level)", "Doctoral school"))
# Saving data file
save(date, file = "Date.Rdata")
load("Date.Rdata")
# Distributia dupa principalele variabile
gender <- tab1(date$gen, graph = T,
               main = "Participants's gender",
               ylab = "Frequency", col = c("light blue", "pink")); gender$output.table
studies <- tab1(date$studii, graph = T, sort.group = "none",
            main = "Participants's educational level",
            ylab = "Frequency"); gender$output.table
online <- tab1(date$online, graph = T,
     main = "Type of data gathering",
     ylab = "Frequency", col = c("red", "green")); online$output.table
age <- describe(date$varsta)[c(-1, -6, -7, -13)]; age

