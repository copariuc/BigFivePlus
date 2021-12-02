# Incarcarea librariilor necesare
library(dplyr); library(lavaan)

load("Date.Rdata")

# Selectia itemilor pentru AGREABILITATE
date.Agreab <- date %>%
  dplyr::select (I49, I53, I58, I64, I66, I73, I82, I88,
                 I50, I54, I59, I65, I67, I74, I83, I89, 
                 I51, I55, I60, I68, I69, I75, I84, I90,
                 I52, I61, I70, I76, I79, I85, I91, I92,
                 I56, I62, I71, I77, I80, I86, I93, I95,
                 I57, I63, I72, I78, I81, I87, I94, I96)
# Constructia modelului original
m.Agreab <-'Incredere =~ I49 + I53 + I58 + I64 + I66 + I73 + I82 + I88
          Moralitate =~ I50 + I54 + I59 + I65 + I67 + I74+ I83 + I89
          Altruism =~ I51 + I55 + I60 + I68 + I69 + I75 + I84 + I90
          Cooperare =~ I52 + I61 + I70 + I76 + I79 + I85 + I91 + I92
          Modestie  =~ I56 + I62 + I71 + I77 + I80 + I86 + I93 + I95
          Compasiune =~ I57 + I63 + I72 + I78 + I81 + I87 + I94 + I96
          
          Agreabilitate =~ Incredere + Moralitate + Altruism + Cooperare 
                          + Modestie + Compasiune'
# Evaluarea modelului initial
evaluare1 <- lavaan(model = m.Agreab, data = date.Agreab,
                    estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                    model.type = "cfa", ordered = names(date.Agreab), fixed.x = T)
Agreab.ini <- summary(evaluare1, fit.measures = T, standardized = T, rsquare = T)

# Crearea tabelului cu incarcarile itemilor
load.tbl.1 <- as.data.frame(Agreab.ini$PE[which(Agreab.ini$PE$lhs == "Incredere" & 
                                                 Agreab.ini$PE$op == "=~"),])
load.tbl.1 <- dplyr::select(load.tbl.1, 3, 5, 7, 8, 10)
load.tbl.1 <- cbind(rep("Incredere", times = 8), load.tbl.1)
names(load.tbl.1) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.2 <- as.data.frame(Agreab.ini$PE[which(Agreab.ini$PE$lhs == "Moralitate" & 
                                                 Agreab.ini$PE$op == "=~"),])
load.tbl.2 <- dplyr::select(load.tbl.2, 3, 5, 7, 8, 10)
load.tbl.2 <- cbind(rep("Moralitate", times = 8), load.tbl.2)
names(load.tbl.2) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.3 <- as.data.frame(Agreab.ini$PE[which(Agreab.ini$PE$lhs == "Altruism" & 
                                                 Agreab.ini$PE$op == "=~"),])
load.tbl.3 <- dplyr::select(load.tbl.3, 3, 5, 7, 8, 10)
load.tbl.3 <- cbind(rep("Altruism", times = 8), load.tbl.3)
names(load.tbl.3) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.4 <- as.data.frame(Agreab.ini$PE[which(Agreab.ini$PE$lhs == "Cooperare" & 
                                                 Agreab.ini$PE$op == "=~"),])
load.tbl.4<- dplyr::select(load.tbl.4, 3, 5, 7, 8, 10)
load.tbl.4 <- cbind(rep("Cooperare", times = 8), load.tbl.4)
names(load.tbl.4) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.5 <- as.data.frame(Agreab.ini$PE[which(Agreab.ini$PE$lhs == "Modestie" & 
                                                 Agreab.ini$PE$op == "=~"),])
load.tbl.5 <- dplyr::select(load.tbl.5, 3, 5, 7, 8, 10)
load.tbl.5 <- cbind(rep("Modestie", times = 8), load.tbl.5)
names(load.tbl.5) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.6 <- as.data.frame(Agreab.ini$PE[which(Agreab.ini$PE$lhs == "Compasiune" & 
                                                 Agreab.ini$PE$op == "=~"),])
load.tbl.6 <- dplyr::select(load.tbl.6, 3, 5, 7, 8, 10)
load.tbl.6 <- cbind(rep("Compasiune", times = 8), load.tbl.6)
names(load.tbl.6) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

# Respecificarea modelului cu factor de ordin II

# Evaluarea modelului respecificat



