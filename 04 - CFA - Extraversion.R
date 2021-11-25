# Incarcarea librariilor necesare
library(dplyr); library(lavaan)

load("Date.Rdata")

# Selectia itemilor pentru EXTRAVERSIUNE
date.Extra <- date %>%
  dplyr::select (I1, I5, I10, I16, I18, I25, I34, I40,
          I2, I6, I11, I17, I19, I26, I35, I41,
          I3, I7, I12, I20, I21, I27, I36, I42,
          I4, I13, I22, I28, I31, I37, I43, I44,
          I8, I14, I23, I29, I32, I38, I45, I47,
          I9, I15, I24, I30, I33, I39, I46, I48)
# Constructia modelului original
m.Extraversion <- '
          Friendliness =~ I1 + I5 + I10 + I16 + I18 + I25 + I34 + I40
          Gregariousness =~ I2 + I6 + I11 + I17 + I19 + I26 + I35 + I41
          Assertiveness =~ I3 + I7 + I12 + I20 + I21 + I27 + I36 + I42
          Activity =~ I4 + I13 + I22 + I28 + I31 + I37 + I43 + I44
          Excitement  =~ I8 + I14 + I23 + I29 + I32 + I38 + I45 + I47
          Cheerfulness =~ I9 + I15 + I24 + I30 + I33 + I39 + I46 + I48
          
          Extraversion =~ Friendliness + Gregariousness + Assertiveness +
                          Activity + Excitement + Cheerfulness'
# Evaluarea modelului initial
evaluare.ini <- lavaan(model = m.Extraversion, data = date.Extra,
                     estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                     model.type = "cfa", ordered = names(date.Extra), fixed.x = T)
extraversion.ini <- summary(evaluare.ini, fit.measures = T, standardized = T, rsquare = T)

# Crearea tabelului cu incarcarile itemilor
load.tbl.1 <- as.data.frame(extraversion.ini$PE[which(extraversion.ini$PE$lhs == "Friendliness" & 
                                                        extraversion.ini$PE$op == "=~"),])
load.tbl.1 <- dplyr::select(load.tbl.1, 3, 5, 7, 8, 10)
load.tbl.1 <- cbind(rep("Friendliness", times = 8), load.tbl.1)
names(load.tbl.1) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.2 <- as.data.frame(extraversion.ini$PE[which(extraversion.ini$PE$lhs == "Gregariousness" & 
                                                        extraversion.ini$PE$op == "=~"),])
load.tbl.2 <- dplyr::select(load.tbl.2, 3, 5, 7, 8, 10)
load.tbl.2 <- cbind(rep("Gregariousness", times = 8), load.tbl.2)
names(load.tbl.2) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.3 <- as.data.frame(extraversion.ini$PE[which(extraversion.ini$PE$lhs == "Assertiveness" & 
                                                        extraversion.ini$PE$op == "=~"),])
load.tbl.3 <- dplyr::select(load.tbl.3, 3, 5, 7, 8, 10)
load.tbl.3 <- cbind(rep("Assertiveness", times = 8), load.tbl.3)
names(load.tbl.3) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.4 <- as.data.frame(extraversion.ini$PE[which(extraversion.ini$PE$lhs == "Activity" & 
                                                        extraversion.ini$PE$op == "=~"),])
load.tbl.4<- dplyr::select(load.tbl.4, 3, 5, 7, 8, 10)
load.tbl.4 <- cbind(rep("Activity", times = 8), load.tbl.4)
names(load.tbl.4) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.5 <- as.data.frame(extraversion.ini$PE[which(extraversion.ini$PE$lhs == "Excitement" & 
                                                        extraversion.ini$PE$op == "=~"),])
load.tbl.5 <- dplyr::select(load.tbl.5, 3, 5, 7, 8, 10)
load.tbl.5 <- cbind(rep("Excitement", times = 8), load.tbl.5)
names(load.tbl.5) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.6 <- as.data.frame(extraversion.ini$PE[which(extraversion.ini$PE$lhs == "Cheerfulness" & 
                                                        extraversion.ini$PE$op == "=~"),])
load.tbl.6 <- dplyr::select(load.tbl.6, 3, 5, 7, 8, 10)
load.tbl.6 <- cbind(rep("Cheerfulness", times = 8), load.tbl.6)
names(load.tbl.6) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

# Respecificarea modelului cu factor de ordin II
r.Extraversion <- '
          Friendliness =~ I1 + I5 + I10 + I16 + I18 + I25 + I34 + I40
          Gregariousness =~ I2 + I6 + I11 + I17 + I19 + I26 + I35 + I41
          Assertiveness =~ I3 + I12 + I20 + I21 + I27 + I36 + I42
          Activity =~ I4 + I13 + I22 + I43 + I44
          #Excitement  =~ I8 + I14 + I23 + I29 + I32 + I38 + I45 + I47
          Cheerfulness =~ I9 + I15 + I24 + I33 + I39 + I46 + I48
          
          Extraversion =~ Friendliness + Gregariousness + Assertiveness +
                          Activity + Cheerfulness
'
# Evaluarea modelului respecificat
evaluare.r <- lavaan(model = r.Extraversion, data = date.Extra,
                       estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                       model.type = "cfa", ordered = names(date.Extra), fixed.x = T)
extraversion.r <- summary(evaluare.r, fit.measures = T, standardized = T, rsquare = T)
