# Incarcarea librariilor necesare
library(dplyr); library(lavaan)

load("Date.Rdata")


# Selectia itemilor pentru ConstZISM
date.Const <- date %>% 
  dplyr::select (I145, I154, I160, I162, I169, I178, I184, I192, 
                 I146, I149, I155, I161, I163, I170, I179, I185, 
                 I147, I150, I156, I164, I165, I171, I180, I186, 
                 I148, I151, I157, I166, I172, I181, I187, I188, 
                 I152, I158, I167, I173, I175, I182, I189, I191, 
                 I153, I159, I168, I174, I176, I177, I183, I190)
# Constructia modelului original
m.Const <-'Eficienta =~ I145 + I154 + I160 + I162 + I169 + I178 + I184 + I192
          Ordine =~ I146 + I149 + I155 + I161 + I163 + I170 + I179 + I185
          Datorie =~ I147 + I150 + I156 + I164 + I165 + I171 + I180 + I186
          Ambitie =~  I148 + I151 + I157 + I166 + I172 + I181 + I187 + I188
          Perseverenta  =~  I152 + I158 + I167 + I173 + I175 + I182 + I189 + I191
          Prudenta =~  I153 + I159 + I168 + I174 + I176 + I177 + I183 + I190
          
          Constiinciozitate =~ Eficienta + Ordine + Datorie + Ambitie + Perseverenta
                        + Prudenta'
# Evaluarea modelului initial
evaluare1 <- lavaan(model = m.Const, data = date.Const,
                    estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                    model.type = "cfa", ordered = names(date.Const), fixed.x = T)
Const.ini <- summary(evaluare1, fit.measures = T, standardized = T, rsquare = T)

# Crearea tabelului cu incarcarile itemilor
load.tbl.1 <- as.data.frame(Const.ini$PE[which(Const.ini$PE$lhs == "Eficienta" & 
                                                 Const.ini$PE$op == "=~"),])
load.tbl.1 <- dplyr::select(load.tbl.1, 3, 5, 7, 8, 10)
load.tbl.1 <- cbind(rep("Eficienta", times = 8), load.tbl.1)
names(load.tbl.1) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.2 <- as.data.frame(Const.ini$PE[which(Const.ini$PE$lhs == "Ordine" & 
                                                 Const.ini$PE$op == "=~"),])
load.tbl.2 <- dplyr::select(load.tbl.2, 3, 5, 7, 8, 10)
load.tbl.2 <- cbind(rep("Ordine", times = 8), load.tbl.2)
names(load.tbl.2) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.3 <- as.data.frame(Const.ini$PE[which(Const.ini$PE$lhs == "Datorie" & 
                                                 Const.ini$PE$op == "=~"),])
load.tbl.3 <- dplyr::select(load.tbl.3, 3, 5, 7, 8, 10)
load.tbl.3 <- cbind(rep("Datorie", times = 8), load.tbl.3)
names(load.tbl.3) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.4 <- as.data.frame(Const.ini$PE[which(Const.ini$PE$lhs == "Ambitie" & 
                                                 Const.ini$PE$op == "=~"),])
load.tbl.4<- dplyr::select(load.tbl.4, 3, 5, 7, 8, 10)
load.tbl.4 <- cbind(rep("Ambitie", times = 8), load.tbl.4)
names(load.tbl.4) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.5 <- as.data.frame(Const.ini$PE[which(Const.ini$PE$lhs == "Perseverenta" & 
                                                 Const.ini$PE$op == "=~"),])
load.tbl.5 <- dplyr::select(load.tbl.5, 3, 5, 7, 8, 10)
load.tbl.5 <- cbind(rep("Perseverenta", times = 8), load.tbl.5)
names(load.tbl.5) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.6 <- as.data.frame(Const.ini$PE[which(Const.ini$PE$lhs == "Prudenta" & 
                                                 Const.ini$PE$op == "=~"),])
load.tbl.6 <- dplyr::select(load.tbl.6, 3, 5, 7, 8, 10)
load.tbl.6 <- cbind(rep("Prudenta", times = 8), load.tbl.6)
names(load.tbl.6) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

# Respecificarea modelului cu factor de ordin II

# Evaluarea modelului respecificat

evaluare2 <- lavaan(model = m.Const.r, data = date.Const,
                    estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                    model.type = "cfa", ordered = names(date.Const), fixed.x = T)
Const.r <- summary(evaluare2, fit.measures = T, standardized = T, rsquare = T)
