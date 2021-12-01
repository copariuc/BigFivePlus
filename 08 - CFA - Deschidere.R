# Incarcarea librariilor necesare
library(dplyr); library(lavaan)

load("Date.Rdata")


# Selectia itemilor pentru DESCHIDERE
date.deschidere <- date %>%
  dplyr::select (I193, I197, I202, I208, I210, I217, I226, I232,
                 I194, I198, I203, I209, I211, I218, I227, I233,
                 I195, I199, I204, I212, I213, I219, I228, I234,
                 I196, I205, I214, I220, I223, I229, I235, I236,
                 I200, I206, I215, I221, I224, I230, I237, I239,
                 I201, I207, I216, I222, I225, I231, I238, I240)
# Constructia modelului original
m.deschidere <-'Imaginatie =~ I193 + I197 + I202 + I208 + I210 + I217 + I226 + I232
          Artistic =~ I194 + I198 + I203 + I209 + I211 + I218 + I227 + I233
          Emotionalitate =~ I195 + I199 + I204 + I212 + I213 + I219 + I228 + I234
          Aventurier =~ I196 + I205 + I214 + I220 + I223 + I229 + I235 + I236
          Intelect  =~ I200 + I206 + I215 + I221 + I224 + I230 + I237 + I239
          Liberalism =~ I201 + I207 + I216 + I222 + I225 + I231 + I238 + I240
          
          Deschidere =~ Imaginatie + Artistic + Emotionalitate + Aventurier +
                        Intelect + Liberalism'
# Evaluarea modelului initial
evaluare1 <- lavaan(model = m.deschidere, data = date.deschidere,
                    estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                    model.type = "cfa", ordered = names(date.deschidere), fixed.x = T)
Deschidere.ini <- summary(evaluare1, fit.measures = T, standardized = T, rsquare = T)

# Crearea tabelului cu incarcarile itemilor
load.tbl.1 <- as.data.frame(Deschidere.ini$PE[which(Deschidere.ini$PE$lhs == "Imaginatie" & 
                                                 Deschidere.ini$PE$op == "=~"),])
load.tbl.1 <- dplyr::select(load.tbl.1, 3, 5, 7, 8, 10)
load.tbl.1 <- cbind(rep("Imaginatie", times = 8), load.tbl.1)
names(load.tbl.1) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.2 <- as.data.frame(Deschidere.ini$PE[which(Deschidere.ini$PE$lhs == "Artistic" & 
                                                 Deschidere.ini$PE$op == "=~"),])
load.tbl.2 <- dplyr::select(load.tbl.2, 3, 5, 7, 8, 10)
load.tbl.2 <- cbind(rep("Artistic", times = 8), load.tbl.2)
names(load.tbl.2) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.3 <- as.data.frame(Deschidere.ini$PE[which(Deschidere.ini$PE$lhs == "Emotionalitate" & 
                                                 Deschidere.ini$PE$op == "=~"),])
load.tbl.3 <- dplyr::select(load.tbl.3, 3, 5, 7, 8, 10)
load.tbl.3 <- cbind(rep("Emotionalitate", times = 8), load.tbl.3)
names(load.tbl.3) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.4 <- as.data.frame(Deschidere.ini$PE[which(Deschidere.ini$PE$lhs == "Aventurier" & 
                                                 Deschidere.ini$PE$op == "=~"),])
load.tbl.4<- dplyr::select(load.tbl.4, 3, 5, 7, 8, 10)
load.tbl.4 <- cbind(rep("Aventurier", times = 8), load.tbl.4)
names(load.tbl.4) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.5 <- as.data.frame(Deschidere.ini$PE[which(Deschidere.ini$PE$lhs == "Intelect" & 
                                                 Deschidere.ini$PE$op == "=~"),])
load.tbl.5 <- dplyr::select(load.tbl.5, 3, 5, 7, 8, 10)
load.tbl.5 <- cbind(rep("Intelect", times = 8), load.tbl.5)
names(load.tbl.5) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.6 <- as.data.frame(Deschidere.ini$PE[which(Deschidere.ini$PE$lhs == "Liberalism" & 
                                                 Deschidere.ini$PE$op == "=~"),])
load.tbl.6 <- dplyr::select(load.tbl.6, 3, 5, 7, 8, 10)
load.tbl.6 <- cbind(rep("Liberalism", times = 8), load.tbl.6)
names(load.tbl.6) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

# Constructia modelului respecificat
r.deschidere <-'Imaginatie =~ I193 + I197 + I202 + I208 + I210 + I217 + I226 + I232
          Artistic =~ I194 + I198 + I203 + I209 + I211 + I218 + I227 + I233
          Emotionalitate =~ I199 + I204 + I212 + I213 + I219 + I228 + I234
          Aventurier =~ I196 + I205 + I214 + I220 + I223 + I229 + I235 + I236
          Intelect  =~ I206 + I215+ I224 + I237 + I239
          Liberalism =~ I201 + I222 + I225 + I231 
          
          Deschidere =~ Imaginatie + Artistic + Emotionalitate + Aventurier +
                        Intelect + Liberalism'

# Evaluarea modelului respecificat
evaluare2 <- lavaan(model = r.deschidere, data = date.deschidere,
                         estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                         model.type = "cfa", ordered = names(date.deschidere), fixed.x = T)

Deschidere.fin <- summary(evaluare2, fit.measures = T, standardized = T, rsquare = T)
