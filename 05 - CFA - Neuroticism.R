# Incarcarea librariilor necesare
library(dplyr); library(lavaan)

load("Date.Rdata")


# Selectia itemilor pentru NEVROZISM
date.Nevro <- date %>% 
  dplyr::select (I97, I101, I106, I112, I114, I121, I130, I136,
                 I98, I102, I107, I113, I115, I122, I131, I137,
                 I99, I103, I108, I116, I117, I123, I132, I138,
                 I100, I109, I118, I124, I127, I133, I139, I140,
                 I104, I110, I119, I125, I128, I134, I141, I143,
                 I105, I111, I120, I126, I129, I135, I142, I144)
# Constructia modelului original
m.Nevro <-'Anxietate =~ I97 + I101 + I106 + I112 + I114 + I121 + I130 + I136
          Furie =~ I98 + I102 + I107 + I113 + I115 + I122 + I131 + I137
          Depresie =~ I99 + I103 + I108 + I116 + I117 + I123 + I132 + I138
          Timiditate =~ I100 + I109 + I118 + I124 + I127 + I133 + I139 + I140
          Exagerare  =~ I104 + I110 + I119 + I125 + I128 + I134 + I141 + I143
          Vulnerabil =~ I105 + I111 + I120 + I126 + I129 + I135 + I142 + I144
          
          Nevrozism =~ Anxietate + Furie + Depresie + Timiditate +
                        Exagerare + Vulnerabil'
# Evaluarea modelului initial
evaluare1 <- lavaan(model = m.Nevro, data = date.Nevro,
                       estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                       model.type = "cfa", ordered = names(date.Nevro), fixed.x = T)
nevro.ini <- summary(evaluare1, fit.measures = T, standardized = T, rsquare = T)

# Crearea tabelului cu incarcarile itemilor
load.tbl.1 <- as.data.frame(nevro.ini$PE[which(nevro.ini$PE$lhs == "Anxietate" & 
                                                        nevro.ini$PE$op == "=~"),])
load.tbl.1 <- dplyr::select(load.tbl.1, 3, 5, 7, 8, 10)
load.tbl.1 <- cbind(rep("Anxietate", times = 8), load.tbl.1)
names(load.tbl.1) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.2 <- as.data.frame(nevro.ini$PE[which(nevro.ini$PE$lhs == "Furie" & 
                                                        nevro.ini$PE$op == "=~"),])
load.tbl.2 <- dplyr::select(load.tbl.2, 3, 5, 7, 8, 10)
load.tbl.2 <- cbind(rep("Furie", times = 8), load.tbl.2)
names(load.tbl.2) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.3 <- as.data.frame(nevro.ini$PE[which(nevro.ini$PE$lhs == "Depresie" & 
                                                        nevro.ini$PE$op == "=~"),])
load.tbl.3 <- dplyr::select(load.tbl.3, 3, 5, 7, 8, 10)
load.tbl.3 <- cbind(rep("Depresie", times = 8), load.tbl.3)
names(load.tbl.3) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.4 <- as.data.frame(nevro.ini$PE[which(nevro.ini$PE$lhs == "Timiditate" & 
                                                        nevro.ini$PE$op == "=~"),])
load.tbl.4<- dplyr::select(load.tbl.4, 3, 5, 7, 8, 10)
load.tbl.4 <- cbind(rep("Timiditate", times = 8), load.tbl.4)
names(load.tbl.4) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.5 <- as.data.frame(nevro.ini$PE[which(nevro.ini$PE$lhs == "Exagerare" & 
                                                        nevro.ini$PE$op == "=~"),])
load.tbl.5 <- dplyr::select(load.tbl.5, 3, 5, 7, 8, 10)
load.tbl.5 <- cbind(rep("Exagerare", times = 8), load.tbl.5)
names(load.tbl.5) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

load.tbl.6 <- as.data.frame(nevro.ini$PE[which(nevro.ini$PE$lhs == "Vulnerabil" & 
                                                        nevro.ini$PE$op == "=~"),])
load.tbl.6 <- dplyr::select(load.tbl.6, 3, 5, 7, 8, 10)
load.tbl.6 <- cbind(rep("Vulnerabil", times = 8), load.tbl.6)
names(load.tbl.6) <- c("Latent trait", "Item", "B", "z", "p", "Beta")

# Respecificarea modelului cu factor de ordin II
m.Nevro.r <-'Anxietate =~ I97 + I101 + I106 + I112 + I114 + I121 + I130 + I136
          Furie =~ I98 + I102 + I107 + I113 + I115 + I122 + I131 + I137
          Depresie =~ I99 + I103 + I108 + I116 + I117 + I123 + I132 + I138
          Timiditate =~ I100 + I118 + I124 + I127 + I133 + I139 + I140
          Exagerare  =~ I104 + I119 + I125 + I128 + I134 + I141 + I143
          Vulnerabil =~ I105 + I120 + I126 + I129 + I135 + I142 + I144
          
          Nevrozism =~ Anxietate + Furie + Depresie + Timiditate +
                        Exagerare + Vulnerabil'

# Evaluarea modelului respecificat

evaluare2 <- lavaan(model = m.Nevro.r, data = date.Nevro,
                    estimator = "WLSM", test = "Satorra.Bentler", auto.var = T,
                    model.type = "cfa", ordered = names(date.Nevro), fixed.x = T)
nevro.r <- summary(evaluare2, fit.measures = T, standardized = T, rsquare = T)
