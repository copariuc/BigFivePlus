# Instalarea si incarcarea librariilor necesare
if(!require(psych)) install.packages("psych")
if(!require(nortest))  install.packages("nortest")
if(!require(dplyr))  install.packages("dplyr")

library(psych); library(nortest)
library(rstatix)

# Loading dataset
load("Date.total.Rdata")

# Functia de returnare a valorilor Skewness si Kurtosis ####
Skewness.Kurtosis = function(x){
  # Extragerea numarului de cazuri si a momentelor
  w = length(x) 
  m1 = mean(x, na.rm = T) 
  m2 = sum((x-m1)^2, na.rm = T)
  m3 = sum((x-m1)^3, na.rm = T)
  m4 = sum((x-m1)^4, na.rm = T)
  s1 = sd(x, na.rm = T)
  
  # Calculul Skewness si a erorii standard
  Skewness = w*m3/(w-1)/(w -2)/s1^3
  SE.Skewness = sqrt(6*w*(w-1)/((w-2)*(w+1)*(w+3)))
  # Calculul Kurtosis si a erorii standard
  Kurtosis = (w*(w+1)*m4-3*m2^2*(w-1))/((w-1)*(w-2)*(w-3)*s1^4)
  SE.Kurtosis= sqrt(4*(w^2-1)*SE.Skewness^2/((w-3)*(w+5)))
  
  # Generarea rezultatului si returnarea sa
  rez = matrix(c(Skewness, Kurtosis, SE.Skewness, SE.Kurtosis), 2, 
               dimnames = list(c("Skewness", "Kurtosis"), c("Value", "SE")))
  return(rez)}

# Extraversion ####
EXT <- date.tot %>% 
  dplyr::select(EXT.Frien, EXT.Grega, EXT.Asser, EXT.Activ, EXT.Excit, EXT.Cheer, EXTRAVER)
EXT.de <- describe(EXT, na.rm = F, trim = .05, IQR = F)
EXT.de <- EXT.de %>%
  dplyr::select(-vars, -mad, -se, -trimmed)
EXT.de$skew.se[1] <- Skewness.Kurtosis(date.tot$EXT.Frien)[3]; EXT.de$kurt.se[1] <- Skewness.Kurtosis(date.tot$EXT.Frien)[4]
EXT.de$skew.se[2] <- Skewness.Kurtosis(date.tot$EXT.Grega)[3]; EXT.de$kurt.se[2] <- Skewness.Kurtosis(date.tot$EXT.Grega)[4]
EXT.de$skew.se[3] <- Skewness.Kurtosis(date.tot$EXT.Asser)[3]; EXT.de$kurt.se[3] <- Skewness.Kurtosis(date.tot$EXT.Asser)[4]
EXT.de$skew.se[4] <- Skewness.Kurtosis(date.tot$EXT.Activ)[3]; EXT.de$kurt.se[4] <- Skewness.Kurtosis(date.tot$EXT.Activ)[4]
EXT.de$skew.se[5] <- Skewness.Kurtosis(date.tot$EXT.Excit)[3]; EXT.de$kurt.se[5] <- Skewness.Kurtosis(date.tot$EXT.Excit)[4]
EXT.de$skew.se[6] <- Skewness.Kurtosis(date.tot$EXT.Cheer)[3]; EXT.de$kurt.se[6] <- Skewness.Kurtosis(date.tot$EXT.Cheer)[4]
EXT.de$skew.se[7] <- Skewness.Kurtosis(date.tot$EXTRAVER)[3]; EXT.de$kurt.se[7] <- Skewness.Kurtosis(date.tot$EXTRAVER)[4]

EXT.de$AD.test[1] <- as.numeric(ad.test(date.tot$EXT.Frien)$statistic); EXT.de$p[1] <- as.numeric(ad.test(date.tot$EXT.Frien)$p.value)
EXT.de$AD.test[2] <- as.numeric(ad.test(date.tot$EXT.Grega)$statistic); EXT.de$p[2] <- as.numeric(ad.test(date.tot$EXT.Grega)$p.value)
EXT.de$AD.test[3] <- as.numeric(ad.test(date.tot$EXT.Asser)$statistic); EXT.de$p[3] <- as.numeric(ad.test(date.tot$EXT.Asser)$p.value)
EXT.de$AD.test[4] <- as.numeric(ad.test(date.tot$EXT.Activ)$statistic); EXT.de$p[4] <- as.numeric(ad.test(date.tot$EXT.Activ)$p.value)
EXT.de$AD.test[5] <- as.numeric(ad.test(date.tot$EXT.Excit)$statistic); EXT.de$p[5] <- as.numeric(ad.test(date.tot$EXT.Excit)$p.value)
EXT.de$AD.test[6] <- as.numeric(ad.test(date.tot$EXT.Cheer)$statistic); EXT.de$p[6] <- as.numeric(ad.test(date.tot$EXT.Cheer)$p.value)
EXT.de$AD.test[7] <- as.numeric(ad.test(date.tot$EXTRAVER)$statistic); EXT.de$p[7] <- as.numeric(ad.test(date.tot$EXTRAVER)$p.value)

rownames(EXT.de) <- c("Friendliness", "Gregariousness", "Assertiveness",
                      "Activity level", "Excitement seeking", "Cheerfulness",
                      "Extraversion"); cat("\14"); EXT.de
# Extreme score analysis
boxplot(date.tot$EXT.Frien,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$EXT.Frien); outliers$out
EXT %>% rstatix::identify_outliers(EXT.Frien)
boxplot(date.tot$EXT.Grega,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$EXT.Grega); outliers$out
EXT %>% rstatix::identify_outliers(EXT.Grega)
boxplot(date.tot$EXT.Asser,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$EXT.Asser); outliers$out
EXT %>% rstatix::identify_outliers(EXT.Asser)
boxplot(date.tot$EXT.Activ,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$EXT.Activ); outliers$out
EXT %>% rstatix::identify_outliers(EXT.Activ)
boxplot(date.tot$EXT.Excit,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$EXT.Excit); outliers$out
EXT %>% rstatix::identify_outliers(EXT.Excit)
boxplot(date.tot$EXT.Cheer,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$EXT.Cheer); outliers$out
EXT %>% rstatix::identify_outliers(EXT.Cheer)
boxplot(date.tot$EXTRAVER,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$EXTRAVER); outliers$out
EXT %>% rstatix::identify_outliers(EXTRAVER)

# Agreeableness ####
AGR <- date.tot %>% 
  dplyr::select(AGR.Trust, AGR.Moral, AGR.Altru, AGR.Coope, AGR.Modes, AGR.Sympa, AGREEABL)
AGR.de <- describe(AGR, na.rm = F, trim = .05, IQR = F)
AGR.de <- AGR.de %>%
  dplyr::select(-vars, -mad, -se, -trimmed)
AGR.de$skew.se[1] <- Skewness.Kurtosis(date.tot$AGR.Trust)[3]; AGR.de$kurt.se[1] <- Skewness.Kurtosis(date.tot$AGR.Trust)[4]
AGR.de$skew.se[2] <- Skewness.Kurtosis(date.tot$AGR.Moral)[3]; AGR.de$kurt.se[2] <- Skewness.Kurtosis(date.tot$AGR.Moral)[4]
AGR.de$skew.se[3] <- Skewness.Kurtosis(date.tot$AGR.Altru)[3]; AGR.de$kurt.se[3] <- Skewness.Kurtosis(date.tot$AGR.Altru)[4]
AGR.de$skew.se[4] <- Skewness.Kurtosis(date.tot$AGR.Coope)[3]; AGR.de$kurt.se[4] <- Skewness.Kurtosis(date.tot$AGR.Coope)[4]
AGR.de$skew.se[5] <- Skewness.Kurtosis(date.tot$AGR.Modes)[3]; AGR.de$kurt.se[5] <- Skewness.Kurtosis(date.tot$AGR.Modes)[4]
AGR.de$skew.se[6] <- Skewness.Kurtosis(date.tot$AGR.Sympa)[3]; EXT.de$kurt.se[6] <- Skewness.Kurtosis(date.tot$AGR.Sympa)[4]
AGR.de$skew.se[7] <- Skewness.Kurtosis(date.tot$AGREEABL)[3]; AGR.de$kurt.se[7] <- Skewness.Kurtosis(date.tot$AGREEABL)[4]

AGR.de$AD.test[1] <- as.numeric(ad.test(date.tot$AGR.Trust)$statistic); AGR.de$p[1] <- as.numeric(ad.test(date.tot$AGR.Trust)$p.value)
AGR.de$AD.test[2] <- as.numeric(ad.test(date.tot$AGR.Moral)$statistic); AGR.de$p[2] <- as.numeric(ad.test(date.tot$AGR.Moral)$p.value)
AGR.de$AD.test[3] <- as.numeric(ad.test(date.tot$AGR.Altru)$statistic); AGR.de$p[3] <- as.numeric(ad.test(date.tot$AGR.Altru)$p.value)
AGR.de$AD.test[4] <- as.numeric(ad.test(date.tot$AGR.Coope)$statistic); AGR.de$p[4] <- as.numeric(ad.test(date.tot$AGR.Coope)$p.value)
AGR.de$AD.test[5] <- as.numeric(ad.test(date.tot$AGR.Modes)$statistic); AGR.de$p[5] <- as.numeric(ad.test(date.tot$AGR.Modes)$p.value)
AGR.de$AD.test[6] <- as.numeric(ad.test(date.tot$AGR.Sympa)$statistic); AGR.de$p[6] <- as.numeric(ad.test(date.tot$AGR.Sympa)$p.value)
AGR.de$AD.test[7] <- as.numeric(ad.test(date.tot$AGREEABL)$statistic); AGR.de$p[7] <- as.numeric(ad.test(date.tot$AGREEABL)$p.value)

rownames(AGR.de) <- c("Trust", "Morality", "Altruism",
                      "Cooperation", "Modesty", "Sympathy",
                      "Agreeableness"); cat("\14"); AGR.de
# Extreme score analysis
boxplot(date.tot$AGR.Trust,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$AGR.Trust); outliers$out
AGR %>% rstatix::identify_outliers(AGR.Trust)
# 0 is outlier, not extreme
boxplot(date.tot$AGR.Moral,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$AGR.Moral); outliers$out
AGR %>% rstatix::identify_outliers(AGR.Moral)
# Replace outliers with NA
AGR$AGR.Moral[which(AGR$AGR.Moral == 0)] <- NA
date.tot$AGR.Moral <- AGR$AGR.Moral
boxplot(date.tot$AGR.Altru,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$AGR.Altru); outliers$out
AGR %>% rstatix::identify_outliers(AGR.Altru)
# 0 is outlier, not extreme
boxplot(date.tot$AGR.Coope,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$AGR.Coope); outliers$out
AGR %>% rstatix::identify_outliers(AGR.Coope)
# Replace outliers with NA
AGR$AGR.Coope[which(AGR$AGR.Coope == 0)] <- NA
date.tot$AGR.Coope <- AGR$AGR.Coope
boxplot(date.tot$AGR.Modes,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$AGR.Modes); outliers$out
AGR %>% rstatix::identify_outliers(AGR.Modes)
boxplot(date.tot$AGR.Sympa,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$AGR.Sympa); outliers$out
AGR %>% rstatix::identify_outliers(AGR.Sympa)
# Less or equal with 12 are outliers, not extreme
boxplot(date.tot$AGREEABL,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$AGREEABL); outliers$out
AGR %>% rstatix::identify_outliers(AGREEABL)
# Replace outliers with NA
AGR$AGREEABL[which(AGR$AGREEABL <= 12)] <- NA
date.tot$AGREEABL <- AGR$AGREEABL
