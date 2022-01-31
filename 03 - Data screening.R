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
AGR.de$skew.se[6] <- Skewness.Kurtosis(date.tot$AGR.Sympa)[3]; AGR.de$kurt.se[6] <- Skewness.Kurtosis(date.tot$AGR.Sympa)[4]
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

# Neuroticism ####
NEU <- date.tot %>% 
  dplyr::select(NEU.Anxie, NEU.Anger, NEU.Depre, NEU.Timid, NEU.Immod, NEU.Vulne, NEUROTIC)
NEU.de <- describe(NEU, na.rm = F, trim = .05, IQR = F)
NEU.de <- NEU.de %>%
  dplyr::select(-vars, -mad, -se, -trimmed)
NEU.de$skew.se[1] <- Skewness.Kurtosis(date.tot$NEU.Anxie)[3]; NEU.de$kurt.se[1] <- Skewness.Kurtosis(date.tot$NEU.Anxie)[4]
NEU.de$skew.se[2] <- Skewness.Kurtosis(date.tot$NEU.Anger)[3]; NEU.de$kurt.se[2] <- Skewness.Kurtosis(date.tot$NEU.Anger)[4]
NEU.de$skew.se[3] <- Skewness.Kurtosis(date.tot$NEU.Depre)[3]; NEU.de$kurt.se[3] <- Skewness.Kurtosis(date.tot$NEU.Depre)[4]
NEU.de$skew.se[4] <- Skewness.Kurtosis(date.tot$NEU.Timid)[3]; NEU.de$kurt.se[4] <- Skewness.Kurtosis(date.tot$NEU.Timid)[4]
NEU.de$skew.se[5] <- Skewness.Kurtosis(date.tot$NEU.Immod)[3]; NEU.de$kurt.se[5] <- Skewness.Kurtosis(date.tot$NEU.Immod)[4]
NEU.de$skew.se[6] <- Skewness.Kurtosis(date.tot$NEU.Vulne)[3]; NEU.de$kurt.se[6] <- Skewness.Kurtosis(date.tot$NEU.Vulne)[4]
NEU.de$skew.se[7] <- Skewness.Kurtosis(date.tot$NEUROTIC)[3]; NEU.de$kurt.se[7] <- Skewness.Kurtosis(date.tot$NEUROTIC)[4]

NEU.de$AD.test[1] <- as.numeric(ad.test(date.tot$NEU.Anxie)$statistic); NEU.de$p[1] <- as.numeric(ad.test(date.tot$NEU.Anxie)$p.value)
NEU.de$AD.test[2] <- as.numeric(ad.test(date.tot$NEU.Anger)$statistic); NEU.de$p[2] <- as.numeric(ad.test(date.tot$NEU.Anger)$p.value)
NEU.de$AD.test[3] <- as.numeric(ad.test(date.tot$NEU.Depre)$statistic); NEU.de$p[3] <- as.numeric(ad.test(date.tot$NEU.Depre)$p.value)
NEU.de$AD.test[4] <- as.numeric(ad.test(date.tot$NEU.Timid)$statistic); NEU.de$p[4] <- as.numeric(ad.test(date.tot$NEU.Timid)$p.value)
NEU.de$AD.test[5] <- as.numeric(ad.test(date.tot$NEU.Immod)$statistic); NEU.de$p[5] <- as.numeric(ad.test(date.tot$NEU.Immod)$p.value)
NEU.de$AD.test[6] <- as.numeric(ad.test(date.tot$NEU.Vulne)$statistic); NEU.de$p[6] <- as.numeric(ad.test(date.tot$NEU.Vulne)$p.value)
NEU.de$AD.test[7] <- as.numeric(ad.test(date.tot$NEUROTIC)$statistic); NEU.de$p[7] <- as.numeric(ad.test(date.tot$NEUROTIC)$p.value)

rownames(NEU.de) <- c("Anxiety", "Anger", "Depression",
                      "Timidity", "Exaggeration", "Vulnerability",
                      "Neuroticism"); cat("\14"); NEU.de

# Extreme score analysis
# Anxietate
boxplot(date.tot$NEU.Anxie,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$NEU.Anxie); outliers$out
NEU %>% rstatix::identify_outliers(NEU.Anxie)

#Furie
boxplot(date.tot$NEU.Anger,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$NEU.Anger); outliers$out
NEU %>% rstatix::identify_outliers(NEU.Anger)

#Depresie
boxplot(date.tot$NEU.Depre,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$NEU.Depre); outliers$out
NEU %>% rstatix::identify_outliers(NEU.Depre)
# 8 is an outlier, but not extreme
# Replace outliers with NA
NEU$NEU.Depre[which(NEU$NEU.Depre == 8)] <- NA
date.tot$NEU.Depre <- NEU$NEU.Depre
date.tot$NEU.Depre

#Timiditate
boxplot(date.tot$NEU.Timid,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$NEU.Timid); outliers$out
NEU %>% rstatix::identify_outliers(NEU.Timid)

#Exagerare
boxplot(date.tot$NEU.Immod,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$NEU.Immod); outliers$out
NEU %>% rstatix::identify_outliers(NEU.Immod)

#Vulnerabilitate
boxplot(date.tot$NEU.Vulne,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$NEU.Vulne); outliers$out
NEU %>% rstatix::identify_outliers(NEU.Vulne)

# Neuroticism total
boxplot(date.tot$NEUROTIC,
       horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$NEUROTIC); outliers$out
NEU %>% rstatix::identify_outliers(NEUROTIC)


# Conscientiousness ####
CONS <- date.tot %>% 
  dplyr::select(CON.SelfE, CON.Order, CON.Dutif, CON.Achie, CON.SelfD, CON.Cauti, CONSCIEN)
CONS.de <- describe(CONS, na.rm = F, trim = .05, IQR = F)
CONS.de <- CONS.de %>%
  dplyr::select(-vars, -mad, -se, -trimmed)
CONS.de$skew.se[1] <- Skewness.Kurtosis(date.tot$CON.SelfE)[3]; CONS.de$kurt.se[1] <- Skewness.Kurtosis(date.tot$CON.SelfE)[4]
CONS.de$skew.se[2] <- Skewness.Kurtosis(date.tot$CON.Order)[3]; CONS.de$kurt.se[2] <- Skewness.Kurtosis(date.tot$CON.Order)[4]
CONS.de$skew.se[3] <- Skewness.Kurtosis(date.tot$CON.Dutif)[3]; CONS.de$kurt.se[3] <- Skewness.Kurtosis(date.tot$CON.Dutif)[4]
CONS.de$skew.se[4] <- Skewness.Kurtosis(date.tot$CON.Achie)[3]; CONS.de$kurt.se[4] <- Skewness.Kurtosis(date.tot$CON.Achie)[4]
CONS.de$skew.se[5] <- Skewness.Kurtosis(date.tot$CON.SelfD)[3]; CONS.de$kurt.se[5] <- Skewness.Kurtosis(date.tot$CON.SelfD)[4]
CONS.de$skew.se[6] <- Skewness.Kurtosis(date.tot$CON.Cauti)[3]; CONS.de$kurt.se[6] <- Skewness.Kurtosis(date.tot$CON.Cauti)[4]
CONS.de$skew.se[7] <- Skewness.Kurtosis(date.tot$CONSCIEN)[3]; CONS.de$kurt.se[7] <- Skewness.Kurtosis(date.tot$CONSCIEN)[4]

CONS.de$AD.test[1] <- as.numeric(ad.test(date.tot$CON.SelfE)$statistic); CONS.de$p[1] <- as.numeric(ad.test(date.tot$CON.SelfE)$p.value)
CONS.de$AD.test[2] <- as.numeric(ad.test(date.tot$CON.Order)$statistic); CONS.de$p[2] <- as.numeric(ad.test(date.tot$CON.Order)$p.value)
CONS.de$AD.test[3] <- as.numeric(ad.test(date.tot$CON.Dutif)$statistic); CONS.de$p[3] <- as.numeric(ad.test(date.tot$CON.Dutif)$p.value)
CONS.de$AD.test[4] <- as.numeric(ad.test(date.tot$CON.Achie)$statistic); CONS.de$p[4] <- as.numeric(ad.test(date.tot$CON.Achie)$p.value)
CONS.de$AD.test[5] <- as.numeric(ad.test(date.tot$CON.SelfD)$statistic); CONS.de$p[5] <- as.numeric(ad.test(date.tot$CON.SelfD)$p.value)
CONS.de$AD.test[6] <- as.numeric(ad.test(date.tot$CON.Cauti)$statistic); CONS.de$p[6] <- as.numeric(ad.test(date.tot$CON.Cauti)$p.value)
CONS.de$AD.test[7] <- as.numeric(ad.test(date.tot$CONSCIEN)$statistic); CONS.de$p[7] <- as.numeric(ad.test(date.tot$CONSCIEN)$p.value)

rownames(CONS.de) <- c("Self-efficacy", "Order", "Dutifulness",
                       "Achievement striving", "Perseverance", "Deliberation",
                       "Conscientiousnss"); cat("\14"); CONS.de

# Extreme score analysis
# Eficienta
boxplot(date.tot$CON.SelfE,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$CON.SelfE); outliers$out
CONS %>% rstatix::identify_outliers(CON.SelfE)

# Ordine
boxplot(date.tot$CON.Order,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$CON.Order); outliers$out
CONS %>% rstatix::identify_outliers(CON.Order)

# Datorie
boxplot(date.tot$CON.Dutif,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$CON.Dutif); outliers$out
CONS %>% rstatix::identify_outliers(CON.Dutif)
#  0,1 - outliers, but not extreme
# Inlocuierea outlierilor cu NA
CONS$CON.Dutif[which(CONS$CON.Dutif <= 1)] <- NA
date.tot$CON.Dutif <- CONS$CON.Dutif
date.tot$CON.Dutif

# Ambitie
boxplot(date.tot$CON.Achie,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$CON.Achie); outliers$out
CONS %>% rstatix::identify_outliers(CON.Achie)

# Perseverenta
boxplot(date.tot$CON.SelfD,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$CON.SelfD); outliers$out
CONS %>% rstatix::identify_outliers(CON.SelfD)

# Prudenta
boxplot(date.tot$CON.Cauti,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$CON.Cauti); outliers$out
CONS %>% rstatix::identify_outliers(CON.Cauti)

# Constiinciozitate total

boxplot(date.tot$CONSCIEN,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$CONSCIEN); outliers$out
CONS %>% rstatix::identify_outliers(CONSCIEN)
# Values equal to or less than 7 are outliers, not extreme
# Replace outliers with NA
CONS$CONSCIEN[which(CONS$CONSCIEN <= 7)] <- NA
date.tot$CONSCIEN <- CONS$CONSCIEN

# Openness ####
OPE <- date.tot %>% 
  dplyr::select(OPE.Imagi, OPE.Artis, OPE.Emoti, OPE.Adven, OPE.Intel, OPE.Liber, OPENNESS)
OPE.de <- describe(OPE, na.rm = F, trim = .05, IQR = F)
OPE.de <- OPE.de %>%
  dplyr::select(-vars, -mad, -se, -trimmed)
OPE.de$skew.se[1] <- Skewness.Kurtosis(date.tot$OPE.Imagi)[3]; OPE.de$kurt.se[1] <- Skewness.Kurtosis(date.tot$OPE.Imagi)[4]
OPE.de$skew.se[2] <- Skewness.Kurtosis(date.tot$OPE.Artis)[3]; OPE.de$kurt.se[2] <- Skewness.Kurtosis(date.tot$OPE.Artis)[4]
OPE.de$skew.se[3] <- Skewness.Kurtosis(date.tot$OPE.Emoti)[3]; OPE.de$kurt.se[3] <- Skewness.Kurtosis(date.tot$OPE.Emoti)[4]
OPE.de$skew.se[4] <- Skewness.Kurtosis(date.tot$OPE.Adven)[3]; OPE.de$kurt.se[4] <- Skewness.Kurtosis(date.tot$OPE.Adven)[4]
OPE.de$skew.se[5] <- Skewness.Kurtosis(date.tot$OPE.Intel)[3]; OPE.de$kurt.se[5] <- Skewness.Kurtosis(date.tot$OPE.Intel)[4]
OPE.de$skew.se[6] <- Skewness.Kurtosis(date.tot$OPE.Liber)[3]; OPE.de$kurt.se[6] <- Skewness.Kurtosis(date.tot$OPE.Liber)[4]
OPE.de$skew.se[7] <- Skewness.Kurtosis(date.tot$OPENNESS)[3]; OPE.de$kurt.se[7] <- Skewness.Kurtosis(date.tot$OPENNESS)[4]

OPE.de$AD.test[1] <- as.numeric(ad.test(date.tot$OPE.Imagi)$statistic); OPE.de$p[1] <- as.numeric(ad.test(date.tot$OPE.Imagi)$p.value)
OPE.de$AD.test[2] <- as.numeric(ad.test(date.tot$OPE.Artis)$statistic); OPE.de$p[2] <- as.numeric(ad.test(date.tot$OPE.Artis)$p.value)
OPE.de$AD.test[3] <- as.numeric(ad.test(date.tot$OPE.Emoti)$statistic); OPE.de$p[3] <- as.numeric(ad.test(date.tot$OPE.Emoti)$p.value)
OPE.de$AD.test[4] <- as.numeric(ad.test(date.tot$OPE.Adven)$statistic); OPE.de$p[4] <- as.numeric(ad.test(date.tot$OPE.Adven)$p.value)
OPE.de$AD.test[5] <- as.numeric(ad.test(date.tot$OPE.Intel)$statistic); OPE.de$p[5] <- as.numeric(ad.test(date.tot$OPE.Intel)$p.value)
OPE.de$AD.test[6] <- as.numeric(ad.test(date.tot$OPE.Liber)$statistic); OPE.de$p[6] <- as.numeric(ad.test(date.tot$OPE.Liber)$p.value)
OPE.de$AD.test[7] <- as.numeric(ad.test(date.tot$OPENNESS)$statistic); OPE.de$p[7] <- as.numeric(ad.test(date.tot$OPENNESS)$p.value)

rownames(OPE.de) <- c("Imagination", "Artistic interest", "Emotionality",
                      "Adventurous spirit", "Intellect", "Liberalism",
                      "Openness"); cat("\14"); OPE.de

# Extreme score analysis
# Imaginatie
boxplot(date.tot$OPE.Imagi,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$OPE.Imagi); outliers$out
OPE %>% rstatix::identify_outliers(OPE.Imagi)
# 7, 8 are outliers, not extreme
# Inlocuierea outlierilor cu NA
OPE$OPE.Imagi[which(OPE$OPE.Imagi >= 7)] <- NA
date.tot$OPE.Imagi <- OPE$OPE.Imagi
date.tot$OPE.Imagi

# Interes artistic
boxplot(date.tot$OPE.Artis,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$OPE.Artis); outliers$out
OPE %>% rstatix::identify_outliers(OPE.Artis)

# Emotionalitate
boxplot(date.tot$OPE.Emoti,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$OPE.Emoti); outliers$out
OPE %>% rstatix::identify_outliers(OPE.Emoti)
# 0 is outlier, not extreme
# Inlocuierea outlierilor cu NA
OPE$OPE.Emoti[which(OPE$OPE.Emoti == 0)] <- NA
date.tot$OPE.Emoti <- OPE$OPE.Emoti
date.tot$OPE.Emoti

# Spirit aventurier
boxplot(date.tot$OPE.Adven,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$OPE.Adven); outliers$out
OPE %>% rstatix::identify_outliers(OPE.Adven)

# Intelect
boxplot(date.tot$OPE.Intel,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$OPE.Intel); outliers$out
OPE %>% rstatix::identify_outliers(OPE.Intel)
# 8 is outlier, not extreme
# Inlocuierea outlierilor cu NA
OPE$OPE.Intel[which(OPE$OPE.Intel == 8)] <- NA
date.tot$OPE.Intel <- OPE$OPE.Intel
date.tot$OPE.Intel

# Liberalism
boxplot(date.tot$OPE.Liber,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$OPE.Liber); outliers$out
OPE %>% rstatix::identify_outliers(OPE.Liber)

# Deschidere total

boxplot(date.tot$OPENNESS,
        horizontal = T, col = "dark green", outline = T)
outliers <- boxplot.stats(date.tot$OPENNESS); outliers$out
OPE %>% rstatix::identify_outliers(OPENNESS)
# Values equal to or less than 3 are outliers & 
# values equal to 37 or more than 37 are outliers
# Replace outliers with NA
OPE$OPENNESS[which(OPE$OPENNESS >= 37 | OPE$OPENNESS <= 3)] <- NA
date.tot$OPENNESS <- OPE$OPENNES

