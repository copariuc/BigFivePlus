# Incarcarea pachetelor necersare
library(psych); library(dplyr); library(purrr)

# Extraversiune - ALPHA =====
cat("\014"); EXAffect <- date %>%
  dplyr::select(I1, I5, I10, I16, I18, I25, I34, I40) %>%
  psych::alpha(title = "Friendliness consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); EXAffect
cat("\014"); ExSociab <- date %>%
  dplyr::select(I2, I6, I11, I17, I19, I26, I35, I41) %>%
  psych::alpha(title = "Gregariousness consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); ExSociab
cat("\014"); ExAssert <- date %>%
  dplyr::select(I3, I7, I12, I20, I21, I27, I36, I42) %>%
  psych::alpha(title = "Assertiveness consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); ExAssert
cat("\014"); ExActivi <- date %>%
  dplyr::select(I4, I13, I22, I28, I31, I37, I43, I44) %>%
  psych::alpha(title = "Activity level consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); ExActivi
cat("\014"); ExExcita <- date %>%
  dplyr::select(I8, I14, I23, I29, I32, I38, I45, I47) %>%
  psych::alpha(title = "Excitement seeking consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); ExExcita
cat("\014"); ExVeseli <- date %>%
  dplyr::select(I9, I15, I24, I30, I33, I39, I46, I48) %>%
  psych::alpha(title = "Cheerfulness consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); ExVeseli

## Extraversiune - OMEGA =====
cat("\014"); omegaData <- date %>%
  dplyr::select(I1, I5, I10, I16, I18, I25, I34, I40)
colnames(omegaData) <- c("I01", "I05", "I10", "I16", "I18", "I25", "I34", "I40")
sat.3 <- omega(omegaData, nfactors = 3, flip = T,  fm = "pc", poly = T,
                   title = "Friendliness", plot = T, digits = 2)
sat.2.1 <- omega(omegaData, nfactors = 2, flip = T,  fm = "pc", poly = T,
                   title = "Friendliness", plot = T, option="equal", digits = 2)
sat.2.2 <- omega(omegaData, nfactors = 2, flip = T,  fm = "pc", poly = T,
                   title = "Friendliness", plot = T, option="first", digits = 2)
sat.2.3 <- omega(omegaData, nfactors = 2, flip = T,  fm = "pc", poly = T,
                   title = "Friendliness", plot = T, option="second", digits = 2)
cat("\014"); sat.3
cat("\014"); sat.3$schmid$sl; sat.3$schmid; sat.3$schmid$gloading
cat("\014");sat.3$gstats
sat.3$stats

cat("\014"); sat.2.1
cat("\014"); sat.2.2
cat("\014"); sat.2.3
sat.3$schmid


sat.2.1$stats
sat.2.2$stats


# Agreabilitate - TOTAL =====
cat("\014"); AGIncred <- date %>%
  dplyr::select(I49, I53, I58, I64, I66, I73, I82, I88) %>%
  psych::alpha(title = "Trust consistency",
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); AGIncred
cat("\014"); AGMorali <- date %>%
  dplyr::select(I50, I54, I59, I65, I67, I74, I83, I89) %>%
  psych::alpha(title = "Morality consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); AGMorali
cat("\014"); AGAltrui <- date %>%
  dplyr::select(I51, I55, I60, I68, I69, I75, I84, I90) %>%
  psych::alpha(title = "Altruism consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); AGAltrui
cat("\014"); AGCooper <- date %>%
  dplyr::select(I52, I61, I70, I76, I79, I85, I91, I92) %>%
  psych::alpha(title = "Cooperation consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); AGCooper
cat("\014"); AGModest <- date %>%
  dplyr::select(I56, I62, I71, I77, I80, I86, I93, I95) %>%
  psych::alpha(title = "Modesty consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); AGModest
cat("\014"); AGCompas <- date %>%
  dplyr::select(I57, I63, I72, I78, I81, I87, I94, I96) %>%
  psych::alpha(title = "Sympathy consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); AGCompas
# Nevrozism - TOTAL =====
cat("\014"); NEAnxiet <- date %>%
  dplyr::select(I97, I101, I106, I112, I114, I121, I130, I136) %>%
  psych::alpha(title = "Anxiety consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); NEAnxiet
cat("\014"); NEFuriee <- date %>%
  dplyr::select(I98, I102, I107, I113, I115, I122, I131, I137) %>%
  psych::alpha(title = "Anger consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); NEFuriee
cat("\014"); NEDepres <- date %>%
  dplyr::select(I99, I103, I108, I116, I117, I123, I132, I138) %>%
  psych::alpha(title = "Depression consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); NEDepres
cat("\014"); NETimidi <- date %>%
  dplyr::select(I100, I109, I118, I124, I127, I133, I139, I140) %>%
  psych::alpha(title = "Timidity consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); NETimidi
cat("\014"); NEExager <- date %>%
  dplyr::select(I104, I110, I119, I125, I128, I134, I141, I143) %>%
  psych::alpha(title = "Immoderation consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); NEExager
cat("\014"); NEVulner <- date %>%
  dplyr::select(I105, I111, I120, I126, I129, I135, I142, I144) %>%
  psych::alpha(title = "Vulnerability consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); NEVulner

# Constiinciozitate - TOTAL =====
cat("\014"); COEficie <- date %>%
  dplyr::select(I145, I154, I160, I162, I169, I178, I184, I192) %>%
  psych::alpha(title = "Self-efficacy consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); COEficie
cat("\014"); COOrdine <- date %>%
  dplyr::select(I146, I149, I155, I161, I163, I170, I179, I185) %>%
  psych::alpha(title = "Orderliness consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); COOrdine
cat("\014"); CODatori <- date %>%
  dplyr::select(I147, I150, I156, I164, I165, I171, I180, I186) %>%
  psych::alpha(title = "Dutifulness consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); CODatori
cat("\014"); COAmbiti <- date %>%
  dplyr::select(I148, I151, I157, I166, I172, I181, I187, I188) %>%
  psych::alpha(title = "Achievement-striving consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); COAmbiti
cat("\014"); COPersev <- date %>%
  dplyr::select(I152, I158, I167, I173, I175, I182, I189, I191) %>%
  psych::alpha(title = "Self-discipline consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); COPersev
cat("\014"); COPruden <- date %>%
  dplyr::select(I153, I159, I168, I174, I176, I177, I183, I190) %>%
  psych::alpha(title = "Cautiousness consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); COPruden


# Deschidere - TOTAL =====
cat("\014"); DEImagin <- date %>%
  dplyr::select(I193, I197, I202, I208, I210, I217, I226, I232) %>%
  psych::alpha(title = "Imagination consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); DEImagin
cat("\014"); DEIntere <- date %>%
  dplyr::select(I194, I198, I203, I209, I211, I218, I227, I233) %>%
  psych::alpha(title = "Artistic interests consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); DEIntere
cat("\014"); DEEmotio <- date %>%
  dplyr::select(I195, I199, I204, I212, I213, I219, I228, I234) %>%
  psych::alpha(title = "Emotionality consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); DEEmotio
cat("\014"); DESpirit <- date %>%
  dplyr::select(I196, I205, I214, I220, I223, I229, I235, I236) %>%
  psych::alpha(title = "Adventurousness consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); DESpirit
cat("\014"); DEIntele <- date %>%
  dplyr::select(I200, I206, I215, I221, I224, I230, I237, I239) %>%
  psych::alpha(title = "Intellect consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); DEIntele
cat("\014"); DELibera <- date %>%
  dplyr::select(I201, I207, I216, I222, I225, I231, I238, I240) %>%
  psych::alpha(title = "Liberalism consistency", 
               cumulative = T, na.rm = T, check.keys = T, n.iter = 100); DELibera
