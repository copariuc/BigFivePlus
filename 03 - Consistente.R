# Installing and loading the necessary packages
if(!require(psych)) install.packages("psych")
if(!require(dplyr)) install.packages("dplyr")

library(psych); library(dplyr)

# Loading data set
load("Date.RData")

# Building the scoring key ####
key <- list(
  # Extraversion - Score Key
  EXT.Frien = c("I1", "I5", "I10", "I16", "I18", "I25", "I34", "I40"),       # Friendliness
  EXT.Grega = c("I2", "I6", "I11", "I17", "I19", "I26", "I35", "I41"),       # Gregariousness
  EXT.Asser = c("I3", "I7", "I12", "I20", "I21", "I27", "I36", "I42"),       # Assertiveness
  EXT.Activ = c("I4", "I13", "I22", "I28", "I31", "I37", "I43", "I44"),      # Activity level
  EXT.Excit = c("I8", "I14", "I23", "I29", "I32", "I38", "I45", "I47"),      # Excitement seeking
  EXT.Cheer = c("I9", "I15", "I24", "I30", "I33", "I39", "I46", "I48"),      # Cheerfulness
  
  # Agreeableness - Score Key
  AGR.Trust = c("I49", "I53", "I58", "I64", "I66", "I73", "I82", "I88"),     # Trust
  AGR.Moral = c("I50", "I54", "I59", "I65", "I67", "I74", "I83", "I89"),     # Morality
  AGR.Altru = c("I51", "I55", "I60", "I68", "I69", "I75", "I84", "I90"),     # Altruism
  AGR.Coope = c("I52", "I61", "I70", "I76", "I79", "I85", "I91", "I92"),     # Cooperation
  AGR.Modes = c("I56", "I62", "I71", "I77", "I80", "I86", "I93", "I95"),     # Modesty
  AGR.Sympa = c("I57", "I63", "I72", "I78", "I81", "I87", "I94", "I96"),     # Sympathy
  
  # Neuroticism - Score Key
  NEU.Anxie = c("I97", "I101", "I106", "I112", "I114", "I121", "I130", "I136"),     # Anxiety
  NEU.Anger = c("I98", "I102", "I107", "I113", "I115", "I122", "I131", "I137"),     # Anger
  NEU.Depre = c("I99", "I103", "I108", "I116", "I117", "I123", "I132", "I138"),     # Depression
  NEU.Timid = c("I100", "I109", "I118", "I124", "I127", "I133", "I139", "I140"),    # Timidity
  NEU.Immod = c("I104", "I110", "I119", "I125", "I128", "I134", "I141", "I143"),    # Immoderation
  NEU.Vulne = c("I105", "I111", "I120", "I126", "I129", "I135", "I142", "I144"),    # Vulnerability
  
  # Conscientiousness - Score Key
  CON.SelfE = c("I145", "I154", "I160", "I162", "I169", "I178", "I184", "I192"),    # Self-efficacy
  CON.Order = c("I146", "I149", "I155", "I161", "I163", "I170", "I179", "I185"),    # Orderliness
  CON.Dutif = c("I147", "I150", "I156", "I164", "I165", "I171", "I180", "I186"),    # Dutifulness
  CON.Achie = c("I148", "I151", "I157", "I166", "I172", "I181", "I187", "I188"),    # Achievement-striving
  CON.SelfD = c("I152", "I158", "I167", "I173", "I175", "I182", "I189", "I191"),    # Self-discipline
  CON.Cauti = c("I153", "I159", "I168", "I174", "I176", "I177", "I183", "I190"),    # Cautiousness
  
  # Openness to experience - Score Key
  OPE.Imagi = c("I193", "I197", "I202", "I208", "I210", "I217", "I226", "I232"),    # Imagination
  OPE.Artis = c("I194", "I198", "I203", "I209", "I211", "I218", "I227", "I233"),    # Artistic interests
  OPE.Emoti = c("I195", "I199", "I204", "I212", "I213", "I219", "I228", "I234"),    # Emotionality
  OPE.Adven = c("I196", "I205", "I214", "I220", "I223", "I229", "I235", "I236"),    # Adventurousness
  OPE.Intel = c("I200", "I206", "I215", "I221", "I224", "I230", "I237", "I239"),    # Intellect
  OPE.Liber = c("I201", "I207", "I216", "I222", "I225", "I231", "I238", "I240")     # Liberalism
)

# Upgrading scoring key with dimensions
key <- c(key, list(
  EXTRAVER = c(key$EXT.Frien, key$EXT.Grega, key$EXT.Asser, 
                key$EXT.Activ, key$EXT.Excit, key$EXT.Cheer),      # Extraversion
  AGREEABL = c(key$AGR.Trust, key$AGR.Moral, key$AGR.Altru, 
                key$AGR.Coope, key$AGR.Modes, key$AGR.Sympa),      # Agreeableness
  NEUROTIC = c(key$NEU.Anxie, key$NEU.Anger, key$NEU.Depre, 
                key$NEU.Timid, key$NEU.Immod, key$NEU.Vulne),      # Neuroticism
  CONSCIEN = c(key$CON.SelfE, key$CON.Order, key$CON.Dutif, 
                key$CON.Achie, key$CON.SelfD, key$CON.Cauti),      # Conscientiousness
  OPENNESS = c(key$OPE.Imagi, key$OPE.Artis, key$OPE.Emoti, 
                key$OPE.Adven, key$OPE.Intel, key$OPE.Liber)       # Openness
))

# Compute total score on new data set and returning correlation matrices and response frequencies ####
scores <- scoreItems(key, date, totals = TRUE, impute = "median")
cor.mat <- round(scores$cor, 2); cor.mat.correct <- round(scores$corrected, 3); 
resp.freq <- round(scores$response.freq, 3)
date.tot <- as.data.frame(scores$scores); save(date.tot, file = "Date.total.Rdata")
n <- nrow(date)

# Extraversion - Computing internal consistency ####
# EXT.Frien <- psych::alpha(dplyr::select(date, key$EXT.Frien), title = "Friendliness - Facet", 
#                cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CI.EXT.Frien <- alpha.ci(EXT.Frien$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
# EXT.Grega <- psych::alpha(dplyr::select(date, key$EXT.Grega), title = "Gregariousness - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CI.EXT.Grega <- alpha.ci(EXT.Grega$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
# EXT.Asser <- psych::alpha(dplyr::select(date, key$EXT.Asser), title = "Assertiveness - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CI.EXT.Asser <- alpha.ci(EXT.Asser$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
# EXT.Activ <- psych::alpha(dplyr::select(date, key$EXT.Activ), title = "Activity level - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CI.EXT.Activ <- alpha.ci(EXT.Activ$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
# EXT.Excit <- psych::alpha(dplyr::select(date, key$EXT.Excit), title = "Excitement seeking - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CI.EXT.Excit <- alpha.ci(EXT.Excit$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
# EXT.Cheer <- psych::alpha(dplyr::select(date, key$EXT.Cheer), title = "Cheerfulness - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CI.EXT.Cheer <- alpha.ci(EXT.Cheer$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
# EXTRAVER <- psych::alpha(dplyr::select(date, key$EXTRAVER), title = "Extraversion - Dimension", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CI.EXTRAVER <- alpha.ci(EXTRAVER$total$raw_alpha, n.obs = n, n.var = 48, p.val = .05, digits = 3)

# Agreeableness - Computing internal consistency ####
AGR.Trust <- psych::alpha(dplyr::select(date, key$AGR.Trust), title = "Trust - Facet",
                          cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
CI.AGR.Trust <- alpha.ci(AGR.Trust$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
AGR.Moral <- psych::alpha(dplyr::select(date, key$AGR.Moral), title = "Morality - Facet",
                          cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
CI.AGR.Moral <- alpha.ci(AGR.Moral$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
AGR.Altru <- psych::alpha(dplyr::select(date, key$AGR.Altru), title = "Altruism - Facet",
                          cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
CI.AGR.Altru <- alpha.ci(AGR.Altru$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
AGR.Coope <- psych::alpha(dplyr::select(date, key$AGR.Coope), title = "Cooperation - Facet",
                          cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
CI.AGR.Coope <- alpha.ci(AGR.Coope$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
AGR.Modes <- psych::alpha(dplyr::select(date, key$AGR.Modes), title = "Modesty - Facet",
                          cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
CI.AGR.Modes <- alpha.ci(AGR.Modes$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
AGR.Sympa <- psych::alpha(dplyr::select(date, key$AGR.Sympa), title = "Sympathy - Facet",
                          cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
CI.AGR.Sympa <- alpha.ci(AGR.Sympa$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)
AGREEABL <- psych::alpha(dplyr::select(date, key$AGREEABL), title = "Agreeableness - Dimension",
                         cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
CI.AGREEABL <- alpha.ci(AGREEABL$total$raw_alpha, n.obs = n, n.var = 8, p.val = .05, digits = 3)

# # Neuroticism - Computing internal consistency ####
# NEU.Anxie <- psych::alpha(dplyr::select(date, key$NEU.Anxie), title = "Anxiety - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# NEU.Anger <- psych::alpha(dplyr::select(date, key$NEU.Anger), title = "Anger - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# NEU.Depre <- psych::alpha(dplyr::select(date, key$NEU.Depre), title = "Depression - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# NEU.Timid <- psych::alpha(dplyr::select(date, key$NEU.Timid), title = "Timidity - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# NEU.Immod <- psych::alpha(dplyr::select(date, key$NEU.Immod), title = "Immoderation - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# NEU.Vulne <- psych::alpha(dplyr::select(date, key$NEU.Vulne), title = "Vulnerability - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# NEUROTIC <- psych::alpha(dplyr::select(date, key$NEUROTIC), title = "Neuroticism - Dimension", 
#                          cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# 
# # Conscientiousness - Computing internal consistency ####
# CON.SelfE <- psych::alpha(dplyr::select(date, key$CON.SelfE), title = "Self-efficacy - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CON.Order <- psych::alpha(dplyr::select(date, key$CON.Order), title = "Orderliness - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CON.Dutif <- psych::alpha(dplyr::select(date, key$CON.Dutif), title = "Dutifulness - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CON.Achie <- psych::alpha(dplyr::select(date, key$CON.Achie), title = "Achievement-striving - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CON.SelfD <- psych::alpha(dplyr::select(date, key$CON.SelfD), title = "Self-discipline - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# NEU.Vulne <- psych::alpha(dplyr::select(date, key$NEU.Vulne), title = "Cautiousness - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# CON.Cauti <- psych::alpha(dplyr::select(date, key$CON.Cauti), title = "Conscientiousness - Dimension", 
#                          cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# 
# # Openness to experience - Computing internal consistency ####
# OPE.Imagi <- psych::alpha(dplyr::select(date, key$OPE.Imagi), title = "Imagination - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# OPE.Artis <- psych::alpha(dplyr::select(date, key$OPE.Artis), title = "Artistic interests - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# OPE.Emoti <- psych::alpha(dplyr::select(date, key$OPE.Emoti), title = "Emotionality - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# OPE.Adven <- psych::alpha(dplyr::select(date, key$OPE.Adven), title = "Adventurousness - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# OPE.Intel <- psych::alpha(dplyr::select(date, key$OPE.Intel), title = "Intellect - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# OPE.Liber <- psych::alpha(dplyr::select(date, key$OPE.Liber), title = "Liberalism - Facet", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
# OPENNESS <- psych::alpha(dplyr::select(date, key$OPENNESS), title = "Openness to experience - Dimension", 
#                           cumulative = T, na.rm = T, check.keys = T, n.iter = 100)
