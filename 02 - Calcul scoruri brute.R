# Încărcarea pachetelor necesare
library(expss); library(dplyr)

# Calculul scorurilor brute
date <- date %>%
  mutate(
    # Extraversiune
    EXAffect = sum_row(I1, I5, I10, I16, I18, I25, I34, I40, na.rm = T),
    ExSociab = sum_row(I2, I6, I11, I17, I19, I26, I35, I41, na.rm = T),
    ExAssert = sum_row(I3, I7, I12, I20, I21, I27, I36, I42, na.rm = T),
    ExActivi = sum_row(I4, I13, I22, I28, I31, I37, I43, I44, na.rm = T),
    ExExcita = sum_row(I8, I14, I23, I29, I32, I38, I45, I47, na.rm = T),
    ExVeseli = sum_row(I9, I15, I24, I30, I33, I39, I46, I48, na.rm = T),
    # Agreabilitate
    AGIncred = sum_row(I49, I53, I58, I64, I66, I73, I82, I88, na.rm = T),
    AGMorali = sum_row(I50, I54, I59, I65, I67, I74, I83, I89, na.rm = T),
    AGAltrui = sum_row(I51, I55, I60, I68, I69, I75, I84, I90, na.rm = T),
    AGCooper = sum_row(I52, I61, I70, I76, I79, I85, I91, I92, na.rm = T),
    AGModest = sum_row(I56, I62, I71, I77, I80, I86, I93, I95, na.rm = T),
    AGCompas = sum_row(I57, I63, I72, I78, I81, I87, I94, I96, na.rm = T),
    # Nevrozism
    NEAnxiet = sum_row(I97, I101, I106, I112, I114, I121, I130, I136, na.rm = T),
    NEFuriee = sum_row(I98, I102, I107, I113, I115, I122, I131, I137, na.rm = T),
    NEDepres = sum_row(I99, I103, I108, I116, I117, I123, I132, I138, na.rm = T),
    NETimidi = sum_row(I100, I109, I118, I124, I127, I133, I139, I140, na.rm = T),
    NEExager = sum_row(I104, I110, I119, I125, I128, I134, I141, I143, na.rm = T),
    NEVulner = sum_row(I105, I111, I120, I126, I129, I135, I142, I144, na.rm = T),
    # Constiinciozitate
    COEficie = sum_row(I145, I154, I160, I162, I169, I178, I184, I192, na.rm = T),
    COOrdine = sum_row(I146, I149, I155, I161, I163, I170, I179, I185, na.rm = T),
    CODatori = sum_row(I147, I150, I156, I164, I165, I171, I180, I186, na.rm = T),
    COAmbiti = sum_row(I148, I151, I157, I166, I172, I181, I187, I188, na.rm = T),
    COPersev = sum_row(I152, I158, I167, I173, I175, I182, I189, I191, na.rm = T),
    COPruden = sum_row(I153, I159, I168, I174, I176, I177, I183, I190, na.rm = T),
    # Deschidere
    DEImagin = sum_row(I193, I197, I202, I208, I210, I217, I226, I232, na.rm = T),
    DEIntere = sum_row(I194, I198, I203, I209, I211, I218, I227, I233, na.rm = T),
    DEEmotio = sum_row(I195, I199, I204, I212, I213, I219, I228, I234, na.rm = T),
    DESpirit = sum_row(I196, I205, I214, I220, I223, I229, I235, I236, na.rm = T),
    DEIntele = sum_row(I200, I206, I215, I221, I224, I230, I237, I239, na.rm = T),
    DELibera = sum_row(I201, I207, I216, I222, I225, I231, I238, I240, na.rm = T),
    
    Extra = sum_row(EXAffect, ExSociab, ExAssert, ExActivi, ExExcita, ExVeseli, na.rm = T),
    Agrea = sum_row(AGIncred, AGMorali, AGAltrui, AGCooper, AGModest, AGCompas, na.rm = T),
    Nevro = sum_row(NEAnxiet, NEFuriee, NEDepres, NETimidi, NEExager, NEVulner, na.rm = T),
    Const = sum_row(COEficie, COOrdine, CODatori, COAmbiti, COPersev, COPruden, na.rm = T),
    Desch = sum_row(DEImagin, DEIntere, DEEmotio, DESpirit, DEIntele, ExVeseli, na.rm = T)
    )
  

# Aplicarea etichetelor variabilelor
date <- apply_labels(date,
                     nr_crt = "Current record",
                     filtru = "Filter variabile",
                     nume = "Examenee's name and surname",
                     psiholog = "Psycologist's name and surname",
                     online = "Type of administration",
                     varsta = "Participant's age",
                     gen = "Participant's gender",
                     studii = "Participant's level of education",
                     ocupatie = "Examenee's occupation",
                     
                     EXAffect = "Friendliness raw score",
                     ExSociab = "Gregariousness raw score",
                     ExAssert = "Assertiveness raw score",
                     ExActivi = "Activity level raw score",
                     ExExcita = "Excitement seeking raw score",
                     ExVeseli = "Cheerfulness raw score",
                     
                     AGIncred = "Trust raw score",
                     AGMorali = "Morality raw score",
                     AGAltrui = "Altruism raw score",
                     AGCooper = "Cooperation raw score",
                     AGModest = "Modesty raw score",
                     AGCompas = "Sympathy raw score",
                     
                     NEAnxiet = "Anxiety raw score",
                     NEFuriee = "Anger raw score",
                     NEDepres = "Depression raw score",
                     NETimidi = "Timidity raw score",
                     NEExager = "Immoderation raw score",
                     NEVulner = "Vulnerability raw score",
                     
                     COEficie = "Self-efficacy raw score",
                     COOrdine = "Orderliness raw score",
                     CODatori = "Dutifulness raw score",
                     COAmbiti = "Achievement-striving raw score",
                     COPersev = "Self-discipline raw score",
                     COPruden = "Cautiousness raw score",
                     
                     DEImagin = "Imagination raw score",
                     DEIntere = "Artistic interests raw score",
                     DEEmotio = "Emotionality raw score",
                     DESpirit = "Adventurousness raw score",
                     DEIntele = "Intellect raw score",
                     DELibera = "Liberalism raw score",
                     
                     Extra = "Extraversion raw score",
                     Agrea = "Agreeableness raw score",
                     Nevro = "Neuroticism raw score",
                     Const = "Conscientiousness raw score",
                     Desch = "Openness to experience raw score"
                     )
