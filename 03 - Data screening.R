# Instalarea si incarcarea librariilor necesare
if(!require(expss)) install.packages("expss")
library(expss)

load("Date.total.Rdata"); load("Date.Rdata")
date.tot <- data.frame(date[,c(1, 3, 5:7)], date.tot)

# Aplicarea etichetelor variabilelor
date.tot <- apply_labels(date.tot,
                     nr_crt = "Current record",
                     nume = "Examenee's name and surname",
                     varsta = "Participant's age",
                     gen = "Participant's gender",
                     studii = "Participant's level of education",

                     EXT.Frien = "Friendliness raw score",
                     EXT.Grega = "Gregariousness raw score",
                     EXT.Asser = "Assertiveness raw score",
                     EXT.Activ = "Activity level raw score",
                     EXT.Excit = "Excitement seeking raw score",
                     EXT.Cheer = "Cheerfulness raw score",
                     
                     AGR.Trust = "Trust raw score",
                     AGR.Moral = "Morality raw score",
                     AGR.Altru = "Altruism raw score",
                     AGR.Coope = "Cooperation raw score",
                     AGR.Modes = "Modesty raw score",
                     AGR.Sympa = "Sympathy raw score",
                     
                     NEU.Anxie = "Anxiety raw score",
                     NEU.Anger = "Anger raw score",
                     NEU.Depre = "Depression raw score",
                     NEU.Timid = "Timidity raw score",
                     NEU.Immod = "Immoderation raw score",
                     NEU.Vulne = "Vulnerability raw score",
                     
                     CON.SelfE = "Self-efficacy raw score",
                     CON.Order = "Orderliness raw score",
                     CON.Dutif = "Dutifulness raw score",
                     CON.Achie = "Achievement-striving raw score",
                     CON.SelfD = "Self-discipline raw score",
                     CON.Cauti = "Cautiousness raw score",
                     
                     OPE.Imagi = "Imagination raw score",
                     OPE.Artis = "Artistic interests raw score",
                     OPE.Emoti = "Emotionality raw score",
                     OPE.Adven = "Adventurousness raw score",
                     OPE.Intel = "Intellect raw score",
                     OPE.Liber = "Liberalism raw score",
                     
                     EXTRAVER = "Extraversion raw score",
                     AGREEABL = "Agreeableness raw score",
                     NEUROTIC = "Neuroticism raw score",
                     CONSCIEN = "Conscientiousness raw score",
                     OPENNESS = "Openness to experience raw score")
save(date.tot, file = "Date.total.Rdata"); rm(date)
