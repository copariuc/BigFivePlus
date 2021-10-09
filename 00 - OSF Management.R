# Instalarea pachetelor necesare
if(!"tinytex" %in% rownames(installed.packages())) install.packages("tinytex"); tinytex::install_tinytex()
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if(!"papaja" %in% rownames(installed.packages())) devtools::install_github("crsh/papaja")
if(!"stargazer" %in% rownames(installed.packages())) install.packages("stargazer")
if(!"foreign" %in% rownames(installed.packages())) install.packages("foreign")
if(!"epiDisplay" %in% rownames(installed.packages())) install.packages("epiDisplay")
if(!"psych" %in% rownames(installed.packages())) install.packages("psych")
if(!"nortest" %in% rownames(installed.packages())) install.packages("nortest")
if(!"dplyr" %in% rownames(installed.packages())) install.packages("dplyr") 
if(!"expss" %in% rownames(installed.packages())) install.packages("expss")
if(!"tidyverse" %in% rownames(installed.packages())) install.packages("tidyverse")
if(!"lavaan" %in% rownames(installed.packages())) install.packages("lavaan")

# Codurile sectiunilor proiectului
project_name <- 'ste6f'     # Proiectul general
data_name <- '8f23m'        # Sectiunea datelor
manuscript_name <- 'ek9b5'  # Sectiunea manuscriselor
syntax_name <- "rmec9"      # Sectiunea sintaxelor

# *************************************************************
# *** Functia de returnare a valorilor Skewness si Kurtosis ***
# *************************************************************
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
  return(rez)
}
# *************************************************************
# *************************************************************

# ****************************************************
# **** CODUL DE DOWNLOADARE A DATELOR DE PE SERVER ***
# ****************************************************
# Conectarea la sectiunea datelor si extragerea bazei de date
project <- osf_retrieve_node(data_name); files <- osf_ls_files(project)
osf_download(files, conflicts = "overwrite")
# Conectarea la manuscrise si extragerea manuscriselor
project <- osf_retrieve_node(manuscript_name); files <- osf_ls_files(project)
osf_download(files, conflicts = "overwrite")
# Conectarea la sintaxa si extragerea sintaxelor
project <- osf_retrieve_node(syntax_name); files <- osf_ls_files(project)
osf_download(files, conflicts = "overwrite")
# ***************************************************
# ***************************************************


# ************************************************
# **** CODUL DE UPLOADARE A DATELOR PE SERVER ****
# ************************************************
# Conectarea la sectiunea datelor si incarcarea fisierelor de sintaxa
project <- osf_retrieve_node(data_name)
osf_upload(project, path = "Baza de date.sav", conflicts = "overwrite")
# Conectarea la sectiunea sintaxelor si incarcarea fisierelor de sintaxa
project <- osf_retrieve_node(syntax_name)
osf_upload(project, path = "00 - OSF Management.R", conflicts = "overwrite")
osf_upload(project, path = "01 - Lotul de cercetare.R", conflicts = "overwrite")
osf_upload(project, path = "02 - Calcul scoruri brute.R", conflicts = "overwrite")
osf_upload(project, path = "03 - Consistente.R", conflicts = "overwrite")
osf_upload(project, path = "04 - CFA.R", conflicts = "overwrite")
osf_upload(project, path = "imgs/EXT.png", conflicts = "overwrite")
osf_upload(project, path = "imgs/AGR.png", conflicts = "overwrite")
# Conectarea la sectiunea manuscriselor si incarcarea manuscriselor
project <- osf_retrieve_node(manuscript_name)
osf_upload(project, path = "Articol.Rmd", conflicts = "overwrite")
osf_upload(project, path = "r-references.bib", conflicts = "overwrite")
osf_upload(project, path = "Articol.tex", conflicts = "overwrite")
# ************************************************
# ************************************************

