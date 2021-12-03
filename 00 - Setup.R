# Install "tinytex" and "devtools"
if (!require(tinytex)) install.packages("tinytex")
tinytex::install_tinytex()
if (!require(devtools)) install.packages("devtools")
# Install Zotero AddIn "rbbt"
devtools::install_github("paleolimbot/rbbt", force = F)
library(rbbt)
# Install "papaja"
devtools::install_github("crsh/papaja@devel", force = T)

