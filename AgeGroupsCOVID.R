# Script for extracting data from the SINOLAVE database.
# In this instanceI want data extracted by dates in addition to data previously
# obtained. 
# 15 March 2022

# update, without prompts for permission/clarification
update.packages(ask = FALSE)

ptm <- proc.time() # Code to take the starting time of the process

#required libraries, without these noting works. 

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(pastecs) # for statistics extraction

#cpA = filter(casospos, EDAD <= 19)
#cpB = filter(casospos, EDAD >= 20 & EDAD <= 39)
#cpC = filter(casospos, EDAD >= 40 & EDAD <= 59)
#cpD = filter(casospos, EDAD >= 60)

cpB_1 = filter(cpB, SEXO == 1)
write.table(cpB_1, file = "PosCas20_39_H.cvs", sep = "\t", row.names = F)

cpB_2 = filter(cpB, SEXO == 2)
write.table(cpB_2, file = "PosCas20_39_M.cvs", sep = "\t", row.names = F)

cpC_1 = filter(cpC, SEXO == 1)
write.table(cpC_1, file = "PosCas40_59_H.cvs", sep = "\t", row.names = F)

cpC_2 = filter(cpC, SEXO == 2)
write.table(cpC_2, file = "PosCas40_59_M.cvs", sep = "\t", row.names = F)

cpD_1 = filter(cpD, SEXO == 1)
write.table(cpD_1, file = "PosCas60+_H.cvs", sep = "\t", row.names = F)

cpD_2 = filter(cpD, SEXO == 2)
write.table(cpD_2, file = "PosCas60+_M.cvs", sep = "\t", row.names = F)
